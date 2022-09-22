## purpose: generate visualizations of property tax data
## last run: april 2022

library(tidyverse)
library(sf)
library(tpfuncts)
library(plotly)
library(flextable)

############################################################################
######################### calculate distribution of property values #####
############################################################################

tp_props_taxes <- readRDS("./data/tp_props_taxes.rds")

tp_props_taxes %>%
  filter(taxable.value == 0) %>%
  View()

quant_val <- quantile(tp_props_taxes$current.value, c(seq(0, 0.8, 0.1), seq(0.9, 1, 0.05)))
quant_amount <- quantile(tp_props_taxes$chng_value, c(seq(0, 0.8, 0.1), seq(0.9, 1, 0.05)))
quant_pct <- quantile(tp_props_taxes$pct_chng_val, c(seq(0, 0.8, 0.1), seq(0.9, 1, 0.05)))

df_quants <- data.frame(Percentile = as.factor(names(quant_amount)), 
                        'Current Value (2019)' = quant_val, 
                        'Change in Value' = quant_amount,
                        'Percent Changes' = quant_pct, check.names = F)



tbl_chngs <- flextable::flextable(df_quants) %>%
  flextable::italic(i = 1, part = "header") %>%
  flextable::colformat_double(j = 2:3, prefix = "$", digits = 0) %>%
  flextable::colformat_double(j = 4, suffix = "%", digits = 1) %>%
  flextable::bold(i = 1, part = "header") %>%
  flextable::bg(i = 1, part = "header", bg = "#e6eaff") %>%
  flextable::autofit()

print(tbl_chngs, preview = "docx")

df_quants_plot <- df_quants %>%
  rename_all(.funs = ~ gsub(" ", "_", .x) %>%
               gsub("(\\()|(\\))", "", .) %>%
               tolower()) %>%
  mutate(percentile = factor(percentile, .$percentile, .$percentile))

sub_vals <- plotly::plot_ly(df_quants_plot %>%
                              slice(1:(nrow(.)- 1)), 
                            x = ~ percentile,
                            
                            showlegend= F,
                            y = ~ current_value_2019,
                            type = "bar") %>%
  layout(xaxis = list(title = "Percentile"),
         yaxis = list(title = "Current value"),
         title = "") %>%
  subplot_title("Current value")

sub_chng <- plotly::plot_ly(df_quants_plot %>%
                              slice(1:(nrow(.)- 1)), 
                            x = ~ percentile,
                            y = ~ change_in_value,
                            
                            showlegend= F,
                            type = "scatter",
                            mode = "line+marker") %>%
  layout(xaxis = list(title = "Percentile"),
         yaxis = list(title = "Change in value"),
         title = "") %>%
  subplot_title("Change in value")


sub_pctchng <- plotly::plot_ly(df_quants_plot %>%
                                 slice(1:(nrow(.)- 1)), 
                               x = ~ percentile,
                               y = ~ percent_changes,
                               showlegend= F,
                               type = "scatter",
                               mode = "line+marker") %>%
  layout(xaxis = list(title = "Percentile"),
         yaxis = list(title = "Change in value"),
         title = "") %>%
  subplot_title("Percent change in value")

subplot(sub_chng, sub_pctchng, sub_vals, nrows = 2, shareX = T) %>%
  layout(title = "Takoma Park property tax assessments 2019-2022")

# write data on taxable value
tp_props_taxes$taxpaid %>% sum() %>% commafy
tp_props_taxes$taxpaidinc %>% sum() %>% commafy

tp_props_taxes$taxable.value[tp_props_taxes$taxable.value > 5000000]

plotall <- plot_ly(x = ~ tp_props_taxes$taxable.value,
                   type = "histogram") %>%
  layout(xaxis = list(title = "Taxable value",
                      tickvals = seq(0, 35000000, 1000000)),
         yaxis = list(title = "Number of properties"),
         title = "Total properties by taxable value")

plotless5 <- plot_ly(x = ~ tp_props_taxes$taxable.value[tp_props_taxes$taxable.value <= 5000000],
                     type = "histogram") %>%
  layout(xaxis = list(title = "Taxable value",
                      tickvals = seq(0, 5000000, 250000)),
         yaxis = list(title = "Number of properties"),
         title = "Properties by taxable value (<= $5,000,000)")

# check number chaneg in value more than 1,000,000
tp_props_taxes$chng_value[tp_props_taxes$chng_value > 1000000] %>% length()

plotall_chng <- plot_ly(x = ~ tp_props_taxes$chng_value,
                        type = "histogram") %>%
  layout(xaxis = list(title = "Change in value",
                      tickvals = seq(-250000, 7000000, 250000)),
         yaxis = list(title = "Number of properties"),
         title = "Total properties by change in value")

plotall_chng_less1mil <- plot_ly(x = ~ tp_props_taxes$chng_value[tp_props_taxes$chng_value <= 1000000],
                                 type = "histogram") %>%
  layout(xaxis = list(title = "Change in value",
                      tickvals = seq(-250000, 1000000, 50000)),
         yaxis = list(title = "Number of properties"),
         title = "Total properties by change in value (<= $1,000,000)")

# check percent change value

tp_props_taxes$pct_chng_val[tp_props_taxes$pct_chng_val > 100] %>% length()

plotall_pctchng <- plot_ly(x = ~ tp_props_taxes$pct_chng_val,
                           type = "histogram") %>%
  layout(xaxis = list(title = "Percent change in value",
                      tickvals = seq(-50, 750, 50)),
         yaxis = list(title = "Number of properties"),
         title = "Total properties by percent change in value")

plotall_pctchng_less100 <- plot_ly(x = ~ tp_props_taxes$pct_chng_val[tp_props_taxes$pct_chng_val <= 100],
                                   type = "histogram") %>%
  layout(xaxis = list(title = "Percent change in value",
                      tickvals = seq(-50, 100, 10)),
         yaxis = list(title = "Number of properties"),
         title = "Total properties by percent change in value (<= 100%)")

# total
sum(tp_props_taxes$chng_value == 0) / length(tp_props_taxes$chng_value)

# how many properties more than 5 mil
tp_props_taxes$taxable.value[tp_props_taxes$taxable.value > 5000000] %>% sort %>% commafy

# property buckets
calc_transform <- function(df){
  returns <- c(
    "No taxable value",
    "Decrease in value",
    "No change in value",
    "0-1%",
    "1-5%",
    "5-10%",
    "10-15%",
    "15-20%",
    "20%+"
  )
  
  calc <- function(val, taxval){
    case_when(
      taxval == 0 ~ returns[1],
      val < 0 ~ returns[2],
      val == 0 ~ returns[3],
      val <= 1 ~ returns[4],
      val <= 5 ~ returns[5],
      val <= 10 ~ returns[6],
      val <= 15 ~ returns[7],
      val <= 20 ~ returns[8],
      val > 20 ~ returns[9])
  }
  
  df %>%
    mutate(propcat = calc(val = pct_chng_val, taxval = taxable.value),
           propcat = factor(propcat, returns, returns))
}

tp_props_recat <- tp_props_taxes %>%
  calc_transform() %>%
  select(pct_chng_val, propcat) %>%
  group_by() %>%
  mutate(allprops = n(),
         medval = round(median(pct_chng_val), 1)) %>%
  arrange(propcat)

tp_totals <- tp_props_recat[1, ] %>%
  select(propcat, medval, allprops) %>%
  mutate(propcat = "Total properties and median percent-value change") %>%
  rename("Percent properties" = medval,
         "Total properties" = allprops)

tp_props_final <- tp_props_recat %>%
  ungroup %>%
  group_by(propcat) %>%
  summarize("Total properties" = n(),
            "Percent properties" = round(`Total properties` / allprops * 100, 1)) %>%
  distinct() %>%
  rbind(tp_totals) %>%
  ungroup() %>%
  mutate("Cumulative percent" = cumsum(`Percent properties`),
         `Cumulative percent` = case_when(grepl("Total", propcat) ~ 100,
                                          T ~ `Cumulative percent`)) %>%
  rename("Change in value"= propcat)

flex_props <- flextable::flextable(tp_props_final) %>%
  flextable::bold(part = "body", i = 10) %>%
  flextable::bold(part = "header", i = 1) %>%
  flextable::italic(part = "header") %>%
  flextable::colformat_double(j = 3:4, suffix = "%") %>%
  flextable::bg(bg = "#fcfcbd", i = 1, part = "header") %>%
  flextable::autofit()
    
  
