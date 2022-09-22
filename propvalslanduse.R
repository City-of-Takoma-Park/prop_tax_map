## purpose: generate visualizations of property tax data
## last run: april 2022

library(tidyverse)
library(sf)
library(tpfuncts)
library(plotly)
library(glue)

tp_props <- read_rds("./data/tp_props_taxes.rds")

lus_own <- tp_props %>%
  group_by(land.use.desc, owner.occ.desc) %>%
  mutate(land.use.desc = gsub(" ", "\n", land.use.desc) %>%
           gsub("\\/", "\n& ", .),
         owner.occ.desc = gsub(" O", "\nO", owner.occ.desc)) %>%
  summarize(
    numprop = n(),
    percentprop = round(numprop * 100 / nrow(.), 1),
    medianvalue = median(current.value),
    medianchange = median(chng_value),
    medianpctchange = median(pct_chng_val),
    avgvalue = mean(current.value),
    avgchange = mean(chng_value),
    avgpctchange = mean(pct_chng_val),
    maxvalue = max(current.value),
    maxchange = max(chng_value),
    maxpctchange = max(pct_chng_val),
    mediantaxable = median(taxable.value),
    avgtaxable = mean(taxable.value),
    maxtaxable = max(taxable.value)
  )

lus <- tp_props %>%
  group_by(land.use.desc) %>%
  mutate(land.use.desc = gsub(" ", "\n", land.use.desc) %>%
           gsub("\\/", "\n& ", .)) %>%
  summarize(
    numprop = n(),
    percentprop = round(numprop * 100 / nrow(.)),
    medianvalue = median(current.value),
    medianchange = median(chng_value),
    medianpctchange = median(pct_chng_val),
    avgvalue = mean(current.value),
    avgchange = mean(chng_value),
    avgpctchange = mean(pct_chng_val),
    maxvalue = max(current.value),
    maxchange = max(chng_value),
    maxpctchange = max(pct_chng_val),
    mediantaxable = median(taxable.value),
    avgtaxable = mean(taxable.value),
    maxtaxable = max(taxable.value)
  )


plot_ly(lus_own,
        x = ~ owner.occ.desc, 
        color = ~ land.use.desc,
        y = ~ numprop,
        text = ~ glue("{percentprop}%"),
        textposition = "inside",
        textfont = list(color = "black"),
        type = "bar") %>%
  layout(title = "Total properties by type",
         xaxis = list(title = "Owner-occupied"),
         yaxis = list(title = "Total-properties",
                      tickvals = seq(0, 3250, 250)))

plot_ly(lus,
        x = ~ land.use.desc, 
        color = ~ land.use.desc,
        showlegend = F,
        y = ~ numprop,
        text = ~ glue("{percentprop}%"),
        textposition = "inside",
        textfont = list(color = "black"),
        type = "bar") %>%
  layout(title = "Total properties by type",
         xaxis = list(title = "Land-use"),
         yaxis = list(title = "Total-properties",
                      tickvals = seq(0, 3800, 250)))


plot_ly(lus %>%
          select(land.use.desc, mediantaxable, avgtaxable) %>%
          pivot_longer(cols = c("mediantaxable", "avgtaxable")) %>%
          mutate(name = ifelse(name == "mediantaxable", "Median", "Average")),
        x = ~ land.use.desc, 
        color = ~ name,
        showlegend = T,
        y = ~ value,
        # text = ~ glue("{percentprop}%"),
        textposition = "inside",
        textfont = list(color = "black"),
        type = "bar") %>%
  layout(title = "Taxable value by land use",
         xaxis = list(title = "Land use"),
         yaxis = list(title = "Taxable value"))

plot_ly(lus %>%
          select(land.use.desc, medianpctchange, avgpctchange) %>%
          pivot_longer(cols = c("medianpctchange", "avgpctchange")) %>%
          mutate(name = ifelse(name == "medianpctchange", "Median", "Average")),
        x = ~ land.use.desc, 
        color = ~ name,
        showlegend = T,
        y = ~ value,
        # text = ~ glue("{percentprop}%"),
        textposition = "inside",
        textfont = list(color = "black"),
        type = "bar") %>%
  layout(title = "Percent change in value by land use",
         xaxis = list(title = "Land use"),
         yaxis = list(title = "Percent change in value",
                      tickvals= seq(0, 20, 2.5)))

plot_ly(lus %>%
          select(land.use.desc, medianchange, avgchange) %>%
          pivot_longer(cols = c("medianchange", "avgchange")) %>%
          mutate(name = ifelse(name == "medianchange", "Median", "Average")),
        x = ~ land.use.desc, 
        color = ~ name,
        showlegend = T,
        y = ~ value,
        # text = ~ glue("{percentprop}%"),
        textposition = "inside",
        textfont = list(color = "black"),
        type = "bar") %>%
  layout(title = "Change in value by land use",
         xaxis = list(title = "Land use"),
         yaxis = list(title = "Change in value",
                      tickvals = seq(0, 200000, 25000)))
