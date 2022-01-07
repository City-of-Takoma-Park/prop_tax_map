library(leaflet)
library(tidyverse)
library(leafletwrappers)
library(sf)
library(tpfuncts)



land_recode <- openxlsx::read.xlsx("./data/county_code_expl.xlsx", sheet = "Code Explanations") %>%
  rename_all(tolower)

owner_recode <- openxlsx::read.xlsx("./data/county_code_expl.xlsx", sheet = "Sheet1") %>%
  rename_all(tolower)

tp_props <- openxlsx::read.xlsx("./data/Takoma Park Tax Roll 01-01-2022.xlsx") %>%
  rename_all(tolower) %>%
  mutate(acct = substr(account, 5, length(account)),
         length = str_length(acct),
         chng_value = current.value - base.value,
         pct_chng_val = round((chng_value / base.value) * 100, 2),
         address_full = paste0(number, " ", stringr::str_to_title(street))) %>%
  left_join(owner_recode) %>%
  rename(owner.occ.desc = description) %>%
  left_join(land_recode) %>%
  rename(land.use.desc = description)

tp_props$land.use %>% unique()

land_use_recode <- function(string){
  
}

tp_props$length %>%
  unique

mc_prop_shp <- st_read("./data/Property/property.gdb") %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

tp_prop_shp <- mc_prop_shp %>%
  rename_all(tolower) %>%
  right_join(tp_props, 
            by = "acct")

tp_prop_missing <- tp_props %>%
  anti_join((mc_prop_shp %>%
               rename_all(tolower)), 
             by = "acct")



pal_tp_propvals <- leafletwrappers::pal_numeric(colors = "Blues", var = "current.value", df = st_drop_geometry(tp_prop_shp), reverse = F)

quantile(st_drop_geometry(tp_prop_shp)[["current.value"]], probs = seq(0, 1, .05))

quantile(st_drop_geometry(tp_prop_shp)[["pct_chng_val"]], probs = seq(0, 1, .05))

# tp_prop_bins <- c(100, 175000, 310000, 475000, 525000, 575000, 625000, 700000, 800000, 1000000, 51000000)

tp_prop_bins <- c(100, 175000, 350000, 500000, 60000, 700000, 800000, 1000000, 51000000)


tp_chng_bins <- c(-30, -5, 0, 5, 10, 20, 25, 30, 40, 50, 100, 750)

pal_tp_propvals <- leaflet::colorBin("Blues", domain = c(100, 51000000),
                                    bins = tp_prop_bins)

colors <- RColorBrewer::brewer.pal(11, "PiYG")
colors <- RColorBrewer::brewer.pal(11, "Greens")[-1]


chng_colors <- c("#8E0152", "#F1B6DA", "#F7F7F7", colors)

pal_tp_chngval <- leaflet::colorBin(palette = chng_colors, domain = c(-30, 750),
                                    bins = tp_chng_bins)

# pal_tp_chngval <- pal_numeric(colors = "PuOr", var = "pct_chng_val", df = st_drop_geometry(tp_prop_shp))

labs_poly <- leafletwrappers::label_standardize(st_drop_geometry(tp_prop_shp), 
                                           label_text = 
                                           "Address: {address_full}<p></p>
                                           Date of construction: {year_built}<p></p>
                                           Current value (2022): ${current.value %>% commafy}<p></p>
                                           Last assessment value (2019): ${base.value %>% commafy}<p></p>
                                           Change in value: ${chng_value %>% commafy}<p></p>
                                           Percent change in value: {pct_chng_val}%<p></p>
                                           Current value of land: ${current.land.value %>% commafy}<p></p>
                                           Current value of improvements: ${current.imp.value %>% commafy}<p></p>
                                           Taxable value: ${taxable.value %>% commafy}<p></p>
                                           Land use category: {land.use.desc}<p></p>
                                           Owner occupanyc: {owner.occ.desc}")

add_prop_vals <- function(basemap,
                          var,
                          pal_funct,
                          grp = "Current property values",
                          labs = labs_poly,
                          df = tp_prop_shp) {
  basemap %>%
    leaflet::addPolygons(
      color = "Grey", 
      weight = 1,
      fill = T,
      opacity = 1, 
      fillOpacity = 0.7, 
      fillColor = ~ pal_funct(as.data.frame(df)[[var]]), 
      group = grp,
      highlight = leaflet::highlightOptions(
        stroke = TRUE,
        weight = 3.5, 
        fillOpacity = 0.6, 
        color = "#555EE7",
        opacity = 1, 
        bringToFront = TRUE), 
      label = purrr::map(labs, htmltools::HTML), 
      labelOptions = leaflet::labelOptions(
        style = list(`font-weight` = "normal", 
                     padding = "0.2px 0.2px", 
                     `line-height` = 0.8), 
        textsize = "10px", direction = "auto", 
        opacity = 0.8), 
      data = df) %>%
    addLegend(position = "topright", 
              pal = pal_funct, 
              values = ~ as.data.frame(df)[[var]], 
              group = grp, 
              opacity = 0.7, 
              title= grp)
}


# addlegend_standard()
carto_map <- leaflet(tp_prop_shp) %>%
  addProviderTiles(providers$CartoDB) %>%
  # addTiles(options = tileOptions(opacity = 0.5)) %>%
  add_prop_vals(pal_funct = pal_tp_propvals, var = "current.value") %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val", grp = "Percent change in property values") %>%
  leaflet::addLayersControl(overlayGroups = c("Current property values",
                                              "Percent change in property values"), 
                            position = "topleft", 
                            options = layersControlOptions(collapsed = F)) %>%
  hideGroup("Percent change in property values")

reg_map <- leaflet(tp_prop_shp) %>%
  # addProviderTiles(providers$CartoDB) %>%
  addTiles(options = tileOptions(opacity = 0.5)) %>%
  add_prop_vals(pal_funct = pal_tp_propvals, var = "current.value") %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val", grp = "Percent change in property values") %>%
  leaflet::addLayersControl(overlayGroups = c("Current property values",
                                              "Percent change in property values"), 
                            position = "topleft", 
                            options = layersControlOptions(collapsed = F)) %>%
  hideGroup("Percent change in property values")

htmlwidgets::saveWidget(reg_map, "./data/reg_map.html", selfcontained = T)
htmlwidgets::saveWidget(carto_map, "./data/carto_map.html", selfcontained = T)


# 
# 
# 
# props_map <- leaflet(tp_prop_shp) %>%
#   addTiles(options = tileOptions(opacity = 0.5)) %>%
#   leafletwrappers::add_city() %>%
#   leafletwrappers::addpoly_legend(
#     df_select = st_drop_geometry(tp_prop_shp), 
#     pal_funct_select = pal_tp_propvals, 
#     variable_select = "current.value",  
#     group_select = "Current property values",
#     title_select = "Current property values",
#     labels_select = labs_poly) %>%
#   leafletwrappers::addpoly_legend(
#     df_select = st_drop_geometry(tp_prop_shp), 
#     pal_funct_select = pal_tp_chngval, 
#     variable_select = "pct_chng_val",
#     group_select = "Change in property values",
#     title_select = "Change in property values",
#     labels_select = labs_poly) %>%
#   leaflet::addLayersControl(overlayGroups = c("Current property values",
#                                               "Change in property values"), 
#                             position = "topleft", 
#                             options = layersControlOptions(collapsed = F)) %>%
#   hideGroup("Change in property values")
# 
# 
# 
# leaflet(tp_prop_shp) %>% 
# 

#### work with chris property data
county_data <- st_layers("./data/county_tax_data/TakomaPark/db.gdb")

ppwd_2016 <- st_read("./data/county_tax_data/TakomaPark/db.gdb", layer = "PPWD_2016")

condo_poly <- st_read("./data/county_tax_data/TakomaPark/db.gdb", layer = "CondoPoints_to_Poly")

ppwd_2016_csv <- read.csv("./data/county_tax_data/TakomaPark/ppwd2016.txt") %>%
  rename_all(tolower) %>%
  select(acct, land_assmt, pref_assmt, improv_assmt, property_code, premise_addr_street, premise_addr_city) %>%
  rename_all(.funs = ~ paste0(.x, "_16"))

tp_prop_shp_16merge <- tp_prop_shp %>%
  left_join(ppwd_2016_csv, by = c("acct"= "acct_16")) %>%
  mutate(
    val_16 = land_assmt_16 + improv_assmt_16,
    chng_val_1622 = current.value - val_16,
    chng_val_1619 = base.value - val_16,
    pct_chng_val_1622 = pct_round(chng_val_1622, val_16),
    pct_chng_val_1619 = pct_round(chng_val_1619, val_16))

tp_prop_shp_16merge_unjoin <- tp_prop_shp %>%
  anti_join(ppwd_2016_csv, by = c("acct"= "acct_16"))

labs_poly_16 <- leafletwrappers::label_standardize(st_drop_geometry(tp_prop_shp_16merge), 
                                                label_text = 
                                                  "Address: {address_full}<p></p>
                                           Date of construction: {year_built}<p></p>
                                           Current value (2022): ${current.value %>% commafy}<p></p>
                                           Last assessment value (2019): ${base.value %>% commafy}<p></p>
                                           Change in value since 2019: ${chng_value %>% commafy}<p></p>
                                           Percent change in value since 2019: {pct_chng_val}%<p></p>
                                           2016 assessment value: ${val_16 %>% commafy}<p></p>
                                           Percent change in value since 2016: {pct_chng_val_1622}%<p></p>
                                           Percent change in value between 2016 and 2019: {pct_chng_val_1619}%<p></p>

                                           Current value of land: ${current.land.value %>% commafy}<p></p>
                                           Current value of improvements: ${current.imp.value %>% commafy}<p></p>
                                           Taxable value: ${taxable.value %>% commafy}<p></p>
                                           Land use category: {land.use.desc}<p></p>
                                           Owner occupanyc: {owner.occ.desc}")

quantile(st_drop_geometry(tp_prop_shp_16merge)[["pct_chng_val_1622"]], probs = seq(0, 1, .05), na.rm = T)


tp_chng_bins <- c(-100, -30, -5, 0, 5, 10, 20, 25, 30, 40, 50, 100, 750)
chng_colors <- c("#8E0152", "#C51B7D", "#F1B6DA", "#F7F7F7", colors)

pal_tp_chngval <- leaflet::colorBin(palette = chng_colors, domain = c(-100, 750),
                                    bins = tp_chng_bins)

carto_map_16 <- leaflet(tp_prop_shp_16merge) %>%
  addProviderTiles(providers$CartoDB) %>%
  # addTiles(options = tileOptions(opacity = 0.5)) %>%
  add_prop_vals(pal_funct = pal_tp_propvals, var = "current.value", labs = labs_poly_16, df = tp_prop_shp_16merge) %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val", grp = "Percent change in property values since 2019", labs = labs_poly_16, df = tp_prop_shp_16merge) %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val_1622", grp = "Percent change in property values since 2016", labs = labs_poly_16, df = tp_prop_shp_16merge) %>%
  leaflet::addLayersControl(overlayGroups = c("Current property values",
                                              "Percent change in property values since 2019", "Percent change in property values since 2016"), 
                            position = "topleft", 
                            options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("Percent change in property values since 2019", "Percent change in property values since 2016"))

reg_map_16 <- leaflet(tp_prop_shp) %>%
  # addProviderTiles(providers$CartoDB) %>%
  addTiles(options = tileOptions(opacity = 0.5)) %>%
  add_prop_vals(pal_funct = pal_tp_propvals, var = "current.value", labs = labs_poly_16, df = tp_prop_shp_16merge) %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val", grp = "Percent change in property values since 2019", labs = labs_poly_16, df = tp_prop_shp_16merge) %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val_1622", grp = "Percent change in property values since 2016", labs = labs_poly_16, df = tp_prop_shp_16merge) %>%
  leaflet::addLayersControl(overlayGroups = c("Current property values",
                                              "Percent change in property values since 2019", "Percent change in property values since 2016"), 
                            position = "topleft", 
                            options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("Percent change in property values since 2019", "Percent change in property values since 2016"))

htmlwidgets::saveWidget(reg_map_16, "./data/reg_map_16.html", selfcontained = T)
htmlwidgets::saveWidget(carto_map_16, "./data/carto_map_16.html", selfcontained = T)

tp_prop_shp_checkvals <- tp_prop_shp %>%
  select(acct, current.value, base.value, land_assmt, current.land.value, improv_assmt, pref_assmt, current.imp.value) %>%
  mutate(check_val = current.value == land_assmt + improv_assmt,
         check_improv = current.imp.value == improv_assmt,
         check_land = current.land.value == land_assmt)

write.csv(tp_prop_shp_checkvals, "./data/tp_prop_shp_checkvals.csv")


### work with condos
# look at condo layer
condo_poly



# tp_props <- mc_prop_shp %>%
#   filter(TOWN_CODE == "16023")
# 
# tp_prop <- mc_prop_shp %>%
#   filter(grepl("TAKOMA", PREMISE_ADDR_CITY, ignore.case = T)) 
# 
# mc_prop_acct <- mc_prop_shp %>%
#   rename_all(tolower)
# 
# tp_prop$ACCT %>%
#   str_length() %>%
#   unique
# 
# tp_prop_shp <- 
# 
# tp_prop <- mc_prop_acct %>%
#   filter(grepl("TAKOMA", premise_addr_city, ignore.case = T))