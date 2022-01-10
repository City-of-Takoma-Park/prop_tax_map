library(leaflet)
library(tidyverse)
library(leafletwrappers)
library(sf)
library(tpfuncts)
library(htmltools)
library(RColorBrewer)

# read in land-code values and owner code values from county assessor
land_recode <- openxlsx::read.xlsx("./data/county_code_expl.xlsx", sheet = "Code Explanations") %>%
  rename_all(tolower)

owner_recode <- openxlsx::read.xlsx("./data/county_code_expl.xlsx", sheet = "Sheet1") %>%
  rename_all(tolower)

# read in tp property values from county assessor, extract county/district ID from tax name to merge to county property tax data, calculate change in value and percent change, generate full address
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

tp_props$length %>%
  unique

# read in county property shapefile from https://montgomeryplanning.org/tools/gis-and-mapping/data-downloads/
mc_prop_shp <- st_read("./data/Property/property.gdb") %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

# merge property shapefile and tp properties
tp_prop_shp <- mc_prop_shp %>%
  rename_all(tolower) %>%
  right_join(tp_props, 
            by = "acct")

# per explanation from county planning office, unmatched = condos or other points; confirmed in data 
tp_prop_missing <- tp_props %>%
  anti_join((mc_prop_shp %>%
               rename_all(tolower)), 
             by = "acct")

# generate palette for data
pal_tp_propvals <- leafletwrappers::pal_numeric(colors = "Blues", var = "current.value", df = st_drop_geometry(tp_prop_shp), reverse = F)

# look at distribution of values and set bins for legend based on that
quantile(st_drop_geometry(tp_prop_shp)[["current.value"]], probs = seq(0, 1, .05))

quantile(st_drop_geometry(tp_prop_shp)[["pct_chng_val"]], probs = seq(0, 1, .05))

# tp_prop_bins <- c(100, 175000, 310000, 475000, 525000, 575000, 625000, 700000, 800000, 1000000, 51000000)

tp_prop_bins <- c(100, 175000, 350000, 500000, 600000, 700000, 800000, 1000000, 51000000)

# tp_chng_bins <- c(-30, -5, 0, 5, 10, 20, 25, 30, 40, 50, 100, 750)
tp_chng_bins <- c(-30, -5, 0, 10, 20, 30, 40, 50, 100, 750)


pal_tp_propvals <- leaflet::colorBin("Blues", domain = c(100, 51000000),
                                    bins = tp_prop_bins)


colors <- RColorBrewer::brewer.pal(11, "PiYG")
colors <- RColorBrewer::brewer.pal(11, "Greens")[-1]

# use pinkish colors to represent negative values - append them to palette
chng_colors <- c("#8E0152", "#F1B6DA", "#F7F7F7", colors)

pal_tp_chngval <- leaflet::colorBin(palette = chng_colors, domain = c(-30, 750),
                                    bins = tp_chng_bins)

# pal_tp_chngval <- pal_numeric(colors = "PuOr", var = "pct_chng_val", df = st_drop_geometry(tp_prop_shp))

# generate labels for tp property dataset
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

# define function for adding property tax polygon layers
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
      popupOptions = leaflet::popupOptions(
        maxHeight = 200,
        maxWidth = 200
        ),
      label = ~ address_full,
      labelOptions = labelOptions(
        style = list(`font-weight` = "normal",
                     padding = "0.2px 0.2px",
                     `line-height` = 0.8),
        textsize = "10px",
        direction = "auto",
        opacity = 0.8
      ),
      highlight = leaflet::highlightOptions(
        stroke = TRUE,
        weight = 3.5,
        fillOpacity = 0.6, 
        color = "#555EE7",
        opacity = 1, 
        bringToFront = TRUE), 
      popup = purrr::map(labs, htmltools::HTML), 

      data = df) %>%
    addLegend(position = "topright", 
              pal = pal_funct, 
              values = ~ as.data.frame(df)[[var]], 
              group = grp, 
              opacity = 0.7, 
              title= paste0("Takoma Park Property Assessments, 2022<br>
                            ", grp))
}

# define text to add to bottom
test_control <- tags$div(HTML("Click on a highlighted property to find out more information"))

# create map - carto basemap
carto_map <- leaflet(tp_prop_shp) %>%
  addProviderTiles(providers$CartoDB) %>%
  add_prop_vals(pal_funct = pal_tp_propvals, var = "current.value") %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val", grp = "Percent change in property values") %>%
  leaflet::addLayersControl(overlayGroups = c("Current property values",
                                              "Percent change in property values"), 
                            position = "topleft", 
                            options = layersControlOptions(collapsed = F)) %>%
  hideGroup("Percent change in property values") %>%
  addControl(html = test_control, position = "bottomleft")

carto_map

# create map - normal basemap
reg_map <- leaflet(tp_prop_shp) %>%
  # addProviderTiles(providers$CartoDB) %>%
  addTiles(options = tileOptions(opacity = 0.5)) %>%
  add_prop_vals(pal_funct = pal_tp_propvals, var = "current.value") %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val", grp = "Percent change in property values") %>%
  leaflet::addLayersControl(overlayGroups = c("Current property values",
                                              "Percent change in property values"), 
                            position = "topleft", 
                            options = layersControlOptions(collapsed = F)) %>%
  hideGroup("Percent change in property values") %>%
  addControl(html = test_control, position = "bottomleft")

# save maps
htmlwidgets::saveWidget(reg_map, "./data/reg_map.html", selfcontained = T)
htmlwidgets::saveWidget(carto_map, "./data/carto_map.html", selfcontained = T)
htmlwidgets::saveWidget(carto_map, "./data/property-tax-map.html", selfcontained = T)

#### work with county planning property data - sent by county planning
county_data <- st_layers("./data/county_tax_data/TakomaPark/db.gdb")

# readin 2016 assessment data
ppwd_2016 <- st_read("./data/county_tax_data/TakomaPark/db.gdb", layer = "PPWD_2016")

# read in condo points to polygon shapefile - for future use
condo_poly <- st_read("./data/county_tax_data/TakomaPark/db.gdb", layer = "CondoPoints_to_Poly")

# read in csv version 2016 assessments
ppwd_2016_csv <- read.csv("./data/county_tax_data/TakomaPark/ppwd2016.txt") %>%
  rename_all(tolower) %>%
  select(acct, land_assmt, pref_assmt, improv_assmt, property_code, premise_addr_street, premise_addr_city) %>%
  rename_all(.funs = ~ paste0(.x, "_16"))

# merge 2016 data to properties file - caculate value in 2016, change in value since 2016 from 2019 to 2016 and 2022 to 2016, and eprcentages
tp_prop_shp_16merge <- tp_prop_shp %>%
  left_join(ppwd_2016_csv, by = c("acct"= "acct_16")) %>%
  mutate(
    val_16 = land_assmt_16 + improv_assmt_16,
    chng_val_1622 = current.value - val_16,
    chng_val_1619 = base.value - val_16,
    pct_chng_val_1622 = pct_round(chng_val_1622, val_16),
    pct_chng_val_1619 = pct_round(chng_val_1619, val_16))

# check missing
tp_prop_shp_16merge_unjoin <- tp_prop_shp %>%
  anti_join(ppwd_2016_csv, by = c("acct"= "acct_16"))

# create labels for 2016 file
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

# view value distribution
quantile(st_drop_geometry(tp_prop_shp_16merge)[["pct_chng_val_1622"]], probs = seq(0, 1, .05), na.rm = T)


# pick new bins for updated value distribution
# tp_chng_bins <- c(-100, -30, -5, 0, 5, 10, 20, 25, 30, 40, 50, 100, 750)
tp_chng_bins <- c(-100, -30, -5, 0, 10, 20, 30, 40, 50, 100, 750)

chng_colors <- c("#8E0152", "#C51B7D", "#F1B6DA", "#F7F7F7", colors)

pal_tp_chngval <- leaflet::colorBin(palette = chng_colors, domain = c(-100, 750),
                                    bins = tp_chng_bins)

# create map with layers for change in value since 2016
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
  hideGroup(c("Percent change in property values since 2019", "Percent change in property values since 2016")) %>%
  addControl(html = test_control, position = "bottomleft")


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
  hideGroup(c("Percent change in property values since 2019", "Percent change in property values since 2016")) %>%
  addControl(html = test_control, position = "bottomleft")


htmlwidgets::saveWidget(reg_map_16, "./data/reg_map_16.html", selfcontained = T)
htmlwidgets::saveWidget(carto_map_16, "./data/carto_map_16.html", selfcontained = T)

# test data - resolving differences with county
tp_prop_shp_checkvals <- tp_prop_shp %>%
  select(acct, current.value, base.value, land_assmt, current.land.value, improv_assmt, pref_assmt, current.imp.value) %>%
  mutate(check_val = current.value == land_assmt + improv_assmt,
         check_improv = current.imp.value == improv_assmt,
         check_land = current.land.value == land_assmt)

write.csv(tp_prop_shp_checkvals, "./data/tp_prop_shp_checkvals.csv")


#### load in points shapefile from county planning - https://montgomeryplanning.org/tools/gis-and-mapping/data-downloads/
points_file <- st_read("./data/Property_Points.gdb") %>%
  st_transform(4326) %>%
  rename_all(tolower)

# join point properties that didnt merge to polygons to this
tp_points_read <- points_file %>%
  right_join(tp_prop_missing, by = c("tax_no" = "acct" ))


sf::sf_use_s2(FALSE)

# a few erroneous polygons well outside city - filter those out, will check with county
intersect_city <- tp_points_read %>%
  st_intersection(wards %>%
                  st_transform(4326))

# assign to tp points
tp_points <- intersect_city

# confirms no points missing
missing_points <-tp_points_read %>%
  anti_join( points_file, by = c("acct" = "tax_no"))

quantile(st_drop_geometry(tp_points)[["current.value"]], probs = seq(0, 1, .05))

# recode distribution of values based on lower-condo values, and generate new palette
tp_prop_bins_condos <- c(100, 50000, 100000, 150000, 200000, 250000, 300000, 1000000)

pal_condos <- c("#808080", "#BEC5C6", brewer.pal(length(tp_prop_bins_condos) - 2, "Blues"))

pal_tp_propvals_condos <- leaflet::colorBin(pal_condos, domain = c(100, 1000000),
                                     bins = tp_prop_bins_condos)
# 
# colors <- RColorBrewer::brewer.pal(11, "PiYG")
# colors <- RColorBrewer::brewer.pal(11, "Greens")[-1]
# 
# 
# chng_colors <- c("#8E0152", "#F1B6DA", "#F7F7F7", colors)
# 
# pal_tp_chngval <- leaflet::colorBin(palette = chng_colors, domain = c(-30, 750),
#                                     bins = tp_chng_bins)

# quantile(st_drop_geometry(tp_points)[["pct_chng_val"]], probs = seq(0, 1, .05))

# create new labels for points layer
labs_points <- leafletwrappers::label_standardize(st_drop_geometry(tp_points), 
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

# create function to add points layer
add_point_vals <- function(basemap,
                          var,
                          pal_funct,
                          grp = "Current property values (condos and other point-properties)",
                          labs = labs_points,
                          df = tp_points) {
  basemap %>%
    leaflet::addCircleMarkers(
      radius = 0.1,
      color = ~ pal_funct(st_drop_geometry(df)[[var]]),
      group = grp,
      stroke = T,
      weight = 20, 
      data = df,
      clusterOptions = markerClusterOptions(),
      popupOptions = leaflet::popupOptions(
        maxHeight = 200,
        maxWidth = 200
      ),
      label = ~ address_full,
      labelOptions = labelOptions(
        style = list(`font-weight` = "normal",
                     padding = "0.2px 0.2px",
                     `line-height` = 0.8),
        textsize = "10px",
        direction = "auto",
        opacity = 0.8
      ),
      # highlight = leaflet::highlightOptions(
      #   stroke = TRUE,
      #   weight = 3.5,
      #   fillOpacity = 0.6, 
      #   color = "#555EE7",
      #   opacity = 1, 
      #   bringToFront = TRUE), 
      popup = purrr::map(labs, htmltools::HTML)
      ) %>%
    addLegend(position = "topright", 
              pal = pal_funct, 
              values = ~ as.data.frame(df)[[var]], 
              group = grp, 
              opacity = 0.7,
              title= paste0("Takoma Park Property Assessments, 2022<br>
                            ", grp))
}

# define updated control
condo_control <- tags$div(HTML("Click on a highlighted property to find out more information. Click on point-clusters to break them into smaller clusters and points; click on the point to find out more information"))

# generate points maps
carto_map_points <- leaflet(tp_prop_shp) %>%
  addProviderTiles(providers$CartoDB) %>%
  # addTiles(options = tileOptions(opacity = 0.5)) %>%
  add_prop_vals(pal_funct = pal_tp_propvals, var = "current.value") %>%
  add_prop_vals(pal_funct = pal_tp_chngval, var = "pct_chng_val", grp = "Percent change in property values") %>%
  add_point_vals(var = "current.value", pal_funct = pal_tp_propvals_condos, labs = labs_points, df = tp_points) %>%
  add_point_vals(var = "pct_chng_val", pal_funct = pal_tp_chngval, grp = "Percent change in property values (condos and other point-properties)", labs = labs_points, df = tp_points) %>%
  leaflet::addLayersControl(overlayGroups = c(
    "Current property values", 
    "Percent change in property values", 
    "Current property values (condos and other point-properties)",
    "Percent change in property values (condos and other point-properties)"
  ), 
  position = "topleft", 
  options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("Current property values (condos and other point-properties)", "Percent change in property values", "Percent change in property values (condos and other point-properties)")) %>%
  addControl(html = condo_control, position = "bottomleft")

carto_map_points

htmlwidgets::saveWidget(carto_map_points, "./data/carto_map_points.html", selfcontained = T)
htmlwidgets::saveWidget(carto_map_points, "./data/property-tax-map-points.html", selfcontained = T)
