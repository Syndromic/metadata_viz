## Install and load required packages----
# Install necessary packages
if("tigris" %in% rownames(installed.packages()) == FALSE) {install.packages("tigris", repos='http://cran.us.r-project.org')};
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet", repos='http://cran.us.r-project.org')};
if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio", repos='http://cran.us.r-project.org')};

# Load necessary packages
library(tigris)
library(leaflet)
library(rio)
library(stringr)
library(magrittr)
library(dplyr)

## Grab real Metadata Workgroup data and merge with spatial data for mapping----
Timeliness <- import("NSSP_MDWG_aYear.xlsx", which=5)
Completeness <- import("NSSP_MDWG_aYear.xlsx", which=6) # Ignore warnings. These are triggered by odd entries (NR instead of %) but they are processed correctly (as NAs).

Timeliness_Extended <- mutate(Timeliness, 
                              Geographic_Level = case_when(
                                                          str_detect(Timeliness$Site_Name, ",") == 1 ~ "County",
                                                          str_detect(Timeliness$Site_Name, ",") == 0 ~ "State"
                                                          ),
                              County_Name = str_split_fixed(Timeliness$Site_Name, ",", 2)[ , 1]
                              )

Completeness_Extended <- mutate(Completeness, 
                                Geographic_Level = case_when(
                                                            str_detect(Completeness$Site_Name, ",") == 1 ~ "County",
                                                            str_detect(Completeness$Site_Name, ",") == 0 ~ "State"
                                                            ),
                                County_Name = str_split_fixed(Completeness$Site_Name, ",", 2)[ , 1],
                                pvisits = if_else(is.na(pvisits) == TRUE, 0, pvisits)
                                )

state_sex <- filter(Completeness_Extended, dqvar == "Administrative_Sex", Geographic_Level == "State")
county_sex <- filter(Completeness_Extended, dqvar == "Administrative_Sex", Geographic_Level == "County")
 
# Load U.S. State and County spatial data frames
us_states <- states(cb = TRUE, resolution = '20m', year = 2015)
us_counties <- counties(cb = TRUE, resolution = '20m', year = 2015)

# Associate county data with geocodes
data(fips_codes)
counties_codes_sex <- inner_join(county_sex, fips_codes, c("Site_State" = "state", "County_Name" = "county"))
counties_geo_sex <- inner_join(us_counties@data, counties_codes_sex, c("STATEFP" = "state_code", "COUNTYFP" = "county_code"))

# Merge regular data frame to spatial data frame for states and counties
state_metadata_merged <- geo_join(us_states, state_sex, "STUSPS", "Site_State", by = NULL, how = "left")
county_metadata_merged <- geo_join(us_counties, counties_geo_sex, "GEOID", "GEOID", by = NULL, how = "left")

# Define set of colors
color_set <- colorBin("Greens", NULL, bins = c(0, 0.25, 0.50, 0.75, 1.00))

# Setup popup text boxes for states and counties, populate with corresponding data for each jurisdiction
state_popup <- paste0("<strong>State: </strong>", state_metadata_merged$NAME, 
                      "<br><strong>Percent Completeness: </strong>", as.character(state_metadata_merged$pvisits))
county_popup <- paste0("<strong>State FIPS Code: </strong>", county_metadata_merged$STATEFP,
                       "<br><strong>County: </strong>", county_metadata_merged$NAME, 
                       "<br><strong>Percent Completeness: </strong>", as.character(county_metadata_merged$pvisits))

## Create map----

leaflet() %>%
  # Pick the popular map design known as "CartoDB.Positron"
  addProviderTiles("CartoDB.Positron") %>% 
  # This addPolygons section adds the geospatial and metadata for U.S. States
  addPolygons(data = state_metadata_merged,  
              fillColor = ~color_set(state_metadata_merged$pvisits), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = state_popup, group = "States") %>%
  # This addPolygons section adds the geospatial and metadata for U.S. Counties
  addPolygons(data = county_metadata_merged, 
              fillColor = ~color_set(county_metadata_merged$pvisits), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = county_popup, group = "Counties") %>%
  # This addLegend section creates a legend (one color for each 25%-quantile of the range 0-100%)
  addLegend(pal = color_set,
            values = c(0:100), 
            position = "bottomright", 
            title = "Percent Completeness") %>%
  # Creates two map layers, one for States, one for Counties
  addLayersControl(
    baseGroups = c("States", "Counties"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  # Uncheck Counties box so that only the States box is checked initially
  hideGroup("Counties") %>% 
  # Set to zoom in and center on U.S.initially
  setView(lng = -95.712891, lat = 37.09024, zoom = 3)
