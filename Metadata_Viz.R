# Install necessary packages
if("tigris" %in% rownames(installed.packages()) == FALSE) {install.packages("tigris", repos='http://cran.us.r-project.org')};
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet", repos='http://cran.us.r-project.org')};

# Load necessary packages
library(tigris)
library(leaflet)

# Load U.S. State and County spatial data frames
us_states <- states(cb = TRUE, resolution = '20m', year = 2015)
us_counties <- counties(cb = TRUE, resolution = '20m', year = 2015)

# Create regular data frames with simulated metadata measures for both states and counties
State_GEOID <- us_states@data$GEOID
State_Percent <- sample(0:100, 52, replace = TRUE)
State_Measures <- data.frame(State_GEOID, State_Percent)  

County_GEOID <- us_counties@data$GEOID
County_Percent <- sample(0:100, 3220, replace = TRUE)
County_Measures <- data.frame(County_GEOID, County_Percent)  

# Merge regular data frame to spatial data frame for states and counties
state_metadata_merged <- geo_join(us_states, State_Measures, "GEOID", "State_GEOID", by = NULL, how = "left")
county_metadata_merged <- geo_join(us_counties, County_Measures, "GEOID", "County_GEOID", by = NULL, how = "left")

# Define set of colors
color_set <- colorQuantile("Greens", NULL, n = 4)

# Setup popup text boxes for states and counties, populate with corresponding data for each jurisdiction
state_popup <- paste0("<strong>State: </strong>", state_metadata_merged$NAME, 
                      "<br><strong>Percent Completeness: </strong>", as.character(state_metadata_merged$State_Percent))
county_popup <- paste0("<strong>State FIPS Code: </strong>", county_metadata_merged$STATEFP,
                       "<br><strong>County: </strong>", county_metadata_merged$NAME, 
                       "<br><strong>Percent Completeness: </strong>", as.character(county_metadata_merged$County_Percent))

# Create map
leaflet() %>%
  # Pick the popular map design known as "CartoDB.Positron"
  addProviderTiles("CartoDB.Positron") %>% 
  # This addPolygons section adds the geospatial and metadata for U.S. States
  addPolygons(data = state_metadata_merged,  
              fillColor = ~color_set(state_metadata_merged$State_Percent), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = state_popup, group = "States") %>%
  # This addPolygons section adds the geospatial and metadata for U.S. Counties
  addPolygons(data = county_metadata_merged, 
              fillColor = ~color_set(county_metadata_merged$County_Percent), 
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
