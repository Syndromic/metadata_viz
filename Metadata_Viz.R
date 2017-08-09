# Load necessary packages
library(tigris)
library(leaflet)
library(rio)
library(stringr)
library(magrittr)
library(dplyr)
library(shiny)


## UI ----
ui <- fluidPage(
  leafletOutput("mymap"),
  absolutePanel(fixed = TRUE, draggable = TRUE, top = 60, bottom = "auto", left = 20, right = "auto",
                width = 330, height = "auto",
                
                h2("Metadata Viz App"),
                
                # Select completeness or timeliness domain
                selectInput(
                  "domain", label = h3("Select data quality domain:"),
                  c(Completeness = "completeness",
                    Timeliness = "timeliness"), selected = "completeness"),
                
                # Only show this panel if completeness domain is selected
                conditionalPanel(
                  condition = "input.domain == 'completeness'",
                  selectInput("field", label = h3("Select field:"), 
                              choices = list("Administrative_Sex" = "Administrative_Sex", "C_Patient_Age" = "C_Patient_Age", "Age_Calculated" = "Age_Calculated",
                                             "Age_Reported" = "Age_Reported", "Age_Units_Calculated" = "Age_Units_Calculated", "Age_Units_Reported" = "Age_Units_Reported",
                                             "Birth_Date_Time" = "Birth_Date_Time", "Patient_Zip" = "Patient_Zip", "C_Chief_Complaint" = "C_Chief_Complaint",
                                             "Admit_Reason_Description" = "Admit_Reason_Description", "Chief_Complaint_Text" = "Chief_Complaint_Text", "Admit_Reason_Combo" = "Admit_Reason_Combo",
                                             "Clinical_Impression" = "Clinical_Impression", "Triage_Notes" = "Triage_Notes", "Discharge_Disposition" = "Discharge_Disposition",
                                             "Diagnosis_Combo" = "Diagnosis_Combo"), 
                              selected = "Administrative_Sex")
                ),
                # Only show this panel if timeliness domain is selected
                conditionalPanel(
                  condition = "input.domain == 'timeliness'",
                  selectInput("measure", label = h3("Select measure:"), 
                              choices = list("Percent of records received within 24 hours" = "pLessThan24", "Percent of records received within 48 hours" = "pLessThan48", "Average number of days until 80% of records received" = "d_p80"), 
                              selected = "pLessThan24", width = "450px")
                )
  )
)

## Server ----
server <- function(input, output) {
  
  ## Import Metadata Workgroup data ----
  Timeliness <- import("NSSP_MDWG_aYear.xlsx", which=5)
  Completeness <- suppressWarnings(import("NSSP_MDWG_aYear.xlsx", which=6, col_types=c("timevar"="numeric", "site_id"="numeric", "Site_Name"="text", "Site_State"="text", "dqgrp"="numeric", "dqgrp2"="numeric", "dqvar"="text", "pvisits"="numeric"))) # Ignore warnings. These are triggered by "NR" character entries instead of numeric values in the pvisits field. They end up being processed correctly (as NAs) so it is OK.
  
  ## Clean and format Timeliness data ----
  Timeliness_Extended <- Timeliness %>% mutate(
                                Geographic_Level = case_when(
                                  str_detect(.$Site_Name, ",") == 1 ~ "County",
                                  str_detect(.$Site_Name, ",") == 0 ~ "State"
                                ),
                                County_Name = case_when(
                                  str_detect(.$Site_Name, ",") == 1 ~ str_split_fixed(.$Site_Name, ",", 2)[ , 1],
                                  str_detect(.$Site_Name, ",") == 0 ~ NA_character_ 
                                ),
                                pLessThan24 = (pLessThan24 * 100) %>% round(2),
                                pLessThan48 = (pLessThan48 * 100) %>% round(2),
                                d_p80 = d_p80 %>% round(2)
  )
  
  ## Clean and format Completeness data ----
  Completeness_Extended <- Completeness %>% mutate( 
                                  Geographic_Level = case_when(
                                    str_detect(.$Site_Name, ",") == 1 ~ "County",
                                    str_detect(.$Site_Name, ",") == 0 ~ "State"
                                  ),
                                  County_Name = case_when(
                                    str_detect(.$Site_Name, ",") == 1 ~ str_split_fixed(.$Site_Name, ",", 2)[ , 1],
                                    str_detect(.$Site_Name, ",") == 0 ~ NA_character_ 
                                  ),
                                  pvisits = (pvisits * 100) %>% round(2)
  )

  ## Load spatial data and merge with dataframes ----
  
  # Load U.S. State and County spatial data frames
  us_states <- states(cb = TRUE, resolution = '20m', year = 2015)
  us_counties <- counties(cb = TRUE, resolution = '20m', year = 2015)
  
  # Map code only
  output$mymap <- renderLeaflet({
    
    # If completeness domain selected
    if(input$domain == "completeness"){  
      # Filter by field and county  
      state_data <- filter(Completeness_Extended, dqvar == input$field, Geographic_Level == "State") %>%
        rename(measure = pvisits)
      county_data <- filter(Completeness_Extended, dqvar == input$field, Geographic_Level == "County") %>%
        rename(measure = pvisits)
    }
    
    # If timeliness domain is selected  
    if (input$domain == "timeliness"){
      state_data <- filter(Timeliness_Extended, Geographic_Level == "State") %>%
        rename_(measure = input$measure)
      county_data <- filter(Timeliness_Extended, Geographic_Level == "County") %>%
        rename_(measure = input$measure)      
    } 
    
    # Associate county data with geocodes
    data(fips_codes)
    counties_codes_data <- inner_join(county_data, fips_codes, c("Site_State" = "state", "County_Name" = "county"))
    counties_geo_data <- inner_join(us_counties@data, counties_codes_data, c("STATEFP" = "state_code", "COUNTYFP" = "county_code"))
    
    # Merge regular data frame to spatial data frame for states and counties
    state_metadata_merged <- geo_join(us_states, state_data, "STUSPS", "Site_State", by = NULL, how = "left")
    county_metadata_merged <- geo_join(us_counties, counties_geo_data, "GEOID", "GEOID", by = NULL, how = "left")
    
    ## Define color sets, legend titles, and text popups ----
    if (input$domain == "completeness"){
      color_set <- colorBin("Greens", NULL, bins = c(0, 25, 50, 75, 100))
      state_popup <- paste0("<strong>State: </strong>", state_metadata_merged$NAME, 
                            "<br><strong>Percent Completeness: </strong>", as.character(state_metadata_merged$measure))
      county_popup <- paste0("<strong>State FIPS Code: </strong>", county_metadata_merged$STATEFP,
                             "<br><strong>County: </strong>", county_metadata_merged$NAME, 
                             "<br><strong>Percent Completeness: </strong>", as.character(county_metadata_merged$measure))
      Legend_Title <- "Percent Completeness"
    } else if (input$measure == "pLessThan24"){
      color_set <- colorBin("Greens", NULL, bins = c(0, 25,50, 75, 100))
      state_popup <- paste0("<strong>State: </strong>", state_metadata_merged$NAME, 
                            "<br><strong>Percent Of Records Received Within 24 Hours: </strong>", as.character(state_metadata_merged$measure))
      county_popup <- paste0("<strong>State FIPS Code: </strong>", county_metadata_merged$STATEFP,
                             "<br><strong>County: </strong>", county_metadata_merged$NAME, 
                             "<br><strong>Percent Of Records Received Within 24 Hours: </strong>", as.character(county_metadata_merged$measure))
      Legend_Title <- "Percent Received Within 24 Hours"
    } else if (input$measure == "pLessThan48"){
      color_set <- colorBin("Greens", NULL, bins = c(0, 25,50, 75, 100))
      state_popup <- paste0("<strong>State: </strong>", state_metadata_merged$NAME, 
                            "<br><strong>Percent Of Records Received Within 48 Hours: </strong>", as.character(state_metadata_merged$measure))
      county_popup <- paste0("<strong>State FIPS Code: </strong>", county_metadata_merged$STATEFP,
                             "<br><strong>County: </strong>", county_metadata_merged$NAME, 
                             "<br><strong>Percent Of Records Received Within 48 Hours: </strong>", as.character(county_metadata_merged$measure))
      Legend_Title <- "Percent Received Within 48 Hours"
    } else if (input$measure == "d_p80"){
      color_set <- colorBin("Reds", NULL, bins = c(0, 1, 2, 5, 50))
      state_popup <- paste0("<strong>State: </strong>", state_metadata_merged$NAME, 
                            "<br><strong>Average # Of Days Until 80% Of Records Received: </strong>", as.character(state_metadata_merged$measure))
      county_popup <- paste0("<strong>State FIPS Code: </strong>", county_metadata_merged$STATEFP,
                             "<br><strong>County: </strong>", county_metadata_merged$NAME, 
                             "<br><strong>Average # Of Days Until 80% Of Records Received: </strong>", as.character(county_metadata_merged$measure))
      Legend_Title <- "Average # Of Days"
    } 
    
    ## Create map----
    
    leaflet() %>%
      # Pick the popular map design known as "CartoDB.Positron"
      addProviderTiles("CartoDB.Positron") %>% 
      # This addPolygons section adds the geospatial and metadata for U.S. States
      addPolygons(data = state_metadata_merged,  
                  fillColor = ~color_set(state_metadata_merged$measure), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = state_popup, group = "States") %>%
      # This addPolygons section adds the geospatial and metadata for U.S. Counties
      addPolygons(data = county_metadata_merged, 
                  fillColor = ~color_set(county_metadata_merged$measure), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = county_popup, group = "Counties") %>%
      # This addLegend section creates a legend (one color for each 25%-bin of the range 0-100%)
      addLegend(pal = color_set,
                values = c(0:100), 
                position = "bottomright", 
                title = Legend_Title) %>%
      # Creates two map layers, one for States, one for Counties
      addLayersControl(
        baseGroups = c("States", "Counties"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      # Uncheck Counties box so that only the States box is checked initially
      hideGroup("Counties") %>% 
      # Set to zoom in and center on U.S.initially
      setView(lng = -95.712891, lat = 37.09024, zoom = 3)
  })
  
}

shinyApp(ui = ui, server = server)
