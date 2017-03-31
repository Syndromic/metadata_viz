######## Install and load required packages =======================
# Install necessary packages
if("tigris" %in% rownames(installed.packages()) == FALSE) {install.packages("tigris", repos='http://cran.us.r-project.org')};
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet", repos='http://cran.us.r-project.org')};
if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio", repos='http://cran.us.r-project.org')};

# Load necessary packages
library(tigris)
library(leaflet)
library(rio)

######## Grab real Metadata Workgroup data and merge with spatial data for mapping =======================

# import timeliness & completeness data, separate into data from states/from counties
Timeliness <- import("NSSP_MDWG_aYear.xlsx", which=5)
Completeness <- import("NSSP_MDWG_aYear.xlsx", which=6) # Ignore warnings. These are triggered by odd entries (NR instead of %) but they are processed correctly (as NAs).

which_counties_timeliness = which(grepl(",", Timeliness$Site_Name, fixed = TRUE))
which_counties_completeness = which(grepl(",", Completeness$Site_Name, fixed = TRUE))

timeliness_counties = Timeliness[which_counties_timeliness, ]
timeliness_states = Timeliness[-which_counties_timeliness, ]

completeness_counties = Completeness[which_counties_completeness, ]
completeness_states = Completeness[-which_counties_completeness, ]

# Load U.S. State and County spatial data frames
us_states <- states(cb = TRUE, resolution = '20m', year = 2015)
us_counties <- counties(cb = TRUE, resolution = '20m', year = 2015)

##### Create regular data frames with simulated metadata measures for both states and counties  (edit: real data is now in)
##### Merge regular data frame to spatial data frame for states and counties

### //// data for states
# // timeliness
timeliness_states_metadata_merged = geo_join(spatial_data = us_states, data_frame = timeliness_states, 
                                             by_sp = "NAME", by_df = "Site_Name", how = "left")

# // completeness
# the below does not work!  
# (because the completeness data is "long" and there are several rows which match each row in the spatial data - must reshape to "wide" first)
## completeness_states_metadata_merged = geo_join(spatial_data = us_states, data_frame = completeness_states, 
##                                              by_sp = "NAME", by_df = "Site_Name", how = "left")

completeness_states2 = completeness_states[, -c(1, 5, 6)]   #remove timevar, dqgrp, dqgrp2 (these variables would mess up the reshape step)
completeness_states_wide = reshape(completeness_states2, 
                                   timevar = c("dqvar") , idvar = c("site_id", "Site_Name", "Site_State"), 
                                   direction = "wide")

completeness_states_metadata_merged = geo_join(spatial_data = us_states, data_frame = completeness_states_wide, 
                                             by_sp = "NAME", by_df = "Site_Name", how = "left")

### //// data for counties
# // timeliness
thisgeoid = c()
for (j in 1:(dim(timeliness_counties)[1]) ){
  stateFPfromstatedf = us_states@data$STATEFP[us_states@data$STUSPS == timeliness_counties$Site_State[j]]
  ## to match, need to cut off the last 11 characters from timeliness county name (' COUNTY, SS' where SS is the 2-letter abbreviation for the state)
  nch = nchar(timeliness_counties$Site_Name[j])
  truncsitename = substr(timeliness_counties$Site_Name[j], 1, (nch-11))
  thisgeoid[j] = us_counties@data$GEOID[us_counties@data$STATEFP == stateFPfromstatedf & us_counties@data$NAME == truncsitename]
}
timeliness_counties = data.frame(thisgeoid, timeliness_counties, stringsAsFactors = FALSE)

timeliness_counties_metadata_merged = geo_join(spatial_data = us_counties, data_frame = timeliness_counties, 
                                             by_sp = "GEOID", by_df = "thisgeoid", how = "left")

# // completeness
completeness_counties2 = completeness_counties[, -c(1, 5, 6)]   #remove timevar, dqgrp, dqgrp2 (these variables would mess up the reshape step)
completeness_counties_wide = reshape(completeness_counties2, 
                                   timevar = c("dqvar") , idvar = c("site_id", "Site_Name", "Site_State"), 
                                   direction = "wide")

thisgeoid = c()
for (j in 1:(dim(completeness_counties_wide)[1]) ){
  stateFPfromstatedf = us_states@data$STATEFP[us_states@data$STUSPS == completeness_counties_wide$Site_State[j]]
  ## to match, need to cut off the last 11 characters from completeness county name (' COUNTY, SS' where SS is the 2-letter abbreviation for the state)
  nch = nchar(completeness_counties_wide$Site_Name[j])
  truncsitename = substr(completeness_counties_wide$Site_Name[j], 1, (nch-11))
  thisgeoid[j] = us_counties@data$GEOID[us_counties@data$STATEFP == stateFPfromstatedf & us_counties@data$NAME == truncsitename]
}

completeness_counties_wide = data.frame(thisgeoid, completeness_counties_wide, stringsAsFactors = FALSE)

completeness_counties_metadata_merged = geo_join(spatial_data = us_counties, data_frame = completeness_counties_wide, 
                                               by_sp = "GEOID", by_df = "thisgeoid", how = "left")

######## Create map =======================

# (setup for map) Define set of colors
color_set <- colorBin("Greens", NULL, bins = c(0, 0.25, 0.50, 0.75, 1.00))

### (setup for map) Set up popup text boxes for states and counties, populate with corresponding data for each jurisdiction

## /// completeness popups
numcompletenessvars = length(unique(Completeness$dqvar))
completenessvars = unique(Completeness$dqvar)

for (i in 1:numcompletenessvars){
  this.completenessvar = completenessvars[i]
  this.completenessvar2 = paste("pvisits.", this.completenessvar, sep = "")
  
  ## states
  this.completenessvardata = completeness_states_metadata_merged[[this.completenessvar2]]
  this.completenessvardata = round(this.completenessvardata*100, 2)   #do I have to re-geojoin this to the spatial data frame for the popup to work?
  
  tempstatepopup = paste0("<strong>State: </strong>", 
                          completeness_states_metadata_merged$NAME,     #interesting, don't have to do @data$NAME; just $NAME suffices
                          "<br><strong>Percent Completeness </strong>", "for ", this.completenessvar, ": ",   #Q: how to make variable name bold?
                          as.character(this.completenessvardata)  )

  # assign temp state popup to a particular completeness variable
  assign(paste("statepop.comp.", this.completenessvar, sep = ""), tempstatepopup)
  
  ## counties
  # Note: with this code, a county name is only able to be shown in the popup if that county has data for the variable at hand.
  # Otherwise, the county name is shown as NA.
  this.completenessvardata = completeness_counties_metadata_merged[[this.completenessvar2]]
  this.completenessvardata = round(this.completenessvardata*100, 2)   #do I have to re-geojoin this to the spatial data frame for the popup to work?
  
  tempcountypopup = paste0("<strong>County: </strong>", 
                          completeness_counties_metadata_merged@data$Site_Name,
                          "<br><strong>Percent Completeness </strong>", "for ", this.completenessvar, ": ",   #Q: how to make variable name bold?
                          as.character(this.completenessvardata)  )
  
  # assign temp county popup to a particular completeness variable
  assign(paste("countypop.comp.", this.completenessvar, sep = ""), tempcountypopup)
  
}

## /// timeliness popups
numtimelinessvars = length(which(names(Timeliness) %in% c("Site_Name", "Site_State", "site_id", "timevar") == FALSE))
timelinessvars = names(Timeliness)[which(names(Timeliness) %in% c("Site_Name", "Site_State", "site_id", "timevar") == FALSE)]

for (i in 1:numtimelinessvars){
  this.timelinessvar = timelinessvars[i]
  
  ## //// I do not know why this doesn't work (variable not found); the analogous code for completeness does work.
  ## I tried fiddling with it in various ways and still can't get it to work.
  # this.timelinessvardata = timeliness_states_metadata_merged[[this.timelinessnessvar]]  
  
  ## //// here's an alternative
  ## states
  whichcol = which(names(timeliness_states_metadata_merged) == "timevar")
  this.timelinessvardata = timeliness_states_metadata_merged@data[ ,(whichcol+i)]
  
  if(substr(this.timelinessvar, 1, 9) == "pLessThan"){   #not extremely general, but I think ok for now
    this.timelinessvardata = round(this.timelinessvardata*100, 2)   #only multiply the proportion variables by 100 to get percentages
  } else {
    this.timelinessvardata = round(this.timelinessvardata, 2)
  }
  
  tempstatepopup = paste0("<strong>State: </strong>", 
                          timeliness_states_metadata_merged$NAME,     #interesting, don't have to do @data$NAME; just $NAME suffices
                          "<br><strong>Timeliness - </strong>", this.timelinessvar, ": ",
                          as.character(this.timelinessvardata))
  
  # assign temp state popup to a particular timeliness variable
  assign(paste("statepop.time.", this.timelinessvar, sep = ""), tempstatepopup)
  
  ## counties
  # Note: with this code, a county name is only able to be shown in the popup if that county has data for the variable at hand.
  # Otherwise, the county name is shown as NA.
  whichcol = which(names(timeliness_counties_metadata_merged) == "timevar")
  this.timelinessvardata = timeliness_counties_metadata_merged@data[ ,(whichcol+i)]
  
  if(substr(this.timelinessvar, 1, 9) == "pLessThan"){   #not extremely general, but I think ok for now
    this.timelinessvardata = round(this.timelinessvardata*100, 2)   #only multiply the proportion variables by 100 to get percentages
  } else {
    this.timelinessvardata = round(this.timelinessvardata, 2)
  }
 
  tempcountypopup = paste0("<strong>County: </strong>", 
                          timeliness_counties_metadata_merged@data$Site_Name,     #interesting, don't have to do @data$NAME; just $NAME suffices
                          "<br><strong>Timeliness - </strong>", this.timelinessvar, ": ",
                          as.character(this.timelinessvardata))
  
  # assign temp county popup to a particular timeliness variable
  assign(paste("countypop.time.", this.timelinessvar, sep = ""), tempcountypopup)
  
}

### now, actually create the map
### *** note: this is an example; only the Birth_Date_Time completeness variable is included in this map

leaflet() %>%
  # Pick the popular map design known as "CartoDB.Positron"
  addProviderTiles("CartoDB.Positron") %>% 
  # This addPolygons section adds the geospatial and metadata for U.S. States (Birth_Date_Time completeness only)
  addPolygons(data = completeness_states_metadata_merged,  
              fillColor = ~color_set(completeness_states_metadata_merged$pvisits.Birth_Date_Time), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = statepop.comp.Birth_Date_Time, group = "States") %>%
  # This addPolygons section adds the geospatial and metadata for U.S. Counties (Birth_Date_Time completeness only)
  addPolygons(data = completeness_counties_metadata_merged, 
              fillColor = ~color_set(completeness_counties_metadata_merged$pvisits.Birth_Date_Time), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = countypop.comp.Birth_Date_Time, group = "Counties") %>%
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
