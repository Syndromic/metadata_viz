# Install required packages

if("tigris" %in% rownames(installed.packages()) == FALSE) {install.packages("tigris", repos='http://cran.us.r-project.org')};
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet", repos='http://cran.us.r-project.org')};
if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio", repos='http://cran.us.r-project.org')};
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr", repos='http://cran.us.r-project.org')};
if("magrittr" %in% rownames(installed.packages()) == FALSE) {install.packages("magrittr", repos='http://cran.us.r-project.org')};
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr", repos='http://cran.us.r-project.org')};
if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny", repos='http://cran.us.r-project.org')};
if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny", repos='http://cran.us.r-project.org')};

# Run app
library(shiny)
runApp("Metadata_Viz.R")
