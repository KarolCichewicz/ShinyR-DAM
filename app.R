# ShinyR-DAM shiny application for analyzing data recorded by Trikinetics Drosophila Activity Monitor System
# Code developed by Karol Cichewicz, Jay Hirsh lab, University of Virginia, Charlottesville

# You can run the application by clicking
# the 'Run App' button above.

# Loads required libraries

library(shiny)
library(plyr)
library(ggplot2)
library(dplyr)
library(zoo)
library(gtools)
library(scales)
library(gridExtra)
library(data.table)
library(Kmisc)
library(lubridate)
library(colourpicker)
library(grid)


# Define UI for the application
ui <- fluidPage(
  
  # This blocks printing any errors in the Shiny UI. These errors are mostly uninterpretable to a user anyway. 
  # Use with Caution, disable during development :) 
 tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tabsetPanel(
    
    tabPanel(
      h4("Settings and Daily Locomotor Activity", style = "color: #2750D6;"),
      
      #Java script for Google Analytics - tracks the app usage recording the localization of the app launches.
      tags$head(includeScript("google-analytics.js")),       
    
  source("Settings_and_daily_locomotor_activity_ui.R", local=TRUE)[1]
          ),

### Activity Profiles ###

tabPanel(h4("Activity Profiles", style = "color: #2750D6;"),
         tags$br(),
         source("Activity_profiles_ui.R", local = TRUE)[1]
),


## Sleep analysis ## 

tabPanel(h4("Sleep Analysis", style = "color: #2750D6;"),
         tags$br(),
         source("Sleep_analysis_ui.R", local = TRUE)[1]
),


#### Actograms ###
tabPanel(h4("Actograms", style = "color: #2750D6;"),
         tags$br(),           
         source("Actograms_ui.R", local = TRUE)[1]
),




#### Circadian Period Analysis ###
tabPanel(h4("Circadian Period Analysis", style = "color: #2750D6;"),
         tags$br(),
         
         source("Circadian_period_ui.R", local=TRUE)[1]         
),

### Documentation ###

tabPanel(h4("Documentation", style = "color: #2750D6;"),
         
         source("Documentation.R", local = TRUE)[1]
)
)
)



###################################   SERVER   ###################################

# Define server logic
server <- function(input, output) {
  
  # Sets max file upload to 300 MB. Default is 5 MB.
  options(shiny.maxRequestSize=300*1024^2)
  
  
  
  ### Initial Data pre-processing, including error messages ###
  source("Settings_and_daily_locomotor_activity_server.R", local = TRUE)   
  
  
  ### Daily locomotor activity analysis plots ###   
  source("Daily_locomotor_activity_plots_server.R", local = TRUE) 
  
  
  ### Activity Profiles ###
  source("Activity_profiles_server.R", local = TRUE)
  
  
  ### Sleep analysis ###
  
  source("Sleep_analysis_server.R", local = TRUE)    
  
  
  ### Actograms ####
  source("Actograms_server.R", local = TRUE)    
  
  
  #### Circadian Period analysis ####
  source("Circadian_period_server.R", local = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

