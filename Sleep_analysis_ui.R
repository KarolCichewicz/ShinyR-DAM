# Nothing will be displayed in this tab if recording frequency is other than 1 min
sidebarLayout(
  
  sidebarPanel(width = 3,


conditionalPanel(
  condition = "input.data_recording_frequency == 1",
  
  
  fluidRow(
    
    column(6,
           radioButtons('sl_profile', 'Conditions on a plot',
                        c(Overlap ="Overlapping", Split ="Nonoverlapping"),
                        'Overlapping')
    ),
    
    column(6,
           radioButtons('display_error_bars_sl', 'Display SEM error bars',
                        c("Yes" ="Yes", "No" ="No"),
                        'No')
    )
    
  ),
  
  radioButtons('sl_scale', 'X-axis average sleep profile time scale',
               c("Data acquisition time" ="Data_acq", "Zeitgeber time" ="Zeitgeber"),
               'Data_acq'),
  
  
  
  tags$hr(),
  
  
  sliderInput("sleep_profile_window", "Averaging window of sleep profiles [min]", 
              min=1, max=120, value=30, step = NULL, round = FALSE,
              format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  
  sliderInput("max_slp_value", "Max value of sleep displayed", min=0, max=1.2, value=1.2),
  
  tags$hr(),
  
  sliderInput("sl_profile_height", "Plot height [pixel]", min=10, max=1000, value=400, step = NULL, round = FALSE,
              format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  sliderInput("sl_profile_width", "Plot width [pixel]", min=400, max=3000, value=1400, step = NULL, round = FALSE,
              format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  tags$head(
    tags$style(HTML('#refresh_2{background-color:#67EB5E}'))
  ),     
  actionButton("refresh_2", "Plot Sleep Profiles"),
  
  
  tags$br(),
  tags$br(),
  tags$br(),
  

  fluidRow(downloadLink("downloadSleep_daily", "Download daily sleep profile data.csv")),
  
  fluidRow(downloadLink("downloadSleep_profiles", "Download average sleep profile data.csv")),
  
  fluidRow(downloadLink("downloadSleep_ind_flies", "Download individual fly day night sleep data.csv"))
  
  
  
  
  
  
  
)), 

mainPanel(align="center",
          
          fluidPage(align="center",
                    conditionalPanel(
                      condition = "input.sl_profile == 'Overlapping'",
                      tags$h2("Daily sleep profiles"),
                      uiOutput("sl_profiles_by_day")
                    )
          ),
          
          fluidPage(align="center",
                    conditionalPanel(
                      condition = "input.sl_profile == 'Nonoverlapping'",
                      tags$h2("Daily sleep profiles"),
                      uiOutput("sl_profiles_by_day_split")
                    )
          ),
          
          
          conditionalPanel(
            condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",         
            
            fluidPage(align="center",
                      conditionalPanel(
                        condition = "input.sl_profile == 'Overlapping'",
                        tags$h2("Average sleep profiles in LD"),
                        uiOutput("average_LD_sleep_profiles")
                      )
            ),
            
            
            fluidPage(align="center",
                      conditionalPanel(
                        condition = "input.sl_profile == 'Nonoverlapping'",
                        tags$h2("Average sleep profiles in LD"),
                        uiOutput("average_LD_sleep_profiles_split")
                      )
            )
          ),
          
          
          
          
          
          
          fluidPage(align="center",
                    tags$br(),
                    
                    column(4,
                           plotOutput("Daytime_sleep")
                    ),
                    
                    column(2, ""),
                    
                    column(4,
                           plotOutput("Nighttime_sleep")
                    )
                    
          ),
          
          fluidPage(align="center",
                    
                    column(4,
                           tableOutput("sleep_per_cond_Day")
                    ),
                    
                    column(2, ""),
                    
                    column(4,
                           tableOutput("sleep_per_cond_Night")
                    )
                    
          )
)
)