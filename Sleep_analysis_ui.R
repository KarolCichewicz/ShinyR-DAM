# Nothing will be displayed in this tab if recording frequency is other than 1 min
sidebarLayout(
  
  
  sidebarPanel(width = 3,
               
               conditionalPanel(
                 condition = "input.data_recording_frequency != 1",
                 tags$div(
                   tags$b('Sleep analysis requires DAM system data acquisition frequency = 1 min. 
                 Please make sure your dataset meets this requirement.
                          '), 
                   style = "font-size: 17px; color:SlateBlue")
                 
               ),
          
          conditionalPanel(
            condition = "input.LDDD == 'DD'",
            
            
            tags$div(
              tags$b('Advanced sleep statistics are available only for LD
                     '), 
              style = "font-size: 17px; color:SlateBlue")
            
            ),
          
               
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
  
  fluidRow(
    column(6,
           radioButtons('date_disp_1', 'Display dates on a plot',
                        c("Yes" ="Yes", "No" ="No"),
                        'Yes')
    )),
  
  
  radioButtons('sl_scale', 'X-axis average sleep profile time scale',
               c("Data acquisition time" ="Data_acq", "Zeitgeber time" ="Zeitgeber"),
               'Data_acq'),
  
  
  
  tags$hr(),
  
  
  sliderInput("sleep_profile_window", "Bin sleep profile data points into average [min]", 
              min=1, max=120, value=30, step = NULL, round = FALSE,
              ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  
  sliderInput("max_slp_value", "Max value of sleep displayed", min=0, max=1.2, value=1.2),
  
  tags$hr(),
  
  sliderInput("sl_profile_height", "Plot height [pixel]", min=10, max=1000, value=400, step = NULL, round = FALSE,
              ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  sliderInput("sl_profile_width", "Plot width [pixel]", min=400, max=3000, value=1400, step = NULL, round = FALSE,
              ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  tags$head(
    tags$style(HTML('#refresh_2{background-color:#67EB5E}'))
  ),     
  actionButton("refresh_2", "Plot Sleep Profiles"),
  
  
  tags$br()
  
  
  
)
  
  
  
  
), 

mainPanel(align="center",
  
          conditionalPanel(
            condition = "input.refresh_2 != 0",        
                  
          fluidRow(align="center",
                    conditionalPanel(
                      condition = "input.sl_profile == 'Overlapping'",
                      tags$h2("Daily sleep profiles"),
                      uiOutput("sl_profiles_by_day")
                    )
          ),
          
          fluidRow(align="center",
                    conditionalPanel(
                      condition = "input.sl_profile == 'Nonoverlapping'",
                      tags$h2("Daily sleep profiles"),
                      uiOutput("sl_profiles_by_day_split")
                    )
          ),
          
          
          fluidRow(
            column(6, align='left',
                   tags$head(tags$style(HTML('#download_daily_sleep{background-color:#FCE897}'))),  
                   downloadButton("download_daily_sleep", "Download daily sleep profile data.csv")
            )
            
            ),
          
          
          
  conditionalPanel(
    condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",         
            
            fluidRow(align="center",
                      conditionalPanel(
                        condition = "input.sl_profile == 'Overlapping'",
                        tags$h2("Average sleep profiles in LD"),
                        uiOutput("average_LD_sleep_profiles")
                      )
            ),
            
            
            fluidRow(align="center",
                      conditionalPanel(
                        condition = "input.sl_profile == 'Nonoverlapping'",
                        tags$h2("Average sleep profiles in LD"),
                        uiOutput("average_LD_sleep_profiles_split")
                      )
            ),
          
          
          
          fluidRow(
            column(6, align='left',
                   tags$head(tags$style(HTML('#download_average_sleep_profiles_in_LD{background-color:#FCE897}'))),  
                   downloadButton("download_average_sleep_profiles_in_LD", "Download average sleep profile in LD data.csv")
            )),
          
          
               
          
          fluidRow(align="center",
                    tags$br(),
                    
                    column(8,
                           plotOutput("Total_sleep")
                    )),
          
          
          fluidRow(align="center",
                    tags$br(),
                    
                    column(5,
                           tableOutput("Total_sleep_table")
                    ),
                    
                    column(7, "")
                    
                    ),
          
          
          fluidRow(align="center",
                    tags$br(),
                    tags$hr(),
                    
                    column(6,
                           plotOutput("Daytime_sleep")
                    ),
                    
                   
                    
                    column(6,
                           plotOutput("Nighttime_sleep")
                    )
                    
          ),
    
    
    
      
      
      
    
    
          
          fluidRow(align="center",
                    
                    column(5,
                           tableOutput("sleep_per_cond_Day")
                    ),
                    
                    column(2, ""),
                    
                    column(5,
                           tableOutput("sleep_per_cond_Night")
                    )
                  
                   
                    
          ),
    
          fluidRow(
              column(6, align='left',
              tags$head(tags$style(HTML('#download_ind_day_night_sleep{background-color:#FCE897}'))),  
              downloadButton("download_ind_day_night_sleep", "Download individual fly day and night sleep data.csv")) 
                ),
          
          
          
          fluidRow(align = "center",
                    tags$hr(),
                    tags$hr(),
                    column(6,
                           plotOutput("bout_length_sleep_plot")),
                    column(6,
                           plotOutput("bout_number_sleep_plot"))
            
            
          ),
          
          
          
          fluidRow(align="center",
                    column(6,
                           tableOutput("Bout_length_sleep_table")
                           ),
                    column(6,
                           tableOutput("Bout_number_sleep_table"))
          ),
          
          
          
          
          fluidRow(align = "center",
                    tags$br(),
                    column(6,
                           plotOutput("bout_length_activity_plot")),
                    column(6,
                           plotOutput("bout_number_activity_plot"))
                    
                    
          ),
          
          
          
          fluidRow(align="center",
                    column(6,
                           tableOutput("Bout_length_activity_table")),
                    column(6,
                           tableOutput("Bout_number_activity_table"))
                           
                    
          ),
          
          fluidRow(align="center",
                    column(6,
                           plotOutput("sleep_activity_lengths_plot")
                           ),
                    column(6,
                           plotOutput("sleep_activity_ratios_plot"))
                    ),
          
          fluidRow(align="left",
                    column(6,
                           tableOutput("sleep_activity_length_table")
                           ),
                    column(6,
                           tableOutput("sleep_activity_length_ratio_table"))
                    
                    ),
    
      fluidRow(
        column(6, align='left',
             tags$head(tags$style(HTML('#download_slp_act_bout_data{background-color:#FCE897}'))),  
             downloadButton("download_slp_act_bout_data", "Download individual sleep activity bout LD data.csv")
      )
    )
    
          )
          )
          
          
)
)