
sidebarLayout(
  
  sidebarPanel(width = 3,
               
               fluidRow(
                column(6,
               radioButtons('act_profile', 'Conditions on a plot',
                            c(Overlap ="Overlapping", Split ="Nonoverlapping"),
                            'Overlapping')),
               column(6,
               radioButtons('display_error_bars', 'Display SEM error bars',
                            c("Yes" ="Yes", "No" ="No"),
                            'No')
                )),
               
               
               fluidRow(
                 column(6,
                        radioButtons('date_disp', 'Display dates on a plot',
                                     c("Yes" ="Yes", "No" ="No"),
                                     'Yes')
                 )),
               
          
               radioButtons('act_scale', 'X-axis average activity profile time scale',
                            c("Data acquisition time" ="Data_acq", "Zeitgeber time" ="Zeitgeber"),
                            'Data_acq'),
               
               tags$hr(),
               
               
               sliderInput("act_profile_window", "Bin activity profile data points into average [min]", min=1, max=120, value=30, step = NULL, round = FALSE,
                           ticks = TRUE, animate = FALSE,
                           width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                           timezone = NULL, dragRange = TRUE),
               
               sliderInput("ac_profile_y_lim", "Max number of counts displayed", 1, 20, 4, step = NULL, round = FALSE,
                           ticks = TRUE, animate = FALSE,
                           width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                           timezone = NULL, dragRange = TRUE),
               tags$hr(),
               
               sliderInput("ac_profile_height", "Plot height [pixel]", min=10, max=1000, value=400, step = NULL, round = FALSE,
                           ticks = TRUE, animate = FALSE,
                           width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                           timezone = NULL, dragRange = TRUE),
               
               
               sliderInput("ac_profile_width", "Plot width [pixel]", min=400, max=3000, value=1400, step = NULL, round = FALSE,
                           ticks = TRUE, animate = FALSE,
                           width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                           timezone = NULL, dragRange = TRUE),
               
               
               
               
               
               tags$head(
                 tags$style(HTML('#refresh_1{background-color:#67EB5E}'))
               ),     
               actionButton("refresh_1", "Plot Activity Profiles"),
               
               tags$br()
               
               
              
               
  ),
  
  conditionalPanel(
    condition = "input.refresh_1 !=0",
  
  mainPanel(align="center",
            fluidRow( 
              conditionalPanel(
                condition = "input.act_profile == 'Overlapping'",
                tags$h2("Daily activity profiles"),
                uiOutput('act_profiles_by_day')
              )),
            

            
            fluidRow(
              conditionalPanel(
                condition = "input.act_profile == 'Nonoverlapping'",
                tags$h2("Daily activity profiles"),
                uiOutput('act_profiles_by_day_split')
              )),
            
            fluidRow(
              column(6, align='left',
                     tags$head(tags$style(HTML('#download_daily_act_profiles{background-color:#FCE897}'))),  
                     downloadButton("download_daily_act_profiles", "Download daily activity profiles.csv")
              )),
            
            
            conditionalPanel(
              condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",
              
              fluidRow(
                conditionalPanel(
                  condition = "input.act_profile == 'Overlapping'",
                  tags$h2("Average activity profiles in LD"),   
                  uiOutput('act_profile_30_min_overlap')
                )),
              
              fluidRow(
                conditionalPanel(
                  condition = "input.act_profile == 'Nonoverlapping'",
                  tags$h2("Average activity profiles in LD"),   
                  uiOutput('act_profiles_average_split')
                )),
              
              
              fluidRow(
                column(6, align='left',
                       tags$head(tags$style(HTML('#download_average_act_profiles{background-color:#FCE897}'))),  
                       downloadButton("download_average_act_profiles", "Download average activity profiles in LD.csv")
                ))
              
           
              
            )
            
  )
  )
)