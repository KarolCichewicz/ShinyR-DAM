sidebarLayout(
  
  sidebarPanel(width=3,
               
  
  fluidRow(
                 
    column(6,
        radioButtons('per_profile', 'Conditions on a plot',
                       c(Overlap ="Overlapping", Split ="Nonoverlapping"),
                          'Overlapping')
                 ),
                 
    column(6,
        radioButtons('display_error_bars_per', 'Display SEM error bars',
                                     c("Yes" ="Yes", "No" ="No"),
                                     'Yes')
                 )
    
  ),                            
  
  tags$hr(),
  
  sliderInput("Period_range", label = "Chi-Sq period testing range [h]", min = 5, 
              max = 55, value = c(18, 30)),
  
  
  
  sliderInput("Period_resolution", label = "Chi-Sq period testing resolution [h]", min = 0.05, 
              max = 1, value = 0.2),       
  
  
  
  sliderInput("rhythmicity_threshold", label = "Rhythmicity threshold [Qp.act/Qp.sig]", min = 0, 
              max = 3, value = 1, step = 0.1),
  
  tags$hr(),
  
  sliderInput("per_height", "Plot height [pixel]", min=10, max=1000, value=400, step = NULL, round = FALSE,
              format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  sliderInput("per_width", "Plot width [pixel]", min=400, max=2000, value=1400, step = NULL, round = FALSE,
              format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  
  
  tags$head(tags$style(HTML('#refresh_3{background-color:#67EB5E}'))),
  actionButton("refresh_3", "Calculate Periodograms"),
  
  tags$br(),
  tags$br(),
  
  fluidRow(downloadLink("download_mean_period_by_condition_rhythmic", "Download mean periodogram data.csv")),
  fluidRow(downloadLink("download_individual_period_peaks_alive_rhythmic", "Download individual fly period peak data.csv"))
  
  ),
  
  
  
  
  
  mainPanel(
    
    conditionalPanel(
      # Displays the LD range only if LD or Both radio buttons are selected
      condition = "input.per_profile == 'Overlapping'",
      
      fluidPage(align="center",
                tags$h2("Mean periodograms"),
                uiOutput("per_profiles")
      )),
    
    
    conditionalPanel(
      # Displays the LD range only if LD or Both radio buttons are selected
      condition = "input.per_profile == 'Nonoverlapping'",
      
      fluidPage(align="center",
                tags$h2("Mean periodograms"),
                uiOutput("per_profiles_split")
      )),
    
    
    
    fluidPage(align="center",
              tags$h3("Mean periodogram period peaks", align = "center"),
              column(4,
                     tableOutput("period_peaks"))
    ), 
    
  tags$hr(),
  tags$hr(),
    
    fluidPage(align="center",
              tags$h2("Individual periodograms"),
              plotOutput("individual_periodograms")
    ),
    
    
    tags$br(),
    tags$hr(),
    
    
    fluidPage(
      column(6,
             plotOutput("ind_period_peaks")
      ),
      column(6,
             plotOutput("ind_period_strength")
      )
    ),
  
  
  fluidPage(
    tags$h2("Summary of circadian period peaks"),
    column(8,
           tableOutput("mean_of_period_peaks_by_condition")
    )
  )
  )
)