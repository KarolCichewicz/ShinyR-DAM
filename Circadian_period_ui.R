sidebarLayout(
  
  sidebarPanel(width=3,
  
    conditionalPanel(
      condition = "input.LDDD == 'LD'",
      
      tags$div(
        tags$b('Circadian Period Analysis is only available for DD'), 
        style = "font-size: 16px; color:SlateBlue")
      
      
                 ),
    
    conditionalPanel(
      condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
      tags$b('Only DD days are used in the Circadian Period Analysis'), 
      style = "font-size: 15px; color:SlateBlue",
      tags$br()
      ),
      
    
                 
  conditionalPanel(
  condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",        
  
  
  fluidRow(
    column(6,
        radioButtons('per_profile', 'Mean periodograms',
                       c("Do not display" = "Do_not_display", "Overlap conditions" ="Overlapping", "Split conditions" ="Nonoverlapping"),
                          'Do_not_display')
                 ),

    
    conditionalPanel(
      condition = "input.per_profile == 'Overlapping' || input.per_profile == 'Nonoverlapping'", 
    column(6,
        radioButtons('display_error_bars_per', 'Display SEM error bars',
                                     c("Yes" ="Yes", "No" ="No"),
                                     'Yes')
                 )
    )
    
    
  ),  
  
  
  fluidRow(
    column(8,
           radioButtons('per_profile2', 'Individual periodograms',
                        c("Do not display" = "Do_not_display", "Display individual periodograms" ="Nonoverlapping", "Group by condition" = "Overlapping"),
                        'Do_not_display')
    )
    
  ),
  
  
  tags$hr(),
  
  sliderInput("Period_range", label = "Chi-Sq period testing range [h]", min = 5, 
              max = 55, value = c(18, 30)),
  
  
  
  sliderInput("Period_resolution", label = "Chi-Sq period testing resolution [h]", min = 0.05, 
              max = 1, value = 0.2),       
  
  
  
  sliderInput("rhythmicity_threshold", label = "Rhythmicity threshold for filtering arhythmic individuals [Qp.act/Qp.sig]", min = 0, 
              max = 3, value = 1, step = 0.1),
  
  tags$hr(),
  
  sliderInput("per_height", "Plot height [pixel]", min=10, max=1000, value=400, step = NULL, round = FALSE,
              ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  sliderInput("per_width", "Plot width [pixel]", min=400, max=2000, value=1400, step = NULL, round = FALSE,
              ticks = TRUE, animate = FALSE,
              width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
              timezone = NULL, dragRange = TRUE),
  
  
  
  
  tags$head(tags$style(HTML('#refresh_3{background-color:#67EB5E}'))),
  actionButton("refresh_3", "Calculate Periodograms")
  
               )
  
  ),
  
  
  
  
  
  mainPanel(
    
    conditionalPanel(
      condition = "input.per_profile == 'Overlapping' && input.refresh_3 != 0",
      
      fluidPage(align="center",
                tags$h2("Mean periodograms"),
                uiOutput("per_profiles")
      )),
    
    
    conditionalPanel(
      condition = "input.per_profile == 'Nonoverlapping' && input.refresh_3 != 0",
      
      fluidPage(align="center",
                tags$h2("Mean periodograms"),
                uiOutput("per_profiles_split")
      )),
 
    
    conditionalPanel(
      condition = "(input.per_profile == 'Nonoverlapping' || input.per_profile == 'Overlapping') && input.refresh_3 != 0",
    fluidRow(align='left',
             tags$head(tags$style(HTML('#download_mean_period_by_condition_rhythmic{background-color:#FCE897}'))),
             downloadButton("download_mean_period_by_condition_rhythmic", "Download mean periodograms data.csv"))
    
    ),
       
    
    
    
    conditionalPanel(
      condition = "(input.per_profile == 'Overlapping' || input.per_profile == 'Nonoverlapping') && input.refresh_3 != 0",
      
    fluidPage(align="center",
              tags$h4("Mean periodograms period peaks as displayed on the plot above", align = "left"),
              column(4,
                     tableOutput("period_peaks"))
    )), 
    

  tags$br(),
  
  
  conditionalPanel(
    # Displays the LD range only if LD or Both radio buttons are selected
    condition = "input.per_profile2 == 'Overlapping' && input.refresh_3 != 0",
    
    fluidPage(align="center",
              tags$h2("Individual periodograms"),
              plotOutput("individual_periodograms")
    )),
  
  
  conditionalPanel(
    condition = "input.per_profile2 == 'Nonoverlapping' && input.refresh_3 != 0",
    
    fluidPage(align="center",
              tags$h2("Individual periodograms"),
              uiOutput("ind_per2")
    )),

  
  
  conditionalPanel(
    # Displays the LD range only if LD or Both radio buttons are selected
    condition = "(input.per_profile2 == 'Nonoverlapping' || input.per_profile2 == 'Overlapping') && input.refresh_3 != 0",
    fluidRow(align='left',
             tags$head(tags$style(HTML('#download_individual_periodograms_alive_rhythmic{background-color:#FCE897}'))),
             downloadButton("download_individual_periodograms_alive_rhythmic", "Download individual periodograms data.csv"))
    
  ),
  
  
  
  
  
    
    tags$br(),

    
    
    fluidPage(
      column(6,
             plotOutput("ind_period_peaks")
      ),
      column(6,
             plotOutput("ind_period_strength")
      )
    ),
  
  
  conditionalPanel(
    condition = "input.refresh_3 != 0",
    
  tags$head(tags$style(HTML('#download_individual_period_peaks_alive_rhythmic{background-color:#FCE897}'))),  
  fluidRow(downloadButton("download_individual_period_peaks_alive_rhythmic", "Download individual fly period peak data.csv"))
  ),
  
  fluidPage(
    conditionalPanel(
      # Displays the LD range only if LD or Both radio buttons are selected
      condition = "input.refresh_3 != 0",
    
    
    column(8,
           tags$h4("Statistics of individual periodograms period peaks", align = "left"),
           tableOutput("mean_of_period_peaks_by_condition")
    )
  ))
  )
)
