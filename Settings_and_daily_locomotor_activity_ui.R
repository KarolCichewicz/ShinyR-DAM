      
fluidPage(  
  fluidRow(
    column(12, 
           tags$h1("ShinyR-DAM", align = "center"),
           tags$h4(div(HTML("v2.1 <q>Daring <i>Drosophila</i>")), align="center"),
           tags$h3("Program analyzing locomotor activity in TriKinetics Drosophila Activity Monitor System", align = "center"),
           
           tags$h4("Karol Cichewicz, Jay Hirsh laboratory, University of Virginia, Charlottesville, VA", align = "center"),
           
           tags$h4("", align = "center"),
           
           tags$div(
             tags$p('If you have and comments or suggestions please email Karol at: kc3fb@virginia.edu.
                    ', align = "center"), 
             style = "font-size: 16px;"),
           
           
           fileInput('file1', 'Upload Monitor files', multiple = TRUE,
                     accept=c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.csv'))
    )),
  
  wellPanel(
    fluidRow(
      column(2, 
             numericInput("Number_of_conditions", "Number of Conditions", 1, min = 1, max = NA, step = 1,
                          width = NULL)
      )),
    
    fluidRow(
      # Condition names input 
      column(2,
             uiOutput("Conditions_input")
      ),
      
      column(2, 
             uiOutput("Number_of_flies_per_cond")
      ),
      
      # Condition colors
      column(2,
             uiOutput("Colour_selection")
      ),
      
      column(2,
             uiOutput("Order_monitors")
      )
    )),    
  
  
  wellPanel(  
    fluidRow(
      column(1,
             radioButtons('LDDD', 'LD DD analysis',
                          c(LD ="LD",DD ="DD", Both ="Both"),
                          'Both')
      ),
      
      column(2,
             conditionalPanel(
               # Displays the LD range only if LD or Both radio buttons are selected
               condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",     
               dateRangeInput('dateRange',
                              label = 'Date range of LD',
                              start = as.Date("2017-06-23"), end = as.Date("2017-06-27"))
             ),
             
             conditionalPanel(
               # Displays the DD range only if DD or Both radio buttons are selected
               condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
               dateRangeInput('dateRange1',
                              label = 'Date range of DD',
                              start = as.Date("2017-06-28"), end = as.Date("2017-07-02"))
             )  
      ),
      
      column(1,
             selectInput("light_onset_time", "Light onset time", 
                         c("00:00:00","01:00:00","02:00:00","03:00:00","04:00:00","05:00:00","06:00:00", 
                           "07:00:00", "08:00:00","09:00:00","10:00:00","11:00:00","12:00:00","13:00:00",
                           "14:00:00","15:00:00", "16:00:00","17:00:00","18:00:00","19:00:00","20:00:00",
                           "21:00:00","22:00:00","23:00:00"), selected = "06:00:00", multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL)
      ),
      
      column(3,
             numericInput("dead_fly_counts_threshold", "Threshold of counts per day for identifying dead flies", 50, min = 0, max = NA, step = 1,
                          width = NULL)
      ),
      
      column(3,
             numericInput("data_recording_frequency", "DAM system data acquisition frequency [min]", 1, min = 1, max = NA, step = 1,
                          width = NULL)
      )
    ) 
  ),
  
  fluidRow(
    column(3, 
           
           #Action Button - Start Analysis
           tags$head(
             tags$style(HTML('#go{background-color:#67EB5E}'))
           ),
           actionButton("go", "Start Analysis")
    )
  ),
  
  
  ######## Error messages OUTPUT #################
  tags$h4("Data and settings validation"),
  
  wellPanel(
    fluidRow(
      
      # Coloring output text doesn't work as planned. I intended to color the text in red if it prints an error.    
      # tags$style(type='text/css', ifelse(length(as.character(textOutput('Monitor_data_codes')) < 100), 
      #                                   "#Monitor_data_codes { color: green;}", 
      #                                   "#Monitor_data_codes { color: red;}")),
      
      # Prints 4 error messages/ checkpoint outputs. 
      column(7,
             verbatimTextOutput('Monitor_data_codes'),
             
             verbatimTextOutput('Monitors_vs_conditions_check'),   
             
             conditionalPanel(
               # Displays the LD DD ocerlap check only if Both ranges are selected
               condition = "input.LDDD == 'Both'",    
               verbatimTextOutput('LD_DD_overlap_check')),
             
             verbatimTextOutput('Data_range_correctness')
      ),
      
      
      # Monitor layout and file download
      column(4,
             tags$strong(tags$p("Condition/Monitor layout")),
             
             tableOutput('Monitor_layout'),
             
             fluidRow(downloadLink("downloadActivity_per_Condition_per_fly_all", "Download individual fly daily activity.csv")),
             fluidRow(downloadLink("download_Summary_by_fly_alive", "Download individual fly activity in LD DD.csv")),
             fluidRow(downloadLink("download_D_N_individual_activity", "Download individual fly daytime and nighttime activity.csv"))
      )
    )
  ),
  
  
  ########################### OUTPUT PLOTS  ############################
  
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",
             plotOutput('bar_plot_LD')
           )
    ),
    
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",
             plotOutput('box_plot_LD')
           )
    ),
    
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",
             plotOutput('density_plot_LD')
           )
    )
  ),
  
  
  
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
             plotOutput('bar_plot_DD')
           )
    ),
    
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
             plotOutput('box_plot_DD')
           )
    ),
    
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
             plotOutput('density_plot_DD')
           )
    )
  ),
  
  fluidRow(
    column(8,
           tags$hr(),
           tags$h3("Locomotor activity by day"),
           
           plotOutput("point_plot_by_day"),
           
           tags$hr()
    )
  ),
  
  # Tables with LD and DD data
  
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",
             tags$h3("Locomotor activity per condition in LD"),
             tableOutput('general_summary_LD')
           )),
    column(1, ""),
    column(4,
           conditionalPanel(
             condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
             tags$h3("Locomotor activity per condition in DD"),
             tableOutput('general_summary_DD')
           )
    )
  ),
  
  tags$hr(),
  
  conditionalPanel(
    condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",
    
    fluidRow(column(12,
                    tags$h3("Nighttime Daytime locomotor activity in LD"))
    ),
    
    fluidRow(
      
      column(4,
             plotOutput("Daytime_activity")),
      column(4,
             plotOutput("Nighttime_activity")),
      column(4,
             plotOutput("Nighttime_to_daytime_act_ratio"))
    ),
    
    fluidRow( 
      column(4,
             tableOutput("daytime_activity_table")),
      column(4,
             tableOutput("nighttime_activity_table")),
      column(4,
             tableOutput("n_d_ratio_table"))
    )
    
  )
  
)