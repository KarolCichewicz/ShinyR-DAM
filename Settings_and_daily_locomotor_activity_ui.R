      
fluidPage(  
  fluidRow(
    column(12, 
           tags$h1("ShinyR-DAM", align = "center"),
           tags$h4(div(HTML("v3.0 <q>Noble <i>Drosophila</i>")), align="center"),
           tags$h3("Program analyzing locomotor activity in TriKinetics Drosophila Activity Monitor System", align = "center"),
           
           tags$h4("Karol Cichewicz, Jay Hirsh laboratory, University of Virginia, Charlottesville, VA", align = "center"),
           
           tags$h4("", align = "center"),
           
           #tags$div(
           #   tags$p('If you have and comments or suggestions please email Karol at: kc3fb@virginia.edu.
           #          ', align = "center"), 
           #  style = "font-size: 16px;"),
           
           
           fileInput('file1', 'Upload Monitor files', multiple = TRUE,
                     accept=c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.csv'))
    )),
  
  wellPanel(
    fluidRow(
      column(3, 
             numericInput("Number_of_conditions", "Number of Conditions", 1, min = 1, max = NA, step = 1,
                          width = NULL)
      )),
    
    fluidRow(
      # Condition names input 
      column(3,
             uiOutput("Conditions_input")
      ),
      
      column(3, 
             uiOutput("Number_of_flies_per_cond")
      ),
      
      # Condition colors
      column(3,
             uiOutput("Colour_selection")
      ),
      
      column(3,
             uiOutput("Order_monitors")
      )
    )),    
  
  
  wellPanel(  
    fluidRow(
      column(2,
             radioButtons('LDDD', 'LD DD analysis',
                          c(LD ="LD",DD ="DD", Both ="Both"),
                          'Both')
      ),
      
      column(4,
             conditionalPanel(
               # Displays the LD range only if LD or Both radio buttons are selected
               condition = "input.LDDD == 'LD' || input.LDDD == 'Both'",     
               dateRangeInput('dateRange',
                              label = 'Date range of LD',
                              format = "dd-mm-yyyy",
                              start = Sys.Date()-10, end = Sys.Date()-6,
                              separator = " - "
                              )
             ),
             
             conditionalPanel(
               # Displays the DD range only if DD or Both radio buttons are selected
               condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
               dateRangeInput('dateRange1',
                              label = 'Date range of DD',
                              format = "dd-mm-yyyy",
                              start = Sys.Date()-5, end = Sys.Date()-1,
                              separator = " - "
                              
                              )
             )  
      ),
      
      column(3,
             numericInput("data_recording_frequency", "DAM system data acquisition frequency [min]", 1, min = 1, max = NA, step = 1,
                          width = NULL)
             
             
            
      ),
      
      column(3,
             numericInput("dead_fly_counts_threshold", "Threshold of counts per day for identifying dead flies", 50, min = 0, max = NA, step = 1,
                          width = NULL),
             
             selectInput("light_onset_time", "Light onset time", 
                         c("00:00","01:00","02:00","03:00","04:00","05:00","06:00", 
                           "07:00", "08:00","09:00","10:00","11:00","12:00","13:00",
                           "14:00","15:00", "16:00","17:00","18:00","19:00","20:00",
                           "21:00","22:00","23:00"), selected = "06:00", multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL)
             
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
  conditionalPanel(
    condition = "input.go != 0", 
    
  
  tags$br(),
  
  wellPanel(
    
    fluidRow(
      
    column(7,
    tags$h3("Data and settings validation")),
    
    column(4,
           tags$h3("Condition-Monitor layout")       
           
           )
    
    ),
        
    fluidRow(
      
      # Coloring output text doesn't work as planned. I intended to color the text in red if it prints an error.    
      # tags$style(type='text/css', ifelse(length(as.character(textOutput('Monitor_data_codes')) < 100), 
      #                                   "#Monitor_data_codes { color: green;}", 
      #                                   "#Monitor_data_codes { color: red;}")),
      
      # Prints 4 error messages/ checkpoint outputs. 
      column(7,
             verbatimTextOutput('Monitor_data_codes'),
             
             verbatimTextOutput('Monitors_vs_conditions_check'),   
             
             verbatimTextOutput('LD_DD_overlap_check'),
             
             verbatimTextOutput('Data_range_correctness')
      ),
      
      
      # Monitor layout and file download
      
      
      column(4,
             
             tableOutput('Monitor_layout')
             )
             
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
    conditionalPanel(
      condition = "input.go != 0",
    
    column(4,
             
             tags$head(tags$style(HTML('#download_general_summary{background-color:#FCE897}'))),  
             fluidRow(align='center',
               downloadButton("download_general_summary", "Download daily locomotor activity data.csv"))
           ),
    column(6,
             fluidRow(align='center',
                      tags$head(tags$style(HTML('#download_summary_by_fly{background-color:#FCE897}'))),
                      downloadButton("download_summary_by_fly", "Download average individual daily locomotor activity data.csv"),
                      
                      tags$head(tags$style(HTML('#downloadActivity_per_Condition_per_fly_all{background-color:#FCE897}'))),
                      downloadButton("downloadActivity_per_Condition_per_fly_all", "Download individual fly daily activity.csv")
                      
                      )
           ) 
    )
  ),
  
  
  fluidRow(
   
  ),

  
  # Tables with LD and DD data
  
  fluidRow(
    column(5,
           conditionalPanel(
             condition = "(input.LDDD == 'LD' || input.LDDD == 'Both') && input.go !=0",
             tags$h3("Locomotor activity in LD"),
             tableOutput('general_summary_LD')
           )),
    column(1, ""),
    column(5,
           conditionalPanel(
             condition = "(input.LDDD == 'DD' || input.LDDD == 'Both') && input.go !=0",
             tags$h3("Locomotor activity in DD"),
             tableOutput('general_summary_DD')
           )
    )
  ),
    
  
  
  fluidRow(
    conditionalPanel(
      condition = "input.go != 0",
      tags$hr(),
    column(8,
           
           tags$h3("Locomotor activity by day"),
           
           plotOutput("point_plot_by_day"),
           
           tags$head(tags$style(HTML('#download_activity_by_day{background-color:#FCE897}'))),  
           fluidRow(align='left',
                    downloadButton("download_activity_by_day", "Download locomotor activity by day.csv"))
           
                    )
    )
  ),
  
 
  
  
  conditionalPanel(
    condition = "(input.LDDD == 'LD' || input.LDDD == 'Both') && input.go !=0",
    
    fluidRow(
      tags$hr(),
      column(12,
                    tags$h3("Daytime vs nighttime locomotor activity in LD"))
    ),
    
    fluidRow(
      
      column(6,
             plotOutput("Daytime_nighttime_activity_plot")),
      column(1, ""),
      column(4,
             plotOutput("Nighttime_to_daytime_act_ratio")),
      column(1,"")
    ),
    
    fluidRow(
      column(6, align='left',
      tags$head(tags$style(HTML('#download_daytime_nighttime_activity{background-color:#FCE897}'))),  
      downloadButton("download_daytime_nighttime_activity", "Download daytime vs nighttime activity.csv")
      ),
      column(1, ""),
      column(4, align='left',
      tags$head(tags$style(HTML('#download_nighttime_daytime_activity_ratio{background-color:#FCE897}'))),  
      downloadButton("download_nighttime_daytime_activity_ratio", "Download nighttime daytime activity ratio.csv")    
             ),
      column(1, "")
    ),
    
    fluidRow( 
      column(3,
             tableOutput("daytime_activity_table")),
      column(1,""),
      column(3,
             tableOutput("nighttime_activity_table")),
      column(1,""),
      column(3,
             tableOutput("n_d_ratio_table")),
      column(1, "")
    )
    
  )
  
)
