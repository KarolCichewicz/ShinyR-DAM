
  
sidebarLayout(
  
  sidebarPanel(width = 3,
               fluidRow(
                 
                 column(6,
                        radioButtons('Mean_median', 'Actogram statistic',
                                     c(Mean="Mean", Median ="Median", "Individual alive" = "Individual_alive", "Individual dead" = "Individual_dead"),
                                     'Mean')),
                 column(6,
                        radioButtons('Double_Single', 'Actogram type',
                                     c("Single Plotted"="SP", "Double Plotted" ="DP"),
                                     'SP'))
               ),
               
               tags$hr(),  
               
               
               sliderInput("actogram_bin", "Bin actogram data points into average [min]", min=1, max=30, value=5, step = NULL, round = FALSE,
                           ticks = TRUE, animate = FALSE,
                           width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                           timezone = NULL, dragRange = TRUE),
               sliderInput("max_act_value", "Max number of counts displayed", min=1, max=50, value=4),
               
               tags$hr(),
               
               
               sliderInput("actograms_height", "Actogram height [pixel]", min=10, max=600, value=100, step = NULL, round = FALSE,
                           ticks = TRUE, animate = FALSE,
                           width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                           timezone = NULL, dragRange = TRUE),
               
               
               sliderInput("actograms_width", "Actogram width [pixel]", min=400, max=2000, value=800, step = NULL, round = FALSE,
                           ticks = TRUE, animate = FALSE,
                           width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                           timezone = NULL, dragRange = TRUE),
               
               
               
               
               
               
               colourInput('actogram_color','Actogram color', value = "#0D226E"),
               
               tags$head(
                 tags$style(HTML('#refresh{background-color:#67EB5E}'))
               ),
               
               actionButton("refresh", "Plot Actograms"),
               
               
               
               conditionalPanel(
                 condition = "input.refresh != 0", 
                 
                 tags$hr(),   
               
                  fluidRow(
                        tags$head(tags$style(HTML('#download_mean_and_median_actogram_data{background-color:#FCE897}'))),  
                        downloadButton("download_mean_and_median_actogram_data", "Download mean and median actogram data.csv")
                 ),
                 
                 fluidRow(
                   tags$head(tags$style(HTML('#download_individual_actogram_data{background-color:#FCE897}'))),  
                   downloadButton("download_individual_actogram_data", "Download individual actogram data.csv")
                 ),
                 
                 tags$br(),
                 
                 fluidRow(
                   tags$head(tags$style(HTML('#download_list_of_alive_flies{background-color:#FCE897}'))),  
                   downloadButton("download_list_of_alive_flies", "Download a list of alive flies.csv")
                 ),
                 
                 fluidRow(
                   tags$head(tags$style(HTML('#download_list_of_dead_flies{background-color:#FCE897}'))),  
                   downloadButton("download_list_of_dead_flies", "Download a list of dead flies.csv")
                 )
                 
                 
                 
                 
                 
               )
               
           
               
  ),
  
  
  
  mainPanel(align="center",
            
            conditionalPanel(
              # Displays the ontout only when the Plot Actograms button is pressed
              condition = "input.refresh != 0",    
            
            conditionalPanel(
              # Displays the LD range only if LD or Both radio buttons are selected
              condition = "input.Mean_median == 'Mean'",       
              tags$h2("Mean Actograms", align = "center"), 
              
              uiOutput("Mean_act")
            ),
            
            conditionalPanel(
              # Displays the LD range only if LD or Both radio buttons are selected
              condition = "input.Mean_median == 'Median'",        
              tags$h2("Median Actograms", align = "center"),
              
              uiOutput("Median_act")),
        
          fluidPage(      
            conditionalPanel(
              # Displays the LD range only if LD or Both radio buttons are selected
              condition = "input.Mean_median == 'Individual_alive' || input.Mean_median == 'Individual_dead'",        
              tags$h2("Individual Actograms", align = "center"),
              
              uiOutput("Individual_act")
            )  )
          
     
            )
          
               
  )
)
