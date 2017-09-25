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


# Define UI for the application


ui <- fluidPage(
  

  # This blocks printing any errors in the Shiny UI. These errors are mostly uninterpretable to a user anyway. 
  # Use with Caution, disable during development :) 
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tabsetPanel(
  tabPanel(
    
  h4("Settings and Daily Locomotor Activity Analysis", style = "color: #2750D6;"),
  
  #Java script for Google Analytics - tracks the app usage recording the localization of the app launches.
  tags$head(includeScript("google-analytics.js")),             
             
  fluidRow(
  column(12, 
  tags$h1("ShinyR-DAM", align = "center"),
  tags$h3("Program analyzing locomotor activity in TriKinetics Drosophila Activity Monitor system", align = "center"),
  tags$h4("Karol Cichewicz, Jay Hirsh laboratory, University of Virginia, Charlottesville, VA", align = "center"),
  
  tags$h4("", align = "center"),
  
  tags$div(
    tags$p('If you encounter any problems, have comments or suggestions, please email Karol at: kc3fb@virginia.edu. Please include ShinyR-DAM in the title.
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

  fluidRow(downloadLink("downloadActivity_per_Condition_per_fly_all", "Download individual fly activity by day.csv")),
  fluidRow(downloadLink("download_Summary_by_fly_alive", "Download individual fly activity by light cycle.csv"))
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
      tags$h3("Locomotor activity per Condition in LD"),
      tableOutput('general_summary_LD')
                      ), 
    
    
      conditionalPanel(
      condition = "input.LDDD == 'DD' || input.LDDD == 'Both'",
      tags$h3("Locomotor activity per Condition in DD"),
      tableOutput('general_summary_DD')
                      )
            )
  )
),

### LD Activity Profiles ###

tabPanel(h4("LD Activity Profiles", style = "color: #2750D6;"),
         
  tags$h2("LD Activity Profiles"),

  tags$br(),
  
  fluidRow(
    
# Averaging window of activity profiles
      column(3,
            selectInput("act_profile_window", "Averaging window of activity profiles [min]", choices = c("1", "2", "3", "4", "5", "6", "8", "10", "15", "30", "60", "120", "1440"), selected = "30",
                               width = NULL, multiple = FALSE, selectize = TRUE, size = NULL)),
      
# Max number of counts displayed      
      column(3, 
             sliderInput("ac_profile_y_lim", "Max number of counts displayed", 1, 20, 4, step = NULL, round = FALSE,
                         format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                         width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                         timezone = NULL, dragRange = TRUE)
             ),
            
      column(3,
        fluidRow(downloadLink("downloadActivity_profiles", "Download activity profiles data.csv")),
        fluidRow(downloadLink("download_D_N_individual_activity", "Download individual fly daytime and nighttime activity.csv"))
        ),
      
      column(3,
      tags$head(
        tags$style(HTML('#refresh_1{background-color:#67EB5E}'))
               ),     
        actionButton("refresh_1", "Plot Activity Profiles")
            )
    ),        
          
fluidPage(
  tags$h4("Time of day X axis"),   
           plotOutput('activity_profile_30_min')
         ),

fluidPage(
  tags$h4("Zeitgeber time X axis"),
      plotOutput('activity_profile_30_minZT')
         ),

tags$hr(),


fluidPage(
  column(4,
         plotOutput("Daytime_activity")
         ),
  column(4,
         plotOutput("Nighttime_activity")
         ),
  column(4,
         plotOutput("Nighttime_to_daytime_act_ratio")
         )
      ),
  
fluidPage(
    column(4,
           tableOutput("daytime_activity_table")
           ),
    column(4,
           tableOutput("nighttime_activity_table")
           ),
    column(4,
           tableOutput("n_d_ratio_table")
           )
        )
),


tabPanel(h4("LD Sleep Analysis", style = "color: #2750D6;"),
         
  tags$h2("LD Sleep analysis"),
  
  tags$h5("Requires DAM system data acquisition frequency = 1 min "),
  
# Nothing will be displayed in this tab if recording frequency is other than 1 min
  conditionalPanel(
    condition = "input.data_recording_frequency == 1",
  
  fluidRow(
    column(3,
      selectInput("sleep_profile_window", "Interval for average sleep profile [min]", 
                  choices = c("1", "2", "3", "4", "5", "6", "8", "10", "15", "30", "60", "120", "1440"), selected = "30",
                  width = NULL, multiple = FALSE, selectize = TRUE, size = NULL)
          ),
    
    column(3,
           sliderInput("max_slp_value", "Max value of sleep displayed", min=0, max=1, value=1)
           ),
           
    column(3,
          fluidRow(downloadLink("downloadSleep_profiles", "Download sleep profiles data.csv")),
            
          fluidRow(downloadLink("downloadSleep_ind_flies", "Download sleep data of individual flies.csv"))
          ),
    
    column(3,
          tags$head(
             tags$style(HTML('#refresh_2{background-color:#67EB5E}'))
                   ),     
           actionButton("refresh_2", "Plot Sleep Profiles")
          )
  ),
  

# Plots sleep profiles           
  fluidPage(
    tags$h4("Time of day X axis"),
    plotOutput("sleep_profiles")
           ),
         
  fluidPage(
    tags$h4("Zeitgeber time X axis"),
    plotOutput("sleep_profiles_ZT")
           ),
  
  
  fluidPage(
    tags$hr(),
    column(4,
      plotOutput("Daytime_sleep")
           ),
    
    column(4,
           plotOutput("Nighttime_sleep")
           )
  ),
  
  fluidPage(
    column(4,
           tableOutput("sleep_per_cond_Day")
           ),
    column(4,
           tableOutput("sleep_per_cond_Night")
           )
          )
 )
),


#### Actograms ###
tabPanel(h4("Actograms", style = "color: #2750D6;"),
 fluidPage(
  tags$br(),
  column(3,
    sliderInput("actograms_height", "Actogram height", min=10, max=200, value=100, step = NULL, round = FALSE,
                     format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                     width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                     timezone = NULL, dragRange = TRUE)     
        ),
  
  column(3,     
    sliderInput("actograms_width", "Actogram width", min=400, max=1200, value=800, step = NULL, round = FALSE,
                     format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
                     width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                     timezone = NULL, dragRange = TRUE)    
        ),
  
  column(3,
         sliderInput("max_act_value", "Max number of counts displayed", min=1, max=50, value=4)
         ),
  
  column(3, 
         colourInput('actogram_color','Actogram color', value = "#0D226E"),
         
         tags$head(
           tags$style(HTML('#refresh{background-color:#67EB5E}'))
         ),
         
         actionButton("refresh", "Plot Actograms")
      )
),
  
  
    
fluidPage(
  column(6,
    tags$h2("Median Actograms", align = "center"),
    
      uiOutput("Median_act")
    ),
  
  column(6,
    tags$h2("Mean Actograms", align = "center"),         
    uiOutput("Mean_act")
        )
)
),


#### Circadian Period Analysis ###
tabPanel(h4("Circadian Period Analysis", style = "color: #2750D6;"),
         tags$br(),
         
fluidPage(
  column(3, 
    sliderInput("Period_range", label = "Chi-Sq period testing range [h]", min = 5, 
                         max = 55, value = c(18, 30))
              ),
  
  column(3,
    sliderInput("Period_resolution", label = "Chi-Sq period testing resolution [h]", min = 0.05, 
                max = 1, value = 0.2)       
        ),
    
  column(3,
    sliderInput("rhythmicity_threshold", label = "Rhythmicity threshold [Qp.act/Qp.sig]", min = 0, 
                       max = 3, value = 1, step = 0.1)       
           ),
      
  column(3,
      tags$head(tags$style(HTML('#refresh_3{background-color:#67EB5E}'))),
      actionButton("refresh_3", "Calculate Periodograms"),
      
      tags$br(),
      tags$br(),
      
      fluidRow(downloadLink("download_mean_period_by_condition", "Download mean periodogram data.csv")),
      fluidRow(downloadLink("download_individual_period_peaks", "Download individual fly period peak data.csv"))
      )
    ),

fluidPage(
  fluidRow(
      tags$h3("Mean periodograms"),
      plotOutput("mean_periodograms")
      ),
      
  fluidRow(
      tags$h3("Peak period of mean periodograms"),
      column(4,
          tableOutput("period_peaks"))
          ) 
  ),
    
fluidPage(
  fluidRow(
    tags$h3("Individual periodograms"),
    plotOutput("individual_periodograms")
          )
  ),
    
tags$br(),
tags$hr(),

fluidPage(
  fluidRow(
    column(6,
      plotOutput("ind_period_peaks")
           ),
    column(6,
      plotOutput("ind_period_strength")
          )
    )),
    
fluidPage(
  fluidRow(
    tags$h3("Summary of circadian period peaks"),
      column(8,
        tableOutput("mean_of_period_peaks_by_condition")
            )
          )
    )
),

### Documentation ###

tabPanel(h4("Documentation", style = "color: #2750D6;"),

column(8,
  tags$body(
    h1(strong('General information')),
    br(),
    
    tags$div(
      tags$p('ShinyR-DAM analyzes locomotor activity, sleep and circadian rhythmicity data recorded by the TriKinetics Drosophila 
       Activity Monitor (DAM) system. It is designed to summarize individual fly data from the condition-monitor 
             channel layout set by the experimenter. Analyzed data is presented in plots, tables, and 
             csv files are available for download. These can be opened with a spreadsheet program for further statistical analysis.
             '), 
      style = "font-size: 21px;"),
    
    tags$div(
      tags$p('The design of this program is inspired by a classic circadian free run paradigm, 
       which consists of a few days of entrainment, synchronizing animals to a standard 
             24 h cycle of 12 h of day and 12 h of night (LD), followed by a period of constant darkness (DD), 
             allowing animals to express their circadian period of sleep and activity. However, having LD and DD days in a 
             dataset is not required by the program. Locomotor activity and other metrices can be measured 
             separately for LD and DD, but circadian period analysis is available only for DD days. 
             Activity profiles and sleep analysis are calculated only for LD days.
             '), 
      style = "font-size: 21px"),
  
    tags$br(),
    tags$div(
      tags$p(strong('How to cite ShinyR-DAM:'), "Karol Cichewicz and Jay Hirsh, University of Virginia (manuscript in preparation)"), 
      style = "font-size: 21px"),
    
   
    h3(strong('Contact information:')),
    tags$div(
    tags$p('Karol Cichewicz - kc3fb@virginia.edu, Jay Hirsh - jh6u@virginia.edu.'), 
      style = "font-size: 19px"),
    
  
    br(),  
      
  
    
    h2(strong('Settings and Daily Locomotor Activity Analysis')),
    
    
    h3(strong('Program usage')),
    
    tags$div(
      tags$p('ShinyR-DAM is hosted on the shinyapps.io server with limited total time of usage to 500 hours per month for all users.
             Because of this limit we restrict program idle time to 15 min. 
             Please make sure that you saved your data before 15 min of ShinyR-DAM inactivity, or your analysis will be lost.'),
      style = "font-size: 19px"),
    
    h3(strong('Uploading monitor files')),
    
    tags$div(
      tags$p('ShinyR-DAM accepts DAM system monitor files recorded by the DAMSystem3 data acquisition software. 
          If you still use the DAMSystem2 acquisition software, which 
          records individual channel files, please use our channel to monitor file converter:'),
      style = "font-size: 19px"),
  
    tags$div(
      tags$a(href="https://karolcichewicz.shinyapps.io/DAM2_to_DAM3_converter/", 
             " DAMSystem2-Channel to DAMSystem3-Monitor File Converter"), style = "font-size: 19px"),
    tags$br(),
    
     tags$div(
      tags$p(
      'Withind the analyzed period of dates ShinyR-DAM scans monitor files status column for errors, so no pre-filtering 
      or scanning with DAMFileScan is necessary. The maximum size of files that can be uploaded for the analysis is limited 
             to 300 MB, and files can be renamed if necessary.'), 
      style = "font-size: 19px"),
    
    h3(strong('Experiment layout')),
    
    tags$div(
      tags$p('DAM system users typically test multiple replicates of flies in a single experimental condition: genotype, treatment, etc.. 
       The settings panel is designed similar to a typical lab book notes containing information about the experiment.'), 
      style = "font-size: 19px"),
    
   
    tags$div(
      tags$p('A user uploads monitor files and specifies the following settings and variables:'), 
      style = "font-size: 19px")
  ),
      
      
  tags$div(
    tags$div(tags$ul(
      tags$li("Number of experimental conditions,"),
      tags$li("Condition names,"),
      tags$li("Number of flies in each condition,"),
      tags$li("Colors of each condition used for plotting,"),
      tags$li("Order of monitor files")),
  
      style = "font-size: 19px")
  
  ),
  
  tags$h4(strong('Important notes:')),
  
  tags$div(
    tags$div(tags$ul(
      tags$li("Each monitor consists of 32 channels monitoring locomotor activity of 
              individual flies, hence the sum of Number of flies in Condition # must 
              be equal to the number of monitor files uploaded multiplied by 32. If 
              the two values are not equal, the program will print an informative 
              error message in the Data and settings validation section. "),
      tags$div(
        tags$div(
          tags$br(),
          style = "font-size: 10px")
        
      ),
      
      tags$li("The order of condition names must represent the order of channels 
              as specified by the Monitor file order. After uploading monitor files 
              their names are presented in the Monitor order: # select menus. 
              By default, they are ordered by ascending monitor numbers, 
              but a user can freely rearrange this if necessary. By default, 
              the program expects that each experimental condition consists of 
              32 flies (one monitor), but that can be modified to any layout as 
              long as the total number of flies equals the sum of channels in 
              uploaded monitors. If a particular condition spans multiple monitors 
              it is advised to order monitors such that the condition spans continuous
              channels in a range of monitors. The order of conditions is preserved
              throughout the analysis into plots and tables. You can review this layout
              in a Condition/Monitor layout table or by downloading one of the csv files.")),
      style = "font-size: 19px")
    
  ),
  
  tags$h3(strong('Handling empty channels')),
  tags$div(
  tags$p('Empty channels that were not loaded with flies cannot be 
         marked in the layout settings. They have to be assigned into a 
         condition to maintain the requirement of total number of flies 
         equals sum of all monitor channels. If a user decides to add them into 
         experimental conditions, empty channels will be identified as dead flies and 
         automatically excluded from the analysis, thus they will not affect the 
         analysis as long as the Threshold of counts per day for identifying dead flies is set to a non-zero value.
         If an experiment requires a precise assessment of a 
         condition lethality/viability, a user can set a separate Empty condition for these channels.'), 
  style = "font-size: 19px"),

  
  tags$h3(strong('LD DD analysis')),
  tags$div(
    tags$p('User specifies if LD and/or DD days should be analyzed'), 
    style = "font-size: 19px"),
  
  tags$h3(strong('Date settings')),
    tags$div(tags$ul(
      tags$li("ShinyR-DAM only analyzes full 24 h days of recorded data. If a user collects the data in the afternoon and includes that day in the range to analyze, ShinyR-DAM will not proceed with the analysis and prints an informative error in the Data and settings validation section."),
      tags$li("LD and DD days cannot overlap. If ranges of LD and DD days overlap ShinyR-DAM will print an informative error in the Data and settings validation section."),
      tags$li("Dates are inclusive.The first and the last day in a range are included in the analysis.")),
      style = "font-size: 19px"),
  
  tags$h3(strong('Light onset time')),
  tags$div(
    tags$p('Activity profiles and sleep profiles are plotted on two X axis time scales:'), 
    style = "font-size: 19px"),
  tags$div(tags$ul(
    tags$li(" Time of day - data acquisition computer clock time,"),
    tags$li(" Zeitgeber time (ZT) - time scale set to 0 hour at light onset,")),
    style = "font-size: 19px"),
  tags$div(
    tags$p('The purpose of Zeitgeber time is continuous plotting of daytime and nighttime activity/sleep profiles. 
            By default, Light onset time is set to 6:00 AM and it can be adjusted in full hour increments. 
            This setting also defines when a day starts for the daytime and nighttime activity and sleep analyses.
           '), 
    style = "font-size: 19px"),
  
  tags$h3(strong('Threshold of counts per day for identifying dead flies')),
    tags$div(
    tags$p('Good laboratory practice requires removal of dead, completely inactive, flies from the behavioral 
            analysis. This is often done by manual and arbitrary inspection of individual actograms. ShinyR-DAM allows setting 
            a daily activity threshold for identifying dead flies and automatically removes them from the analysis. The threshold 
            checks if during any day of the experiment a fly was less active than the set value. The number of living and dead 
            flies is shown in Locomotor activity per Condition in LD/DD tables.
           '), 
    style = "font-size: 19px"),
  
  
  tags$h3(strong('DAM system data acquisition frequency')),
  tags$div(
    tags$p('The DAM system records sums of counts over a Reading Interval. Most of the ShinyR-DAM functions can analyze data recorded 
            at any Reading Interval, or DAM system data acquisition frequency. 
            Although, Sleep Analysis requires precisely 1 min Reading Intervals, Circadian Period Analysis requires recording 
            frequency >= 1 min. We recommend setting the DAM system to 1 min Reading Interval as it allows 
            the most flexibility in ShinyR-DAM data analyses. 
           '), 
    style = "font-size: 19px"),
  
  
  tags$h3(strong('Data and settings validation')),
  tags$div(
    tags$p('ShinyR-DAM runs four data and settings validation tests: 
           '), 
    style = "font-size: 19px"),
   
  tags$ol(
    tags$li("The first test checks for errors in DAM monitor files. This functionality replaces file scanning with 
            DAMFileScan application. If ShinyR-DAM finds any errors, it prints the detected codes, how many data records are affected, 
            what monitors are affected, and shows the first few rows of problematic data.  This error message also includes 
            a brief suggestion on how to proceed.  To learn more about DAM system error codes please read: "),
             tags$a(href="http://www.trikinetics.com/Downloads/DAMSystem%20User's%20Guide%203.0.pdf", "DAM System User's Guide.pdf"),
    tags$div(br(), style = "font-size: 5px"),
    tags$li("A test ascerting that the total number of flies specified matches the number of channels in uploaded monitor files."), 
    tags$div(br(), style = "font-size: 5px"),
    tags$li("A test for overlap of LD and DD ranges."),
    tags$div(br(), style = "font-size: 5px"),
    tags$li("A test checking if there are missing data records, comparing the number of data records with the number of 
            analyzed days multiplied by acquisition frequency. If an error is detected, ShinyR-DAM prints what day in the range 
            contains missing data, and/or suggests setting acquisition frequency to the correct value."),
    tags$div(br(), style = "font-size: 5px"),
    tags$p(tags$strong('Please pay close attention to these messages.')),
    style = "font-size: 19px"
  ),
  
  
  tags$h3(strong('Error bars')),
  tags$div(
    tags$p('All error bars plotted by ShinyR-DAM are standard errors of the mean (SEM). 
            Error bars represent variation between individuals, not days in the experiment. 
            Variation between days is visualized in the Locomotor activity by day plot.
           '), 
    style = "font-size: 19px"),
  
  tags$h3(strong('Daily locomotor activity analysis')),
  tags$div(
    tags$p('Daily locomotor Aactivity analysis calculates average daily activity of a single fly. 
           '), 
    style = "font-size: 19px"),
  
  
  
  tags$h3(strong('Box plots')),
  tags$div(
    tags$p('Box plots are plotted using ggplot2 geom_boxplot and feature the following statistics: 
           '), 
    style = "font-size: 19px"),
  
  tags$div(tags$ul(
    tags$li("Median,"),
    tags$li("Lower and upper quartiles (25% and 75%) within the box,"),
    tags$li("Minimum and maximum values within whiskers, which are limited to Q1 - 1.5*IQR 
             and Q3 - 1.5*IQR, whereas Q1 - the first quartile, Q3 -  the third quartile, IQR = Q3 - Q1,"),
    tags$li("Outlier points defined as values < Q1 - 1.5*IQR, and > Q3 - 1.5*IQR,")
    ),
    style = "font-size: 19px"),
  
  tags$div(
    tags$p('All data points are displayed in box plots, not only the outliers. 
           '), 
    style = "font-size: 19px"),
  
  
  
  tags$h3(strong('Density plots')),
  tags$div(
    tags$p('Density plots are a smoothened version of histograms produced by ggplot2 geom_density.
            The X axis is arbitrarily limited to values between 0 and 3000 counts/day. 
           '), 
    style = "font-size: 19px"),
  
  
  
  tags$h3(strong('LD activity profiles')),
  tags$div(
    tags$p('LD activity profiles display the average number of counts per fly calculated over the
            Averaging window of activity profiles. The averaging window of activity profiles must be 
            set to one of the multiples of a DAM system data acquisition frequency. Otherwise, plots 
            will not be produced.
           '), 
    style = "font-size: 19px"),
  tags$div(
    tags$p('Nighttime/Daytime activity ratio provides information about the nocturnality. This 
            value represents an average ratio calculated for each individual fly in a condition. 
            Note that it is not calculated as a simple fraction of Daytime activity in LD / Nighttime 
            activity in LD presented in separate plots. The ratio reported may be slightly different 
            from Daytime activity in LD divided by Nighttime activity in LD, which are already averaged in a condition.
           '), 
    style = "font-size: 19px"),
  
  
  
  tags$h3(strong('LD sleep analysis')),
  tags$div(
    tags$p('Sleep is defined as a continuous 5 min period of inactivity. Sleep analysis requires DAM system data 
            acquisition frequency to be 1 min. If this value is different, the analysis is not possible. 
            Sleep events are identified using a sliding window algorithm of 5 min width and 1 min sliding interval.
            A sleep event is detected if all 5 data readings in a window equal 0. Sleep events are then averaged over 
            all individual flies in a condition, and over the Interval for average sleep profile, which sets the resolution 
            for a sleep profile plot. Sleep = 1 means that all flies were inactive. Sleep = 0 means that all flies scored 
            at least 1 count every 5-min within the window of Interval for the average sleep profile.
           '), 
    style = "font-size: 19px"),
  tags$div(
    tags$p('For the convenience of plotting particularly sleepless flies, the maximum value of sleep  
            displayed on a plot (Y axis) can be adjusted with a slider. 
           '), 
    style = "font-size: 19px"),
  
  
  tags$h3(strong('Actograms')),
  tags$div(
    tags$p('Mean and median actograms visualize locomotor activity throughout the progress of an experiment. The Y axis 
            displays mean or median values per acquisition frequency interval. To accommodate longer acquisition frequencies 
            with higher values of counts recorded per interval, or for hyperactive flies, the Y axis limit is adjustable 
            with a Max number of counts displayed slider. Also, the height, width and color of actograms are adjustable. 
           '), 
    style = "font-size: 19px"),
  
  
  tags$h3(strong('Circadian period analysis')),
  tags$div(
    tags$p(' Circadian period is calculated only for DD days. 
           '), 
    style = "font-size: 19px"),
  
  tags$div(
    tags$p('For calculating circadian periodicity we use a chi-square periodogram method implemented by 
           Hitoshi Iuchi and Rikuhiro G. Yamada in their xsp R package. In ShinyR-DAM we adopted their 
           chiSqPeriodogram function by allowing users to specify the Chi-Sq period testing range and Chi-Sq 
           period testing resolution. To allow for filtering out arrhythmic individuals, we 
           provided an adjustable rhythmicity threshold [Qp.act/Qp.sig] filter. Individuals with 
           Qp.act/Qp.sig below that threshold are ignored for calculating mean and median period peaks. 
           '), 
    style = "font-size: 19px"),
  
  tags$div(
    tags$a(href="https://CRAN.R-project.org/package=xsp", " 'xsp' CRAN repository"), style = "font-size: 19px"), 
  
  tags$br(),
  
  tags$div(
    tags$p('Mean periodograms are calculated from all individual periodograms of all alive flies, and peak 
           values are called from these plots. The shaded area represents SEM.
           '), 
    style = "font-size: 19px"),
  
  tags$div(
    tags$p('Individual periodograms show all periodograms as separate traces.
           '), 
    style = "font-size: 19px"),
  
  tags$div(
    tags$p('A box plot of the circadian period of individual flies presents peak circadian period values 
           identified in individuals. Data presented in this plot are subject to the rhythmicity threshold filtering. 
           A plot of circadian period strength of individual flies presents the distribution of Qp.act/Qp.sig values, 
           and is also subjected of the rhythmicity threshold filtering.
           '), 
    style = "font-size: 19px"),
  
  tags$div(
    tags$p('Summary statistics including the number of rhythmic flies are presented in a table. 
           '), 
    style = "font-size: 19px"),
  
    hr()
   
  )
)
)
)

  

###################################   SERVER   ###################################
  
# Define server logic
server <- function(input, output) {

# Sets max file upload to 300 MB. Default is 5 MB.
options(shiny.maxRequestSize=300*1024^2)    
  
# Generates vsrious UI objects requiring values from other inputs, like the number of conditions
  
  output$Conditions_input <- renderUI({
    numb_of_cond <- input$Number_of_conditions
    lapply(1:numb_of_cond, function(i) {
      textInput(paste0('Condition', i), paste('Condition name', i), value = "", width = NULL, placeholder = NULL)
    })
  })
  
  output$Number_of_flies_per_cond <- renderUI({
    numb_of_cond <- input$Number_of_conditions
    lapply(1:numb_of_cond, function(i) {
      numericInput(paste0('Number_of_flies_in_cond', i), paste('Number of flies in Condition', i), value = 32, min=1, width = NULL)
    })
  })
  
  
  output$Colour_selection <- renderUI({
    numb_of_cond <- input$Number_of_conditions
    lapply(1:numb_of_cond, function(i) {
      colourInput(paste0('Condition_colour', i), paste('Color of Condition', i), value = "grey")
    })
  })
  
  
  output$Order_monitors <- renderUI({
    Monitor_files <- input$file1
    lapply(setdiff(1:length(Monitor_files$name), 0), function(i) {
      selectInput(paste0('Monitor_order', i), paste('Monitor file order:', i), Monitor_files$name, selected = Monitor_files$name[i])
    })
  })
  
  
# Generates a list of all input files
  inFile <- reactive(input$file1)

# Reads LD and DD day ranges from the input      
    LD <- reactive(seq(as.Date(input$dateRange[1]),as.Date(input$dateRange[2]),by="day"))
    DD <- reactive(seq(as.Date(input$dateRange1[1]),as.Date(input$dateRange1[2]),by="day"))
    
# Range of days is refined based on the user's choice of LD, DD or both parts of the experiment     
    range_of_days <- reactive({
      if (input$LDDD == "Both") {
        p <- sort(as.Date(c(LD(), DD())))
      } else if (input$LDDD == "DD") {
        p <- sort(as.Date(DD()))
      } else if (input$LDDD == "LD") {
        p <- sort(as.Date(LD()))
        p
      } 
    })
    
    
# A list of all inputs - used for accessing certain variables     
    All_inputs <- reactive(reactiveValuesToList(input))

# Data frame with conditions and number of flies in each condition    
    Conditions_and_numbers <- reactive(
      data.frame(
        Concition_names=unlist(lapply(1:input$Number_of_conditions, function(i){All_inputs()[[paste0("Condition",i)]]})),
        Condition_counts=unlist(lapply(1:input$Number_of_conditions, function(i){All_inputs()[[paste0("Number_of_flies_in_cond",i)]]})))
    )
    
# A vector of desired monitor order     
    Monitor_desired_order <- reactive({
        files <- inFile()
        p <- unlist(lapply(1:length(files$name), function(i){All_inputs()[[paste0("Monitor_order",i)]]}))
        p
        })
    
# A vector of colors    
    Plot_colors <- eventReactive(input$go, {
      files <- inFile()
      p <- unlist(lapply(1:input$Number_of_conditions, function(i){All_inputs()[[paste0("Condition_colour",i)]]}))
      p
    })
    
# A vector of conditions (data frame converstion to a vector)    
    conditions <- reactive(rep(Conditions_and_numbers()$Concition_names, times=Conditions_and_numbers()$Condition_counts))
    
 
    
    
# Generates a table containing LD/DD status
    date_and_light_cycle_1 <- reactive(arrange(arrange(data.frame(date=c(LD(), DD()), light_cycle=c(rep("LD", rep(length(LD()))), rep("DD", rep(length(DD()))))), date), date))
    date_and_light_cycle_2 <- reactive(filter(date_and_light_cycle_1(), date %in% range_of_days()))  #trims the LD/DD table to include only days specified in the general date range of the experiment. 
    #Useful if there are no DD or LD days and some dummy values were specified for missing data.
    
#This applies filtering depending on the LD, DD, both radio button choice,     
    date_and_light_cycle <- reactive({
      if (All_inputs()$LDDD == "Both") {
        p <- date_and_light_cycle_2()
      } else if (All_inputs()$LDDD == "LD") {
        p <- filter(date_and_light_cycle_2(), light_cycle == "LD")
      } else if (All_inputs()$LDDD == "DD") {
        p <- filter(date_and_light_cycle_2(), light_cycle == "DD")
        p
      } 
    })    
    
        

# data_freq - actually a number of minutes per day in the recorded data  
    data_freq <- reactive(1440/input$data_recording_frequency)

# List of monitor files in an order specified by the user. Order is important for proper condition assignment.         
    import.list  <- reactive({
      fil <- input$file1
      fil_ordered <- fil[match(Monitor_desired_order(), fil$name),]
      p<- lapply(fil_ordered$datapath, read.table, fill=TRUE)
      p
      })

# Removes unnecessary columns directly from objects in the list          
    import.lista <- reactive({lapply(import.list(), function(x) x[!(names(x) %in% c("V7", "V8","V9", "V10","V11", "V12"))])})
    
# Merges dataframes of monitor files into a single "data" data frame 
    data <- reactive({
      withProgress(message = 'Reading data', {
        incProgress(1, detail = paste("In progress")) 
      
      Reduce(function(x, y) merge(x, y, all=T, 
                                        by=c("V1", "V2", "V3", "V4", "V5")), import.lista(), accumulate=F)})
      })
    
    
# Singles out the data status columns. This column stores error codes.    
    monitor_data_status <- reactive({
      p <- data()[, grep(glob2rx("V6*"), colnames(data()))]
      p <- as.data.frame(p)
      p
      })
    
# Removes data_status column
    data_x<- reactive({data()[, -grep(glob2rx("V6*"), colnames(data()))]})
    
# Removes columns containing time and data status information, necessary for renaming other collumns with monitor and channel number  
    data_44<- reactive({data_x()[, -c(1:5)]})
    
# Renames channels adding a Monitor number
    file_name_short <- reactive({sub(".txt","", Monitor_desired_order())})
    
# Pastes channel numbers  
    renamed_data_44<-reactive({
      d <- data_44()
      p <- file_name_short()
      colnames(d) <- paste(rep(p, each=32), colnames(d), sep = "_")
      colnames(d) <- gsub("\\V.{1,}", "ch", colnames(d))
      colnames(d) <- paste(grep(glob2rx("*_"), "", colnames(d)), colnames(d), c(1:32), sep="")
      d
    })
    
    
# Pastes monitor numbers into a dataframe containing monitor status codes    
    monitor_data_status_1 <- reactive({ 
      d <- monitor_data_status()
      colnames(d) <- paste(file_name_short(),"data_status", sep = "_")
      d
      })

# Creates a dataframe with date information and status codes         
    data_43 <- reactive({
      data.frame(day=data_x()$V2, month=data_x()$V3, year=data_x()$V4, time=data_x()$V5, monitor_data_status(), renamed_data_44())
    })
    
# Reformats date information from 3 into 1 column.     
    data_42 <- reactive({
      d <- data_43()
      d$date <- as.Date(with(d, paste(d$day, d$month, d$year,sep="-")), "%d-%b-%y")
      d
    })
    
# Subsets the data to a range of dates    
    s_1 <- reactive({
      subset(data_42(), date %in% range_of_days())
    })
    
# Counts the number of days in a dataset    
    number_of_days <- reactive({length(unique(s_1()$date))})
    
# Function converting time into decimal values of minutes of a day  
  decimateTime <- reactive({
    function(time) {
           time=as.numeric(unlist(strsplit(time, ":")))
          time = time[1]*60+time[2]+time[3]/60}
  })  
 
# Converts the  time to decimal values    
   s_3 <- reactive({
      d <- s_1()
      d$Dec_time <- sapply(as.character(d$time), decimateTime())
      d
    })
  
# Saves the s_1 dataframe containing status codes into a separate object. The functionality of checking the status codes was added later and for compatibility with the rest of the code this column is removed in the next line. 
   s_1_with_status<- reactive(s_3())
   
# Selects columns containing data_status
   Data_recording_status<- reactive(subset(s_3(), select=c("date","time", colnames(monitor_data_status()))))
   
# Melting for date and time
   Data_recording_status_m <- reactive({melt_(Data_recording_status(), id.vars=c("date", "time"))})

# Fixes date column. Kmisc package has super fast melt function but it messes up date formating. 
# It's a small price to pay for dramatic increase in performance over the reshape2::melt     
   Data_recording_status_melted <- reactive({
     d <-Data_recording_status_m()
     d$date <- as.Date(d$date)
     d
   })
   
# Function checking data status codes
   Status_codes <- reactive({
     
     p <- Data_recording_status_melted()
     
     if(unique(p$value == 1)){ 
     "Data correct. All DAM data status codes are 1"
    }else {
     print("Error: Data incorrect. Data status codes other than 1 were detected in one or more Monitor files")
     print("Make sure you correctly selected the dates of data to analyze,")
     print("excluding day 0 when you connect monitors to the system,")
     print("and the day you transfered your monitor files from the computer recording the data.")
     print("This program can only analyze FULL DAYS of locomotor activity")
     print("The following status codes were detected")
     print(unique(filter(select(p, value), value != 1)))
     print("Errors were found in this number of rows (minutes of the recorded data if acquisition frequency is set to 1):")  
     print(nrow(filter(p, value != 1)))
     print("Errors were found in the following monitors:")  
     print(unique(select(filter(p, value != 1), variable)))
     print("The first few rows containing errors:")
     print(head(filter(p, value != 1)))
     print("If errors occur in low number of records just ignore them.") 
     print("If errors do not result in a missing data records they will not substantially affect the analysis.")
     print("Errors causing missing data records will also cause the accompanying error:")
     print("Error: The number of minutes in the recorded data does not match the number of days.")
     print("In that case, if the damage is not substantial to the interpretation of the experiment, you may consider to manually fix Monitor files filling the gaps.")
     print("Please also check the condition of your DAM hardware considering the nature of error codes")
     print("More on the DAM system error codes can be found here: http://www.trikinetics.com/Downloads/DAMSystem%20User's%20Guide%203.0.pdf")
     
   }
   })
   
   
# Checks if the number of Monitors and the number of specified conditions match. Sanity check.
   Monitors_vs_conditions_check <- reactive({
     cond <- conditions()
     fil <- input$file1
     
      if(length(cond) == length(fil$name)*32){ 
       ("Input data correct. Total number of flies in Conditions matches total number of channels in Monitors")
     }else {
       print("Error: Input data incorrect; Total number of flies in Conditions doesn't match the total number of channels in Monitors specified")
       print("Number of flies specified in conditions:")
       print(length(conditions()))
       print("Number of channels specified in Monitor files:")
       print(length(fil$name)*32)
       }
     
   })
   
# LD DD dates overlap check. The two date ranges cannot overlap.  
  LD_DD_overlap_check <- reactive({
    invalidateLater(1000)
      
     if(all(!LD() %in% DD())){ 
       print("Input dates correct. LD and DD days do not overlap")
     }else {
       print("Error: Input dates incorrect; LD and DD days overlap")
       print("LD and DD days cannot overlap")
       print("LD days:")
       print(LD())
       print("DD days")
       print(DD())
      }
   })
    
# Checks for missing data records or incorrectly set recording frequency.   
   Data_range_correctness <- reactive({
     
     if(nrow(s_1()) == data_freq()*number_of_days()){ 
       print("Input dates correct. The number of minutes in the recorded data matches the number of days.")
     }else {
       print("Error: The number of minutes in the recorded data does not match the number of days.")
       print("One of the range of dates likely includes an incomplete day of recorded data, or data acquisition frequency is set to an incorrect value.") 
       print("Please adjust your settings")
          nrow_each <- function(x)
          {nrow(filter(s_1(), date==x))}
       print(data.frame(Dates_of_experiment=unique(s_1()$date), weekday=weekdays(unique(s_1()$date)), Number_of_data_readings=sapply(unique(s_1()$date), FUN=nrow_each)))
       print("Each day should contain this number of readings:")
       print(data_freq())
     }
   })
  
  
# Removes unnecessary columns
   s_4 <- reactive({
     d <- s_3()
     d <- d[, -which(colnames(d) %in% c(colnames(monitor_data_status())))]
     d
   })
   
# Priscilla Erickson encounter a problem with data ordering. She added this line to ensure the proper order data in a data frame.
   s_5 <- reactive({
     d <- s_4()
     d <- d[with(d, order(date, Dec_time)),]
     d
   })
     
# Adds LD/DD collumn
   s_6 <- reactive({
     d <- s_5()
     d$Light_cycle <- ifelse(d$date %in% LD(), "LD", "DD")
     d
   })
  
# Makes a list of unique conditions
   unique_conditions <- reactive(unique(conditions()))
   
# An attempt to add a shifted ZT time column to melted dataframe.
# It works but allow to shift the scale only in full hour increments. This part may benefit from some extra work.   
   LD_start <- reactive(as.POSIXct(strptime(input$light_onset_time, format="%H:%M:%S")))
   start <- reactive(as.POSIXct(strptime("00:00:00", format="%H:%M:%S")))
   end <- reactive(as.POSIXct(strptime("23:59:00", format="%H:%M:%S")))
   interval <- reactive(60) #unit seconds
   
# Sequence of hours
   seq_of_Day<- reactive(seq(from=start(), by=interval(), to=end()))
   
# Converstion of date-time format to time as factor
   clock_time<- reactive(as.factor(strftime(seq_of_Day(), format="%H:%M:%S")))
   
# Calculation of time difference between midnigth and the light onset (zeitgeber 0)
   ZT_zero<- reactive(start()-LD_start())
   
# Sets the clock 
   ZT_time<- reactive(as.factor(strftime(seq_of_Day() + hours(ZT_zero()), format="%H:%M:%S")))
  
   ZT_time_conversion <- reactive(data.frame(time=clock_time(), ZT_time=ZT_time()))
   
# Merging with a dataframe
   s_7 <- reactive(merge(s_6(), ZT_time_conversion(), by = "time", all.x=TRUE, all.y=FALSE))
    
# Adding decimated ZT time column
   s_8 <- reactive({
     d <- s_7()
     d$Dec_ZT_time <- sapply(as.character(d$ZT_time),decimateTime())
     d <- d[with(d, order(date, Dec_time)),]
     d
   })
     
# Melting the dataframe and adding a Condition column    
   melted <- reactive({
     d <- s_8()
     d <- melt_(d, id.vars=c("day", "month", "year", "time", "date", "Dec_time", "ZT_time", "Dec_ZT_time", "Light_cycle"))
     d$date <- as.Date(d$date)
     d$Condition <- rep(conditions(), each=(nrow(d)/length(conditions())))
     d
   })
     
     
# A vector of all column names containing fly locomotor data
   all_channels <- reactive(colnames(s_8())[-c(1:4, length(colnames(s_8())), length(colnames(s_8()))-1, length(colnames(s_8()))-2, length(colnames(s_8()))-3,length(colnames(s_8()))-4)])
   
   
# Ensuring the proper order of date and Dec_time columns. It's critical for the roll function to work properly.
   s_9 <- reactive({
     d <- s_8()
     d[with(d, order(date, Dec_time)),]
     d
   })
   
# roll founction calculates sum of counts per day. data_freq is the number of minutes in each day.    
   roll <- reactive({function(x){
     library(dplyr)
     library(zoo)
     rollapply(select(s_9(), get(x)), width = data_freq(), by = data_freq(), FUN = sum, align = "left")}
   })
   
# Calculates daily activity per fly. Execution timewith can be decreased 2x by using paralell sapply.  Tested on 17 monitors over 11 days, 1 min recording freq.
# I had this step paralelized in a script, but it's quite a hassle to get it to run in a Shiny. Pralelization is probably nor allowed on a shiny.io server ?
   activity_by_day <- reactive({
     do.call("cbind", sapply(all_channels(), FUN = roll(), simplify = FALSE))
   })
   
   
# Automatically finds dead flies by their daily activity. Threshold is a total daily activity on any particular day during the experiment.
   dead_flies <- reactive(colnames(activity_by_day())[apply(activity_by_day(), MARGIN=2, function(u) any(u<input$dead_fly_counts_threshold))])
  
   
# Adds date and light_cycle columns
   activity_by_day_1 <- reactive(data.frame(date=date_and_light_cycle()$date, Light_cycle=date_and_light_cycle()$light_cycle, activity_by_day()))
   
   
# Melts activity by day and adds condition, date, and Light_sycle columns
   melted_by_day <- reactive({
    d <- activity_by_day_1()
    p <- melt_(d, id.vars=c("date", "Light_cycle"))
    p$date <- as.Date(p$date)
    p$Condition <- paste(rep(conditions(), each=number_of_days()))
    p
   })
   
# Makes a list of alive flies
   alive<- reactive(setdiff(all_channels(), dead_flies()))
   
# Filters by-date-analysis leaving only living flies. Pun intended.
   melted_by_day_alive <- reactive(subset(melted_by_day(), variable %in% alive()))
   
# Summarizes data per individual fly. data.table package increased the speed of this process a over 200 times comparing do ddply from plyr
   summary_by_fly_alive <- reactive({
   DT <- data.table(melted_by_day_alive())
   p <- DT[,list(mean_value=as.numeric(mean(value))), by=c("Condition", "variable", "Light_cycle")]
   p$Condition <- factor(p$Condition, levels = unique(conditions()))
   p <- arrange(p, Condition)
   p
   })
   
   summary_by_fly_including_dead <- reactive({
   DT<- data.table(melted_by_day())
   p<- DT[,list(mean_value=as.numeric(mean(value))), by=c("Condition", "variable", "Light_cycle")]
   p
   })
   
   all_flies_count <- reactive({
     #it takes the first light cycle condition, either LD or DD.
     p <- summary_by_fly_including_dead()
     k <- ddply(filter(p, Light_cycle==p$Light_cycle[1]), c("Condition"), summarise, N_all_flies=length(Condition))
     k
   })
   
# Object summarizing locomotor activity in LD and DD
   general_summary <- reactive({
     withProgress(message = 'Calculating summary by Condition', min=0, max=0.6, value = 0.5, {
       incProgress(0.8, detail = paste("In progress"))
     
     d <- ddply(summary_by_fly_alive(), c("Condition", "Light_cycle"), summarise,
                           mean = mean(mean_value), sd = sd(mean_value),
                           sem = sd(mean_value)/sqrt(length(mean_value)),
                           N_living_flies=length(Condition))
   d <- cbind(d, Dead_flies=rep(unlist(lapply(1:input$Number_of_conditions, function(i){All_inputs()[[paste0("Number_of_flies_in_cond",i)]]})), each=length(unique(d$Light_cycle))) - d$N_living_flies)
   d$All_flies <- d$N_living_flies + d$Dead_flies
   d
   })})
   
# Reorders the general_summary dataframe according to the original order of conditions
# Filters only the LD or DD data    
   general_summary_LD <- reactive({
     d <- filter(general_summary(), Light_cycle== "LD")
     d <- d[match(unique_conditions(), d$Condition),]
     d
   })
   
   general_summary_DD <- reactive({
   d <- filter(general_summary(), Light_cycle== "DD")
   d <- d[match(unique_conditions(), d$Condition),]
   d
   })

# Calculates summary by day
   summary_by_date<- reactive({
     d<- ddply(melted_by_day_alive(), c("Condition", "Light_cycle", "date"), summarise,
                           mean = mean(value), sum = sum(value), sd = sd(value),
                           sem = sd(value)/sqrt(length(value)))
     d$Condition <- factor(d$Condition, levels = unique(conditions()))
     d <- arrange(d, date)
     d
   })
   
   summary_by_fly_LD <- reactive(filter(summary_by_fly_alive(), Light_cycle=="LD"))
   summary_by_fly_DD <- reactive(filter(summary_by_fly_alive(), Light_cycle=="DD"))


# Here the common general precalculations end. 
# Further part of the code contains aplication tabs specific code, and plotting functions.
   
  # Prevents loading files until files are loaded?
   go_on_files <- eventReactive(input$go, {
     input$file1
   })

### Output of the 4 Error checking functions ###   
output$Monitor_data_codes <- renderPrint({
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate(Status_codes())
   })
   
output$Monitors_vs_conditions_check <- renderPrint({
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate(Monitors_vs_conditions_check())
   })
   
output$LD_DD_overlap_check <- renderPrint({
    go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate(LD_DD_overlap_check())
   })
   
   
output$Data_range_correctness <- renderPrint({
    go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate(Data_range_correctness())
   })
   
   
   
### Monitor layour table ###   
   output$Monitor_layout <- renderTable({
     
     #act_1   <<-  activity_by_day_1()
     #date_light  <<-   date_and_light_cycle()
     #all_inp <<-  All_inputs()
     #in_light <<- input.LDDD()
     
     
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate({
       d <- melted_by_day()
       #Monitor-genotype layout
       #It ain't pretty, but it shows the layout the program interprets the data.
       Monitor_layout<- filter(melted_by_day(), date==(d$date)[1])
       Monitor_layout <- select(Monitor_layout, variable, Condition)
       Monitor_layout <- plyr::rename(Monitor_layout, c("variable"="Channel"))
       #This is an absolut overkill, but it super nicely shows the range of genotypes/conditions in a monitor. I learned a lot by doind this!
       #http://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
       matches <- regmatches(Monitor_layout$Channel, gregexpr("[[:digit:]]+", Monitor_layout$Channel))
       matches<- as.numeric(unlist(matches))
       Monitor_layout <- data.frame(Monitor=matches[seq(1, length(matches), 2)], Channel=matches[seq(2, length(matches), 2)], Condition=Monitor_layout$Condition)
       first <- function(x)
       {head(filter(Monitor_layout, Condition==x), 1)}
       last <- function(x)
       {tail(filter(Monitor_layout, Condition==x), 1)}
       Start<- sapply(unique(melted_by_day()$Condition), FUN=first)
       End <-  sapply(unique(melted_by_day()$Condition), FUN=last)
       Monitor_layout_short<- data.frame(Conditions=colnames(Start), Monitor_Start=paste(Start[1,]), Channel_Start=paste(Start[2,]), Monitor_End=paste(End[1,]),  Channel_End=paste(End[2,]))
       
       Monitor_layout_short
     })
     
   })   
   

### OUTPUT of the Daily Locomotor Activity Analysis ###   

# Data links   
   # Activity per fly
   output$download_Summary_by_fly_alive <- downloadHandler(
     filename = function() {
       paste("Summary_by_fly_alive", Sys.Date(),  ".csv", sep="")
     },
     content = function(file) {
       write.csv(summary_by_fly_alive(), file)
     }
   )
   
   
   # Activity_per_Condition_per_fly_all
   output$downloadActivity_per_Condition_per_fly_all <- downloadHandler(
     filename = function() {
       paste("Activity_per_Condition_per_fly_all", Sys.Date(),  ".csv", sep="")
     },
     content = function(file) {
       write.csv(melted_by_day(), file)
     }
   )   

# Activity in LD bar plot         
   output$bar_plot_LD <- renderPlot({
     
     if (is.null(inFile()))
       return(NULL)
     if (input$go == 0)
       return()
     
     isolate({ 
       
       barplot_LD<- ggplot(general_summary_LD(), aes(x=Condition, y=mean, fill=Condition, width=.5)) + 
         geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
         labs(x="", y="Locomotor activity [counts/day]") +                    #adds/removes axis lables
         theme_bw()+
         geom_errorbar(data=general_summary_LD(), aes(ymax=mean+sem,ymin=mean-sem), width=0.1)+
         scale_x_discrete(limits=unique_conditions())+
         labs(title= "Mean locomotor activity per day in LD", size= 14)+
         theme(legend.text=element_text(size=16))+
         theme(legend.position="none")+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         scale_fill_manual(values=Plot_colors())
       
       barplot_LD
     })
     
   })    

# Activity in DD bar plot      
   output$bar_plot_DD <- renderPlot({
     if (is.null(inFile()))
       return(NULL)
     
     go_on_files()
     
     isolate({  
       ggplot(general_summary_DD(), aes(x=Condition, y=mean, fill=Condition, width=.5)) + 
         geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
         labs(x="", y="Locomotor activity [counts/day]") +                    #adds/removes axis lables
         theme_bw()+
         geom_errorbar(data=general_summary_DD(), aes(ymax=mean+sem,ymin=mean-sem), width=0.1)+
         scale_x_discrete(limits=unique_conditions())+
         theme(legend.text=element_text(size=16))+
         theme(legend.position="none")+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
         labs(title= "Mean locomotor activity per day in DD", size= 14)+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         scale_fill_manual(values=Plot_colors())
     })
   })
   
   
   
# Activity in LD box plot  
   output$box_plot_LD <- renderPlot({
     go_on_files()
     
     if (is.null(inFile()))
       return(NULL)
     isolate({
       box_plot_LD<- ggplot(na.omit(summary_by_fly_LD()), aes(x=Condition, y=mean_value, fill=Condition)) +
         geom_boxplot(alpha=0.7)+
         geom_point()+
         labs(title= "Locomotor activity per day in LD")+
         labs(y="Locomotor activity [counts/day]", x="") +  #adds/removes axis lables
         theme_bw()+
         theme(legend.position="none")+
         theme(legend.text=element_text(size=16))+
         theme(legend.position="none")+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         scale_fill_manual(values=Plot_colors())
       
       box_plot_LD
     })
   }) 

# Activity in DD box plot       
   output$box_plot_DD <- renderPlot({
     go_on_files()
     
     if (is.null(inFile()))
       return(NULL)
     
     isolate({
       box_plot_DD<- ggplot(na.omit(summary_by_fly_DD()), aes(x=Condition, y=mean_value, fill=Condition)) +
         geom_boxplot(alpha=0.7)+
         geom_point()+
         labs(title= "Locomotor activity per day in DD")+
         labs(y="Locomotor activity [counts/day]", x="") +  #adds/removes axis lables
         theme_bw()+
         theme(legend.position="none")+
         theme(legend.text=element_text(size=16))+
         theme(legend.position="none")+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         scale_fill_manual(values=Plot_colors())
       
       box_plot_DD
     })
   }) 

# Activity in LD densitiy plot       
   output$density_plot_LD <- renderPlot({
     if (input$go == 0)
       return()
     
     if (is.null(inFile()))
       return(NULL)
     
     isolate({
       density_plot_LD<- ggplot(na.omit(summary_by_fly_LD()), aes(x=mean_value, fill=Condition)) +
         geom_histogram(aes(y=0.3*..density..),binwidth=100, alpha=0, position="identity") + # alpha=0 makes the histogram invisible, increase to see bars
         geom_density(alpha=.5)+
         xlim(0,3000)+
         labs(title= "Locomotor activity distributions in LD")+
         labs(x="Locomotor activity [counts/day]", y="Density of measurements") +  #adds/removes axis lables
         theme(legend.title=element_blank())+ #removes legend title
         theme_bw()+
         theme(legend.title=element_text(size=16, face="bold"))+
         theme(legend.text=element_text(size=16))+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         theme(axis.title.x = element_text(color="black", size=16))+
         scale_fill_manual(values=Plot_colors())
       
       density_plot_LD
     })
   })
   
# Activity in DD densitiy plot   
   output$density_plot_DD <- renderPlot({
     go_on_files()
     
     if (is.null(inFile()))
       return(NULL)
     isolate({
       density_plot_DD<- ggplot(na.omit(summary_by_fly_DD()), aes(x=mean_value, fill=Condition)) +
         geom_histogram(aes(y=0.3*..density..),binwidth=100, alpha=0, position="identity") + # alpha=0 makes the histogram invisible, increase to see bars
         geom_density(alpha=.5)+
         xlim(0,3000)+
         labs(title= "Locomotor activity distributions in DD")+
         labs(x="Locomotor activity [counts/day]", y="Density of measurements") +  #adds/removes axis lables
         theme(legend.title=element_blank())+ #removes legend title
         theme_bw()+
         theme(legend.title=element_text(size=16, face="bold"))+
         theme(legend.text=element_text(size=16))+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         theme(axis.title.x = element_text(color="black", size=16))+
         scale_fill_manual(values=Plot_colors())
       
       density_plot_DD
     })
   })
   
   
# Activity point plot by day   
   output$point_plot_by_day <- renderPlot({
     
     if (input$go == 0)
       return()
     
     if (is.null(inFile()))
       return(NULL)
     
     isolate({
       
       point_plot_by_date<- ggplot(summary_by_date(), aes(x=date, y=mean, fill=Condition, color=Light_cycle, width=.5))+
         geom_point(stat = "summary", fun.y = "mean", show.legend = FALSE) +         #plots bars,mean value
         labs(x="", y="Locomotor activity [counts/day]") +                    #adds/removes axis lables
         theme_bw()+
         geom_errorbar(data=summary_by_date(), aes(ymax=mean+sem,ymin=mean-sem), width=0.1)+
         theme(strip.text = element_text(size=25)) + #plot titles
         guides(size = guide_legend(order = 1))+
         theme(axis.title.y = element_text(size=14))+
         theme(axis.title.x = element_text(color="black", size=16))+ 
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(legend.title=element_text(size=16, face="bold"))+
         theme(legend.text=element_text(size=16))+
         facet_wrap(~ Condition)
       
       point_plot_by_date
     })
   })
   
# Table output Locomotor activity in LD   
   output$general_summary_LD <- renderTable({
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate({ general_summary_LD() })
   })
   
   
# Table output Locomotor activity in DD   
   output$general_summary_DD <- renderTable({
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate({ general_summary_DD() })
   })
   
  
   
   
############# LD Activity Profiles ############# 
   
   #Activity profiles
   melted_alive <- reactive(filter(melted(), variable %in% alive()))
   melted_alive_LD <- reactive(filter(melted_alive(), Light_cycle=="LD"))
   
   #Claculates mean and sem values for every timepoint per condition in LD
   activity_per_condition<- reactive({
     DT<- data.table(melted_alive_LD())
     p<- DT[,list(mean=as.numeric(mean(value)), sem=as.numeric(sd(value)/sqrt(length(value)))), by=c("Dec_time", "Dec_ZT_time", "Condition")]
     p$Condition <- factor(p$Condition, levels = unique(conditions()))
     p <- arrange(p, Condition)
     p
   })
   
   # Averaging activity into X min intervals. The original approach was just "binning" the data over 30 min, hence the object names. Legacy... 
   
   # Calculating vectors of average values over X min non overlapping windows
   mean_of_activity_per_condition_30_min <- reactive(rollapply(activity_per_condition()$mean, width = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), by = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), FUN = mean))
   sem_of_activity_per_condition_30_min <- reactive(rollapply(activity_per_condition()$sem, width = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), by = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), FUN = mean))
   
   #Adds a mean and sem averaged columns into the original data frame by repeating each element X times
   activity_per_condition_1 <- reactive({
     d <- data.frame(activity_per_condition(), 
                     min_30_mean = rep(mean_of_activity_per_condition_30_min(), each=as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency)),
                     min_30_sem = rep(sem_of_activity_per_condition_30_min(), each=as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency)))
     d
   })
   
   # Sampling the data frame for every Xth row
   activity_30min_mean <- reactive(activity_per_condition_1()[seq(1, nrow(activity_per_condition_1()), by = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency)),])
   
   
   
   # Daytime Nighttime activity in LD. Assigns Day and Night
   Light_status_df <- reactive(data.frame(Dec_ZT_time=c(0:1439), Light_status=c(rep("Day", 720), rep("Night", 720))))
   
   # Side note: Considering Replacing merge function with ifelse. Should be much faster.
   activity_df_matched_alive_LD <- reactive(na.omit(merge(melted_alive_LD(), Light_status_df(), by = "Dec_ZT_time", all.x=TRUE, all.y=TRUE)))
   
   # Calculates sum of counts per individual during the day and during the night
   activity_light_phase_individual <- reactive({
    
     p <- activity_df_matched_alive_LD()
     DT <- data.table(activity_df_matched_alive_LD())
     d <- DT[,list(sum_of_activity_per_ind = sum(value)/length(unique(p$date))), by=c("variable","Condition", "Light_status")]
     d
   })
   
   # Calculates the mean of day/night activity per condition
   activity_condition_phase <- reactive({
     DT<- data.table(activity_light_phase_individual())
     d<- DT[,list(mean_activity_per_condition = mean(sum_of_activity_per_ind),
                  sem = sd(sum_of_activity_per_ind)/sqrt(length(sum_of_activity_per_ind))), by=c("Condition", "Light_status")]
     d$Condition <- factor(d$Condition, levels = unique(conditions()))
     d <- arrange(d, Condition)
     d <- na.omit(d)
     d
   })
   
   activity_condition_Day <- reactive(filter(activity_condition_phase(), Light_status=="Day"))
   activity_condition_Night <- reactive(filter(activity_condition_phase(), Light_status=="Night"))
   
   
   activity_light_phase_individual_Day <- reactive(filter(activity_light_phase_individual(), Light_status=="Day"))
   activity_light_phase_individual_Night <- reactive(filter(activity_light_phase_individual(), Light_status=="Night"))
   
   activity_light_phase_individual_2 <-reactive({
      d <- activity_light_phase_individual_Day()
      n <- activity_light_phase_individual_Night()
   
      p <- data.frame(Channel=d$variable,
                   Condition=d$Condition,
                   sum_of_activity_per_ind_Day= d$sum_of_activity_per_ind,
                   sum_of_activity_per_ind_Night=n$sum_of_activity_per_ind,
                   Night_Day_activity_ratio=n$sum_of_activity_per_ind/d$sum_of_activity_per_ind) 
      p   
     })
     
 #    
   Night_Day_ratio<- reactive({
     k <- ddply(activity_light_phase_individual_2(), c("Condition"), summarise,
                           mean_Night_Day_counts_ratio = mean(Night_Day_activity_ratio),
                           sem = sd(Night_Day_activity_ratio)/sqrt(length(Condition)))
     
     k$Condition <- factor(k$Condition, levels = unique(conditions()))
     k <- arrange(k, Condition)
     k <- na.omit(k)
     k
     
   })
   
# Activity profiles download links
   # Daytime/ Nighttime activity per individual
   output$download_D_N_individual_activity <- downloadHandler(
     filename = function() {
       paste("Daytime_Nighttime_activity_per_individual-", Sys.Date(),  ".csv", sep="")
     },
     content = function(file) {
       write.csv(activity_light_phase_individual_2(), file)
     }
   )
   
   # Activity profiles data file
   output$downloadActivity_profiles <- downloadHandler(
     filename = function() {
       paste("Activity_profiles_data", Sys.Date(),  ".csv", sep="")
     },
     content = function(file) {
       write.csv(activity_30min_mean(), file)
     }
   ) 
   
   
# Activity plots. 30 min interval in the object names is caused by the "legacy" of this code. Bear with me. 
   output$activity_profile_30_min <- renderPlot({
     if (is.null(inFile()))
       return(NULL)
     if (input$refresh_1 == 0)
       return()
     
     withProgress(message = 'Calculating activity profiles', min=0, max=0.6, value = 0.5, {
       incProgress(0.8, detail = paste("In progress"))
       
       isolate({
         # Plots activity in X min intervals
         brake_distances<- c(1,
                             length(unique(activity_30min_mean()$Dec_time))*0.25,
                             length(unique(activity_30min_mean()$Dec_time))*0.5,
                             length(unique(activity_30min_mean()$Dec_time))*0.75,
                             length(unique(activity_30min_mean()$Dec_time))*1)
         
         brakes_vector<- sort(unique(activity_30min_mean()$Dec_time))[brake_distances]
         d <- activity_30min_mean()
         
         plots_activity_30_min <- lapply(unique_conditions(), function(x) ggplot(filter(d, Condition==x), aes(Dec_time, y=min_30_mean, ymax=3, ymin=0)) +
                                           geom_point(color="red") +
                                           geom_line(color="red")+
                                           geom_errorbar(aes(ymax=min_30_mean+min_30_sem,ymin=min_30_mean-min_30_sem), width=0.3)+
                                           labs(title= x, x= "Time of the day [H]", y = "Average counts per min")+
                                           coord_cartesian(ylim=c(0,as.numeric(ac_profile_max_y())))+
                                           scale_x_continuous(breaks = brakes_vector, labels=c("0", "6", "12", "18", "24"))+
                                           theme_bw())
         
         grid.arrange(grobs = plots_activity_30_min, heights = 1)
         
       })
     }) 
  })
   
 # Activity profiles on a ZT scale.  
   output$activity_profile_30_minZT <- renderPlot({
     if (is.null(inFile()))
       return(NULL)
     if (input$refresh_1 == 0)
       return()
     
     isolate({
       
       #Dinfind brake marks for the plot
       brake_distances_shifted<- c(1,
                                   length(unique(activity_30min_mean()$Dec_ZT_time))*0.25,
                                   length(unique(activity_30min_mean()$Dec_ZT_time))*0.5,
                                   length(unique(activity_30min_mean()$Dec_ZT_time))*0.75,
                                   length(unique(activity_30min_mean()$Dec_ZT_time))*1)
       
       brakes_vector_shifted<- sort(unique(activity_30min_mean()$Dec_ZT_time))[brake_distances_shifted]
       
       plots_activity_30_min_shifted <- lapply(unique_conditions(), function(x) ggplot(filter(activity_30min_mean(), Condition==x), aes(Dec_ZT_time, y=min_30_mean, ymax=3, ymin=0)) +
                                                 geom_point(color="red") +
                                                 geom_line(color="red")+
                                                 labs(title= x)+
                                                 theme(plot.title = element_text(size = rel(2), hjust=0.5))+
                                                 geom_errorbar(aes(ymax=min_30_mean-min_30_sem,ymin=min_30_mean+min_30_sem), width=0.3)+
                                                 labs(x= "Zeitgeber time [H]", y = "Average counts per min")+
                                                 coord_cartesian(ylim=c(0,as.numeric(ac_profile_max_y())))+
                                                 scale_x_continuous(breaks = brakes_vector_shifted, labels=c("0", "6", "12", "18", "24"))+
                                                 theme_bw())
       
       grid.arrange(grobs = plots_activity_30_min_shifted, heights=1)  
       
     })
   })  
   
   
   
   
   #
   output$Daytime_activity <- renderPlot({
     
     if (is.null(inFile()))
       return(NULL)
     if (input$go == 0)
       return()
     
     isolate({
       
       barplot_activity_phase_Day<- ggplot(activity_condition_Day(), aes(x=Condition, y=mean_activity_per_condition, fill=Condition, width=.5)) + 
         geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
         labs(x="", y="Locomotor activity [counts/min]") +                    #adds/removes axis lables
         theme_bw()+
         geom_errorbar(data=activity_condition_Day(), aes(ymax=mean_activity_per_condition+sem,ymin=mean_activity_per_condition-sem), width=0.1)+
         scale_x_discrete(limits=unique(conditions()))+
         theme(legend.text=element_text(size=16))+
         theme(legend.position="none")+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
         labs(title= "Daytime activity in LD", size= 14)+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         scale_fill_manual(values=Plot_colors())
       
       barplot_activity_phase_Day  
     })
   })
   
   #
   output$Nighttime_activity <- renderPlot({
     if (is.null(inFile()))
       return(NULL)
     if (input$go == 0)
       return()
     
     isolate({
       
       barplot_activity_phase_Nigth<- ggplot(activity_condition_Night(), aes(x=Condition, y=mean_activity_per_condition, fill=Condition, width=.5)) + 
         geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
         labs(x="", y="Locomotor activity [counts/min]") +                    #adds/removes axis lables
         theme_bw()+
         geom_errorbar(data=activity_condition_Night(), aes(ymax=mean_activity_per_condition+sem,ymin=mean_activity_per_condition-sem), width=0.1)+
         scale_x_discrete(limits=unique(conditions()))+
         theme(legend.text=element_text(size=16))+
         theme(legend.position="none")+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
         labs(title= "Nighttime activity in LD", size= 14)+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         scale_fill_manual(values=Plot_colors())
       
       barplot_activity_phase_Nigth  
     })
   })
   
   
   
   #
   output$Nighttime_to_daytime_act_ratio <- renderPlot({
     if (is.null(inFile()))
       return(NULL)
     if (input$go == 0)
       return()
     
     isolate({
       
       barplot_Night_Day_ratio<- ggplot(Night_Day_ratio(), aes(x=Condition, y=mean_Night_Day_counts_ratio, fill=Condition, width=.5)) + 
         geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
         labs(x="", y="Mean of individual Nighttime counts/ Daytime counts ratios") +                    #adds/removes axis lables
         theme_bw()+
         geom_errorbar(data=Night_Day_ratio(), aes(ymax=mean_Night_Day_counts_ratio+sem,ymin=mean_Night_Day_counts_ratio-sem), width=0.1)+
         scale_x_discrete(limits=unique(conditions()))+
         theme(legend.text=element_text(size=16))+
         theme(legend.position="none")+
         theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
         labs(title= "Nighttime/Daytime activity ratio", size= 14)+
         theme(plot.title = element_text(size = rel(2), hjust=0.5))+
         theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
         theme(axis.title.y = element_text(size=14))+
         scale_fill_manual(values=Plot_colors())
       
       barplot_Night_Day_ratio 
     })
     
     
   })
   
   
   #
   output$daytime_activity_table <- renderTable({
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate({ activity_condition_Day() })
   })
   
   
   #
   output$nighttime_activity_table <- renderTable({
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate({ activity_condition_Night() })
   })
   
   #
   output$n_d_ratio_table <- renderTable({
     go_on_files()
     if (is.null(inFile()))
       return(NULL)
     isolate({ Night_Day_ratio() })
   })
   
   
   
############### SLEEP ANALYSIS ################   
   
# Function calculating a sums in 5 min sliding windows, progressing in 1 min increments. 0 represent sleep. 
   roll_sleep <- reactive({
     function(x){rollapply(select(s_9(), get(x)), width = 5, by = 1, FUN = sum, align = "left")}   
   })

# Applied the roll_speel function to the data         
   activity_sleep <- reactive({
     withProgress(message = 'Calculating sleep 1/3', min=0, max=0.6, value = 0.5, {
       incProgress(0.8, detail = paste("In progress"))
     
     do.call("cbind", sapply(all_channels(), FUN = roll_sleep(), simplify = FALSE))
   })})

# Melts sleep   
   melted_sleep <- reactive({melt_(activity_sleep())})
   
# 5 min sliding window algorithm produces a dataframe that is 4 rows shorter than the original data frame (5 min window cannot span the last 4 min). 
# This copies columns including date, time, etc. from the original data frame, skipping the last 4 rows of recording and joins it with the sleep dataset. 
# The copied s_9 columns are repeated to fit melted_sleep data frame. I didn't need to specify that repetition. It seems to be a default R behavior. ?  
   sleep_df <- reactive({
     d <- data.frame(s_9()[1:(nrow(s_9())-4),  c(1:4,as.numeric(length(colnames(s_9())) - 2), as.numeric(length(colnames(s_9())) - 1), as.numeric(length(colnames(s_9())) - 3), as.numeric(length(colnames(s_9())) - 4), as.numeric(length(colnames(s_9()))))], melted_sleep()[,c(2:3)])
     colnames(d)[10] <- "Channel"
     d
     })
  
# Extracts Channel, Condition columns from the melted object.
   Condition_channel_df <- reactive({
     distinct(data.frame(Channel=melted()$variable, Condition=melted()$Condition))
   })
   
# Merges Condition column with the sleep_df = adds Condit information
   sleep_df_matched <- reactive({
     withProgress(message = 'Calculating sleep 2/3', min=0, max=0.6, value = 0.5, {
       incProgress(0.8, detail = paste("In progress"))
     merge(sleep_df(), Condition_channel_df(), by = "Channel", all.x=TRUE, all.y=TRUE)})})
   
# Assigns values of 1 for inactivity, and 0 for any activity. Values of 1 in the sleep_counts column indicate sleep, 0 are activity.
   sleep_df_matched_1 <- reactive({
     d <- sleep_df_matched()
     d$sleep_counts <- ifelse(d$value==0, 1, 0)
     d
   })
   
# Filters only alive flies
   sleep_df_matched_alive <- reactive({filter(sleep_df_matched_1(), Channel %in% alive())})
   
# Filters only LD data
   sleep_df_matched_alive_LD <- reactive({filter(sleep_df_matched_alive(), Light_cycle=="LD")})
   
   
# Calculates mean sleep and sem in each timepoint for each Condition
    mean_of_sleep_per_condition <- reactive({
      withProgress(message = 'Calculating sleep 3/3', min=0, max=0.6, value = 0.5, {
        incProgress(0.8, detail = paste("In progress"))
      
      DT <- data.table(sleep_df_matched_alive_LD())
      d <- DT[,list(mean_of_sleep_counts=as.numeric(mean(sleep_counts)), sem=as.numeric(sd(sleep_counts)/sqrt(length(sleep_counts)))), by=c("Dec_time", "Dec_ZT_time", "Condition")]
      d$Condition <- factor(d$Condition, levels = unique(conditions()))
      d <- arrange(d, Condition)
      d
    })})
   
# Calculates vectors of average values over X min non overlapping windows. Originaly it used 30 min windows, I later made it adjustable.
    mean_of_sleep_per_condition_30_min <- reactive({rollapply(mean_of_sleep_per_condition()$mean_of_sleep_counts, width = as.numeric(input$sleep_profile_window), by = as.numeric(input$sleep_profile_window), FUN = mean)})

    sem_of_sleep_per_condition_30_min <- reactive({rollapply(mean_of_sleep_per_condition()$sem, width = as.numeric(input$sleep_profile_window), by = as.numeric(input$sleep_profile_window), FUN = mean)})   
   
# Adds mean and sem averaged columns into the original data frame by repeating each element X times
    mean_of_sleep_per_condition_1 <- reactive({
      withProgress(message = 'Calculating sleep profiles', min=0, max=0.6, value = 0.5, {
        incProgress(0.8, detail = paste("In progress"))
      
      d <- mean_of_sleep_per_condition()
      d$min_30_mean <- rep(mean_of_sleep_per_condition_30_min(), each=as.numeric(input$sleep_profile_window))
      d$min_30_sem <- rep(sem_of_sleep_per_condition_30_min(), each=as.numeric(input$sleep_profile_window))
      d
    })})
    
# Sampling the data frame for every Xth row
    sleep_30min_mean <- reactive({mean_of_sleep_per_condition_1()[seq(1, nrow(mean_of_sleep_per_condition_1()), by = as.numeric(input$sleep_profile_window)),]})
    
    
# Quantitative sleep
    # mean sum of sleep per condition
    # Calculates sum of sleep per individual per day in Light and Dark phase
    sleep_per_individual_light_phase <- reactive({
      p <- sleep_df_matched_alive_LD()
      p <- merge(p, Light_status_df(), by = "Dec_ZT_time", all.x=TRUE, all.y=TRUE)
      DT<- data.table(p)
      d <- DT[,list(sum_of_sleep_per_ind=as.numeric(mean(sleep_counts))), by=c("Channel","Condition", "Light_status")]
      d
    })
    
    sleep_per_condition <- reactive({
      DT<- data.table(sleep_per_individual_light_phase())
      d <- DT[,list(mean_of_sleep_per_condition=as.numeric(mean(sum_of_sleep_per_ind)), 
                  sem=as.numeric(sd(sum_of_sleep_per_ind)/sqrt(length(sum_of_sleep_per_ind)))), 
            by=c("Condition", "Light_status")]
      
      d$Condition <- factor(d$Condition, levels = unique(conditions()))
      d <- arrange(d, Condition)
      d
    })
   
    sleep_per_condition_Day <- reactive(filter(sleep_per_condition(), Light_status=="Day")) 
    sleep_per_condition_Night <- reactive(filter(sleep_per_condition(), Light_status=="Night"))

    
# Sleep link - data summary    
    output$downloadSleep_profiles <- downloadHandler(
      filename = function() {
        paste("Sleep_profiles_data", Sys.Date(),  ".csv", sep="")
      },
      content = function(file) {
        write.csv(sleep_30min_mean(), file)
      }
    )
    
# Sleep link - individual fly data     
    output$downloadSleep_ind_flies <- downloadHandler(
      filename = function() {
        paste("Sleep_individual_flies", Sys.Date(),  ".csv", sep="")
      },
      content = function(file) {
        write.csv(sleep_per_individual_light_phase(), file)
      }
    )    
        
    
# Defines the max sleep value displayed on the profile plot  
    slp_profile_max_y <- eventReactive(input$refresh_2,{
      input$max_slp_value
    }) 
    
# Generates a sleep profile plot    
    output$sleep_profiles <- renderPlot({
      withProgress(message = 'Drawing sleep profiles', min=0, max=0.6, value = 0.5, {
        incProgress(0.8, detail = paste("In progress"))
        
        if (is.null(inFile()))
          return(NULL)
        if (input$refresh_2 == 0)
          return()
        
        isolate({
          # Finding brake points for the plot
          brake_distances<- c(1,
                              length(unique(sleep_30min_mean()$Dec_time))*0.25,
                              length(unique(sleep_30min_mean()$Dec_time))*0.5,
                              length(unique(sleep_30min_mean()$Dec_time))*0.75,
                              length(unique(sleep_30min_mean()$Dec_time))*1)
          
          brakes_vector<- sort(unique(sleep_30min_mean()$Dec_time))[brake_distances]
          
          sleep_plots_30_min <- lapply(unique(conditions()), function(x) ggplot(filter(sleep_30min_mean(), Condition==x), aes(Dec_time, y=min_30_mean, ymax=1, ymin=0)) +
                                         geom_point(color="blue") +
                                         geom_line(color="blue")+
                                         geom_errorbar(aes(ymax=min_30_mean-min_30_sem,ymin=min_30_mean+min_30_sem), width=0.3)+
                                         labs(title= x, x= "Time of the day [H]", y = "Sleep for 30 min")+
                                         theme_bw()+
                                         coord_cartesian(ylim=c(0,as.numeric(slp_profile_max_y())))+
                                         scale_x_continuous(breaks = brakes_vector, labels=c("0", "6", "12", "18", "24")))
          
          
          grid.arrange(grobs = sleep_plots_30_min, heights = 1)
        })  
      })})
    
# Generates sleep profiles plotted on a ZT axis    
    output$sleep_profiles_ZT <- renderPlot({
      withProgress(message = 'Drawing sleep profiles ZT', min=0, max=0.6, value = 0.5, {
        incProgress(0.8, detail = paste("In progress"))
        
        
        if (is.null(inFile()))
          return(NULL)
        if (input$refresh_2 == 0)
          return()
        
        isolate({
          # Finding brake points for the plot
          brake_distances<- c(1,
                              length(unique(sleep_30min_mean()$Dec_ZT_time))*0.25,
                              length(unique(sleep_30min_mean()$Dec_ZT_time))*0.5,
                              length(unique(sleep_30min_mean()$Dec_ZT_time))*0.75,
                              length(unique(sleep_30min_mean()$Dec_ZT_time))*1)
          
          brakes_vector<- sort(unique(sleep_30min_mean()$Dec_ZT_time))[brake_distances]
          
          sleep_plots_30_min <- lapply(unique(conditions()), function(x) ggplot(filter(sleep_30min_mean(), Condition==x), aes(Dec_ZT_time, y=min_30_mean, ymax=1, ymin=0)) +
                                         geom_point(color="blue") +
                                         geom_line(color="blue")+
                                         geom_errorbar(aes(ymax=min_30_mean-min_30_sem,ymin=min_30_mean+min_30_sem), width=0.3)+
                                         labs(title= x, x= "Zeitgeber time [H]", y = "Sleep for 30 min")+
                                         theme_bw()+
                                         coord_cartesian(ylim=c(0,as.numeric(slp_profile_max_y())))+
                                         scale_x_continuous(breaks = brakes_vector, labels=c("0", "6", "12", "18", "24")))
          
          
          grid.arrange(grobs = sleep_plots_30_min, heights = 1)
        })
      })})        

        
# Plots daytime sleep stats    
    output$Daytime_sleep <- renderPlot({
      if (is.null(inFile()))
        return(NULL)
      if (input$refresh_2 == 0)
        return()
      
      isolate({
        barplot_sleep_Day<- ggplot(sleep_per_condition_Day(), aes(x=Condition, y=mean_of_sleep_per_condition, fill=Condition, width=.5)) + 
          geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
          labs(x="", y="Average sleep per individual") +                    #adds/removes axis lables
          theme_bw()+
          geom_errorbar(data=sleep_per_condition_Day(), aes(ymax=mean_of_sleep_per_condition+sem,ymin=mean_of_sleep_per_condition-sem), width=0.1)+
          scale_x_discrete(limits=unique_conditions())+
          theme(legend.text=element_text(size=16))+
          theme(legend.position="none")+
          theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
          labs(title= "Daytime sleep in LD", size= 14)+
          theme(plot.title = element_text(size = rel(2), hjust=0.5))+
          theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
          theme(axis.title.y = element_text(size=14))+
          scale_fill_manual(values=Plot_colors())+
          coord_cartesian(ylim=c(min(sleep_per_condition_Day()$mean_of_sleep_per_condition)-(sleep_per_condition_Day()$sem), max(sleep_per_condition_Day()$mean_of_sleep_per_condition)+sleep_per_condition_Day()$sem))
        
        barplot_sleep_Day
        
      })
    })

    
# Plots nighttime sleep stats        
    output$Nighttime_sleep <- renderPlot({
      if (is.null(inFile()))
        return(NULL)
      if (input$refresh_2 == 0)
        return()
      
      isolate({
  
        barplot_sleep_Night<- ggplot(sleep_per_condition_Night(), aes(x=Condition, y=mean_of_sleep_per_condition, fill=Condition, width=.5)) + 
          geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
          labs(x="", y="Average sleep per individual") +                    #adds/removes axis lables
          theme_bw()+
          geom_errorbar(data=sleep_per_condition_Night(), aes(ymax=mean_of_sleep_per_condition+sem,ymin=mean_of_sleep_per_condition-sem), width=0.1)+
          scale_x_discrete(limits=unique_conditions())+
          theme(legend.text=element_text(size=16))+
          theme(legend.position="none")+
          theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
          labs(title= "Nighttime sleep in LD", size= 14)+
          theme(plot.title = element_text(size = rel(2), hjust=0.5))+
          theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
          theme(axis.title.y = element_text(size=14))+
          scale_fill_manual(values=Plot_colors())+
          coord_cartesian(ylim=c(min(sleep_per_condition_Night()$mean_of_sleep_per_condition)-(sleep_per_condition_Night()$sem), max(sleep_per_condition_Night()$mean_of_sleep_per_condition)+sleep_per_condition_Night()$sem))
        
        barplot_sleep_Night
        
      })
    })
    
# Table - daytime sleep    
    output$sleep_per_cond_Day <- renderTable({
      go_on_files()
      if (is.null(inFile()))
        return(NULL)
      if (input$refresh_2 == 0)
        return()
      
      isolate({sleep_per_condition_Day()})
    })

# Table - nighttime sleep        
    output$sleep_per_cond_Night <- renderTable({
      go_on_files()
      if (is.null(inFile()))
        return(NULL)
      if (input$refresh_2 == 0)
        return()
      
      isolate({sleep_per_condition_Night()})
    })    
    
    
    
    
    
######################## Actograms ########################

# Actograms input parameteres  
  
  ac_heigth <- eventReactive(input$refresh, {
    input$actograms_height
  })
  
  ac_width <- eventReactive(input$refresh, {
    input$actograms_width
  })
  
  ac_max_counts <- eventReactive(input$refresh, {
    input$max_act_value
  })
  
  ac_profile_max_y <- eventReactive(input$refresh_1,{
    input$ac_profile_y_lim
  }) 
  

# Calculates a dataset for plotting actograms  
  mean_and_median <- reactive({
    DT<- data.table(melted_alive())
    DT[,list(median=as.numeric(median(value)), mean=as.numeric(mean(value))), by=c("Dec_time", "Condition", "date")]
  })
  

# Function defining a median actogram plot    
  median_actogram <- reactive({function(x){
    if (input$refresh == 0)
      return()
    
    isolate(
      ggplot(filter(mean_and_median(), Condition==x), aes(Dec_time, y=median, ymax=median, ymin=min(median))) +
        geom_ribbon(fill=input$actogram_color) +
        facet_grid(date ~ .)+
        labs(title= x, x= "", y = "Counts/acquisition frequency")+
        theme_bw()+
        scale_x_continuous(breaks = c(0, 360,720, 1080, (1440-input$data_recording_frequency)), labels=c("0h", "6h", "12h", "18h", "24h")) +
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
          )
  }
  })

# Function defining a mean actogram plot   
  mean_actogram <- reactive({function(x){
    
    isolate(
      ggplot(filter(mean_and_median(), Condition==x), aes(Dec_time, y=mean, ymax=mean, ymin=min(mean))) +
        geom_ribbon(fill=input$actogram_color) +
        facet_grid(date ~ .)+
        labs(title= x, x= "", y = "Counts/recording frequency")+
        theme_bw()+
        scale_x_continuous(breaks = c(0, 360,720, 1080, (1440-input$data_recording_frequency)), labels=c("0h", "6h", "12h", "18h", "24h")) +
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
              )
  }
  })
  
# Plots median actograms
  output$Median_actograms <- renderPlot({
    withProgress(message = 'Drawing actograms', min=0, max=0.6, value = 0.5, {
      incProgress(0.8, detail = paste("Plotting median actograms"))
      
      if (is.null(inFile()))
        return(NULL)
      if (input$refresh == 0)
        return()
      
      isolate(
        marrangeGrob(lapply(unique_conditions(), function(x) FUN=median_actogram()(x)), ncol=1, nrow = length(unique(conditions())), top ="")
             )
    })
    })
  
  
# Plots mean actograms
  output$Mean_actograms <- renderPlot({
    
    withProgress(message = 'Drawing actograms', min=0, max=0.6, value = 0.5, {
      incProgress(0.8, detail = paste("Plotting mean actograms"))
      
      if (is.null(inFile()))
        return(NULL)
      if (input$refresh == 0)
        return()
      
      isolate(
        marrangeGrob(lapply(unique_conditions(), function(x) FUN=mean_actogram()(x)), ncol=1, nrow = length(unique(conditions())), top ="")
            )
    })
    })
  
  
# Generates a UI element with median actogram plots - allows heigth and width customization   
  output$Median_act <- renderUI({
    
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh == 0)
      return()
    
    isolate(
      plotOutput("Median_actograms", 
                 height = (as.numeric(ac_heigth())*as.numeric(length(unique_conditions())*as.numeric(length(range_of_days())))), 
                 width = ac_width())
           )
  })
  
# Generates a UI element with mean actogram plots - allows heigth and width customization     
  output$Mean_act <- renderUI({
    
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh == 0)
      return()
    
    isolate(
      plotOutput("Mean_actograms", 
                 height = (as.numeric(ac_heigth())*as.numeric(length(unique_conditions())*as.numeric(length(range_of_days())))), 
                 width = ac_width())
                 )
  })
  

  
  ################# Circadian Period analysis #############################
  
# This function was adapted from the xps package. I added parameters allowing to change the window of period testing, and adjuts the resolution  
  chiSqPeriodogram_2 <- function(activityDF, res=input$Period_resolution){
    testPerVec <- seq(input$Period_range[1], input$Period_range[2],by=res)
    qpArray  <- apply(as.array(testPerVec), 1, function(x) calcQp(activityDF$value,x))
    sigArray <- apply(as.array(testPerVec), 1, function(x) qchisq(0.99^(1/length(testPerVec)), round(x*60)))
    
    data.frame(testPeriod=testPerVec, Qp.act=qpArray, Qp.sig=sigArray)
  }
  
  
# This is a function from the xsp package.    
  calcQp <- function(values, varPer){
    colNum <- round(varPer*60)
    rowNum <- floor(length(values)/colNum)
    foldedValues <- matrix(values[1:(colNum*rowNum)], ncol=colNum, byrow=T)
    avgAll <- mean(foldedValues);
    avgP <- apply(foldedValues, 2, mean)
    numerator <- sum((avgP-avgAll)^2)
    denom <- sum((values-avgAll)^2)/(rowNum*colNum*rowNum)
    qp <- numerator/denom
    return(qp)
  }
  
  
# Filters only DD days  
  s_10 <- reactive(filter(s_9(), Light_cycle == 'DD'))
  s_12 <- reactive(s_10()[, all_channels()])


# Function calculating Qp.act values      
  Qp.act_calculation <- reactive({function(x){
    oscillation.df <- data.frame(dateTime = rep(1:1440, length(unique(s_10()$date))), value = rep(x, each = as.numeric(input$data_recording_frequency)))
    p <- chiSqPeriodogram_2(oscillation.df)
    d <- p[,c(2)]
    d
  }})
  

# Runs the Qp.act_calculation function    
  df_Qp.act <- reactive({
    
    withProgress(message = 'Calculating periodograms', min=0, max=0.6, value = 0.5, {
      incProgress(0.8, detail = paste("Calculating Qp.act"))
      
      do.call("cbind",lapply(s_12(), Qp.act_calculation()))
    })
  })
  

# Function calculating Qp.sig values    
  Qp.sig_calculation <- reactive({function(x){
    oscillation.df <- data.frame(dateTime = rep(1:1440, length(unique(s_10()$date))), value = rep(x, each = as.numeric(input$data_recording_frequency)))
    p <- chiSqPeriodogram_2(oscillation.df)
    d <- p[,c(3)]
    d
  }})

# Runs the Qp.sig_calculation function    
  df_Qp.sig <- reactive({
    withProgress(message = 'Calculating periodograms', min=0, max=0.6, value = 0.5, {
      incProgress(0.8, detail = paste("Calculating Qp.sig"))
      
      do.call("cbind",lapply(s_12(), Qp.sig_calculation()))
    })
  })
  
  
# Calculates periodograms  
  oscillation.df <- reactive({
    data.frame(dateTime = rep(1:1440, length(unique(s_10()$date))), value = rep(s_12()[,1], each = as.numeric(input$data_recording_frequency)))
  })
  k <- reactive(chiSqPeriodogram_2(oscillation.df())[,1])

# Melts    
  df_m_Qp.act <- reactive(melt_(df_Qp.act()))
  df_m_Qp.sig <- reactive(melt_(df_Qp.sig()))
  
# Dataframe of individual fly periods - all flies  
  df_period <- reactive({
    data.frame(
      condition = rep(conditions(), each=nrow(df_m_Qp.act())/length(conditions())),
      channel = df_m_Qp.act()$col,
      period = rep(k(), nrow(df_m_Qp.act())/length(k())),
      Qp.act = df_m_Qp.act()$value,
      Qp.sig = df_m_Qp.sig()$value,
      Act_Sig_ratio = df_m_Qp.act()$value/df_m_Qp.sig()$value
    )
  })
  
# Dataframe of individual fly periods - alive flies  
  df_period_alive <- reactive({
    p <- filter(df_period(), channel %in% alive())
    p$condition <- factor(p$condition, levels = unique(conditions()))
    p <- arrange(p, condition)
    p
  })
  
# Calculates mean period by condition  
  mean_period_by_condition <- reactive({
    DT<- data.table(df_period_alive())
    p<- DT[,list(
      mean_Qp.act = as.numeric(mean(na.omit(Qp.act))), 
      SEM_Qp.act=as.numeric(sd(na.omit(Qp.act))/sqrt(length(na.omit(Qp.act)))),
      n_of_animals = length(Qp.act),
      mean_Qp.sig = as.numeric(mean(na.omit(Qp.sig))),
      sem_Qp.sig=as.numeric(sd(na.omit(Qp.sig))/sqrt(length(na.omit(Qp.sig)))),
      
      "mean_Qp.act_Qp.sig_ratio" = as.numeric(mean(na.omit(Act_Sig_ratio))),
      "SEM_Qp.act_Qp.sig_ratio" = as.numeric(sd(na.omit(Act_Sig_ratio))/sqrt(length(na.omit(Act_Sig_ratio))))),
      
      by=c("condition", "period")]
    p$condition <- factor(p$condition, levels = unique(conditions()))
    p <- arrange(p, condition)
    p
  })
 
# Function finding a peak value in a mean periodogram. Limited by the rhythmicity_threshold.    
  peak_find_mean <- reactive({
    function(x){
      p <- filter(mean_period_by_condition(), condition == x)
      p <- filter(p, mean_Qp.act_Qp.sig_ratio > input$rhythmicity_threshold)
      k <- p[which(max(p$mean_Qp.act_Qp.sig_ratio) == p$mean_Qp.act_Qp.sig_ratio),]
      k
    }
  })
  
# Period peaks detected from the mean periodograms  
  period_peaks <- reactive({
    p <- do.call("rbind",lapply(unique(mean_period_by_condition()$condition), peak_find_mean()))
    p$condition <- factor(p$condition, levels = unique(conditions()))
    p <- arrange(p, condition)
    p
  })
  
# Function finding a peak value in periodograms of individual flies. Limited by the rhythmicity_threshold.  
  peak_find <- reactive({
    function(x){
      p <- filter(df_period_alive(), channel == x)
      p <- filter(p, Act_Sig_ratio > input$rhythmicity_threshold)
      k <- p[which(max(p$Act_Sig_ratio) == p$Act_Sig_ratio),]
      k
    }
  })
  
# Period peaks of individual flies  
  individual_period_peaks <- reactive({
    p <- do.call("rbind",lapply(unique(df_period_alive()$channel), peak_find()))
    p <- filter(p, Act_Sig_ratio > input$rhythmicity_threshold)
    p$condition <- factor(p$condition, levels = unique(conditions()))
    p <- arrange(p, condition)
    p
  })
  
  
# mean_period_peaks_by_condition - summarizes the data. 
  mean_period_peaks_by_condition<- reactive({
    {
      DT<- data.table(individual_period_peaks())
      p<- DT[,list(
        median_period=as.numeric(median(period)),
        mean_period=as.numeric(mean(period)),
        SEM_period=as.numeric(sd(period)/sqrt(length(period))),
        n_of_rhythmic_flies = length(Qp.act),
        
        mean_Qp.act=as.numeric(mean(Qp.act)), 
        SEM_Qp.act=as.numeric(sd(Qp.act)/sqrt(length(Qp.act))),
        mean_Qp.sig = mean(Qp.sig),
        
        "mean_Qp.act/Qp.sig" = as.numeric(mean(Act_Sig_ratio)),
        "SEM_Qp.act/Qp.sig" = as.numeric(sd(Act_Sig_ratio)/sqrt(length(Act_Sig_ratio)))),
        by=c("condition")]
      p$condition <- factor(p$condition, levels = unique(conditions()))
      p <- arrange(p, condition)
      p
    }
  })
  
  
  ########### Circadian plots #########
  
# Mean periodograms  
  output$mean_periodograms <- renderPlot({
    
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_3 == 0)
      return()
    
    isolate({
      ggplot(mean_period_by_condition(), aes(x=period, y=mean_Qp.act, colour = condition)) + 
        geom_point()+
        geom_line()+
        geom_line(data=mean_period_by_condition(), aes(x=period, y = mean_Qp.sig), size=0.5, colour="black")+
        geom_ribbon(aes(ymin=mean_Qp.act-SEM_Qp.act, ymax=mean_Qp.act+SEM_Qp.act), linetype=1, alpha=0.01)+
        theme_bw()+
        labs(x="Circadian period [h]", y="Mean Qp.act")+
        labs(title= "Mean periodograms", size= 14)+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        scale_x_continuous(breaks = seq(4, 56, 2)) +
        geom_vline(xintercept = 24, linetype = 2 ) +
        theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(size=14))+
        theme(axis.title.x = element_text(size=14))+
        theme(axis.text.x=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(legend.title = element_blank())+
        scale_fill_manual(values=Plot_colors())+
        theme(legend.text=element_text(size=16))+
        theme(strip.text = element_text(size=25))+
        scale_color_manual(values=Plot_colors())+
        theme(legend.position="none")+
        facet_wrap(~ condition)
      
    })
  })
  

# Individual periodograms    
  output$individual_periodograms <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_3 == 0)
      return()
    
    isolate({
      ggplot(df_period_alive(), aes(x=period, y=Qp.act, colour = channel)) + 
        geom_point()+
        geom_line()+
        geom_line(data= df_period_alive(), aes(x=period, y = Qp.sig), size=0.5, colour="black")+
        theme_bw()+
        labs(x="Circadian period [h]", y="Qp.act")+
        labs(title= "Individual periodograms", size= 14)+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        scale_x_continuous(breaks = seq(4, 56, 2)) +
        geom_vline(xintercept = 24, linetype = 2 ) +
        theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(size=14))+
        theme(axis.title.x = element_text(size=14))+
        theme(axis.text.x=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(legend.title = element_blank())+
        scale_fill_manual(values=Plot_colors())+
        theme(legend.position="none")+
        theme(strip.text = element_text(size=25))+
        facet_wrap(~ condition)
      
    })
  })
  
  
# Box plot of individual period peaks  
  output$ind_period_peaks <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    geom_dotplot(binaxis = "y", stackdir = "center")
    if (input$refresh_3 == 0)
      return()
    
    isolate({
      ind_periods<- ggplot(na.omit(individual_period_peaks()), aes(x=condition, y=period, colour = condition)) +
        geom_boxplot(alpha=0.7)+
        geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, aes(fill=condition))+
        labs(y="Circadian period [h]", x="") +  #adds/removes axis lables
        theme(legend.title=element_blank())+ #removes legend title
        theme_bw()+
        theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(color="black", size=16))+         #axis title
        theme(legend.title = element_blank())+
        theme(legend.text = element_text(size=14))+
        labs(title= "Circadian period peaks of rhythmic flies")+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(legend.position="none")+
        scale_color_manual(values=Plot_colors())
      
      ind_periods
    })
  })
  
# Box plot of individual period strengths  
  output$ind_period_strength <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    geom_dotplot(binaxis = "y", stackdir = "center")
    if (input$refresh_3 == 0)
      return()
    
    isolate({
      ind_strength<- ggplot(na.omit(individual_period_peaks()), aes(x=condition, y=Act_Sig_ratio, colour = condition)) +
        geom_boxplot(alpha=0.7)+
        geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, aes(fill=condition))+
        labs(y="Qp.act/Qp.sig", x="") +  #adds/removes axis lables
        theme(legend.title=element_blank())+ #removes legend title
        theme_bw()+
        theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(color="black", size=16))+         #axis title
        theme(legend.title = element_blank())+
        theme(legend.text = element_text(size=14))+
        labs(title= "Circadian period strength of rhythmic flies")+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(legend.position="none")+
        scale_color_manual(values=Plot_colors())
      
      ind_strength
    })
  })
  
  
  
  # Table of averaged individual period peaks.   
  output$mean_of_period_peaks_by_condition <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_3 == 0)
      return()
    
    isolate({mean_period_peaks_by_condition()})
  })
  
  # Table of period peaks called from mean periodograms.    
  output$period_peaks <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_3 == 0)
      return()
    
    isolate({period_peaks()})
  })
  
  ### Circadian Links ####
  
  output$download_mean_period_by_condition <- downloadHandler(
    filename = function() {
      paste("Mean_periodogram_data_by_condition-", Sys.Date(),  ".csv", sep="")
    },
    content = function(file) {
      write.csv(mean_period_by_condition(), file)
    }
  )
  
  output$download_individual_period_peaks <- downloadHandler(
    filename = function() {
      paste("Individual_fly_period_peaks-", Sys.Date(),  ".csv", sep="")
    },
    content = function(file) {
      write.csv(individual_period_peaks(), file)
    }
  )  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

