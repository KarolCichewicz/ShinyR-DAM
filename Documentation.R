column(8,
       tags$body(
         h1(strong('General information')),
         br(),
         
         tags$div(
           tags$p('ShinyR-DAM analyzes locomotor activity, sleep and circadian rhythmicity data recorded by the TriKinetics Drosophila 
                  Activity Monitor (DAM) system. It is designed to summarize individual fly data from the condition-monitor 
                  channel layout set by the experimenter. Analyzed data is presented in plots, tables, and 
                  CSV files are available for download. These can be opened with a spreadsheet program for further statistical analysis.
                  '), 
           style = "font-size: 21px;"),
         
         tags$div(
           tags$p('The design of this program is inspired by a classic circadian free run paradigm, 
                  which consists of a few days of entrainment, synchronizing animals to a standard 
                  24 h cycle of 12 h of day and 12 h of night (LD), followed by a period of constant darkness (DD), 
                  allowing animals to express their circadian period of sleep and activity. However, having LD and DD days in a 
                  dataset is not required by the program. Locomotor activity and other metrices can be measured 
                  separately for LD and DD, but circadian period analysis is available only for DD days.
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
         
         h3(strong('Source code and test files:')),
         tags$div(
           tags$p('Source code and test files are available through GitHub at:'), 
           style = "font-size: 19px"),
         
         tags$div(
           tags$a(href="https://github.com/KarolCichewicz/ShinyR-DAM", 
                  "ShinyR-DAM GitHub repository"), style = "font-size: 19px"),
         tags$div(
           tags$a(href="https://github.com/KarolCichewicz/DAMSystem2-Channel-to-DAMSystem3-Monitor-File-Converter", 
                  "DAMSystem2-Channel to DAMSystem3-Monitor File Converter GitHub repository"), style = "font-size: 19px"),
         br(),
         
         tags$div(
           tags$p('Please read the repository README.md files for more details.'), 
           style = "font-size: 19px"),
         
         br(),  
         
         
         
         h2(strong('Settings and Daily Locomotor Activity Analysis')),
         
         
         h3(strong('Program usage')),
         
         tags$div(
           tags$p('ShinyR-DAM is hosted on the shinyapps.io server with limited total time of usage to 500 hours per month for all users.
                  Because of this limit we restrict program idle time to 15 min. 
                  Please make sure that you saved your data before 15 min of ShinyR-DAM inactivity, or your analysis will be lost.'),
           style = "font-size: 19px"),
         
         
         tags$div(
           tags$p('ShinyR-DAM operates on a server with 8 GB of memory. If analyzing a very large dataset causes the program to freeze, 
                  you may be exceeding the memory limit. Please divide your monitor files into subsets, limit the range of days, 
                  or deploy ShinyR-DAM on a local computer with more memory using RStudio.'),
           style = "font-size: 19px"),
         
         tags$div(
           tags$p('Updates will also cause ShinyR-DAM to freeze. We will try to update the program when no one uses it. 
                  If you cannot access ShinyR-DAM, please try again in a few minutes.'),
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
             'Within the analyzed period of dates ShinyR-DAM scans monitor files status column for errors, so no pre-filtering 
             or scanning with DAMFileScan is necessary. The maximum size of files that can be uploaded for the analysis is limited 
             to 300 MB. There is no restriction on file names.'), 
           style = "font-size: 19px"),
         
         h3(strong('Experiment layout')),
         
         tags$div(
           tags$p('DAM system users typically test multiple replicates of flies in a single experimental condition: genotype, treatment, etc.. 
                  The settings panel is designed similar to a typical lab notebook notes containing information about the experiment.'), 
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
                   throughout the analysis into plots and tables. You can review the condition 
                   assignment in a Condition/Monitor layout table, or in any of the CSV files 
                   available in the Settings and Daily Locomotor Activity tab.")),
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
                lethality/viability, a user can set a separate condition for these channels.'), 
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
         tags$p('Activity profiles and sleep profiles can be plotted on two X axis time scales:'), 
         style = "font-size: 19px"),
       tags$div(tags$ul(
         tags$li(" Time of day - data acquisition computer system clock time,"),
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
                a daily activity threshold for identifying dead flies and automatically removes them from the analysis. 
                Flies with less counts than the set threshold during any day in the experiment are excluded from the analyssi.
                The number of living and dead flies is shown in Locomotor activity per Condition tables.
                '), 
         style = "font-size: 19px"),
       
       
       tags$h3(strong('DAM system data acquisition frequency')),
       tags$div(
         tags$p('The DAM system records sums of counts over a Reading Interval. Most of the ShinyR-DAM functions can analyze data recorded 
                at any Reading Interval, or DAM system data acquisition frequency. 
                Although, Sleep Analysis requires precisely 1 min Reading Intervals, Circadian Period Analysis requires 
                Reading Intervals  >= 1 min. We recommend setting the DAM system to 1 min Reading Interval as it allows 
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
                 what monitors are affected, and italso shows the first few rows of the affected monitor data.  This error message also includes 
                 a brief suggestion on how to proceed with an error.  To learn more about DAM system error codes please read: "),
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
       
       tags$h3(strong('Data values')),
       tags$div(
         tags$p('ShinyR-DAM presents average values or activity/sleep/circadian period per single fly, unless stated otherwise.
                '), 
         style = "font-size: 19px"),
       
              
       tags$h3(strong('Error bars')),
       tags$div(
         tags$p('All error bars plotted by ShinyR-DAM are standard errors of the mean (SEM). 
                Error bars represent variation between individuals, not variation between days in the experiment. 
                Variation between days is visualized in the Locomotor activity by day plot.
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
       
       tags$h3(strong('Nighttime/Daytime activity ratio')),
       tags$div(
         tags$p('Nighttime/Daytime activity ratio provides information about the nocturnality. 
                It calculates an average ratio of nighttime to daytime activity for each individual fly in a condition. 
                Note that it is not calculated as a simple fraction of Daytime activity in LD to Nighttime 
                activity in LD, which are averaged per condition.
                '), 
         style = "font-size: 19px"),
       
       
       
       tags$h3(strong('Activity profiles')),
       tags$div(
         tags$p('Activity profiles display the average number of counts per fly calculated over the
                Averaging window of activity profiles. The averaging window must be 
                set to one of the multiples of a DAM system data acquisition frequency. Otherwise, plots 
                will not be produced.
                '), 
         style = "font-size: 19px"),
       
       
       
       tags$h3(strong('Sleep analysis')),
       tags$div(
         tags$p('Sleep is defined as a continuous 5 min period of inactivity. Sleep analysis requires DAM system data 
                acquisition frequency to be set to 1 min. If acquisition frequency is different, the analysis is not available. 
                Sleep events are identified using a sliding window algorithm of 5 min width and 1 min sliding interval.
                A sleep event is detected if all 5 activity readings in a window equal 0. Sleep events are then averaged over 
                all individual flies in a condition, and over the Interval for average sleep profile, which sets the resolution 
                for sleep profile plots. Sleep = 1 means that all flies were inactive. Sleep = 0 means that all flies scored 
                at least 1 count every 5-min within the window of Interval for the average sleep profile.
                '), 
         style = "font-size: 19px"),
       
       
       tags$h3(strong('Actograms')),
       tags$div(
         tags$p('Actograms visualize locomotor activity throughout the progress of an experiment. The Y axis 
                displays mean, median, or individual count values. The count values can be binned using 
                a Bin actogram data points into average slider. To accommodate longer data acquisition frequencies 
                with higher values of counts recorded per interval, or hyperactive flies, the Y axis limit is adjustable 
                with a Max number of counts displayed slider. Also, the height, width and color of actograms are adjustable.
                A user can plot single plotted or double plotted actograms. Also, a user can choose to plot 
                individual actograms of dead flies, to verify the value of Threshold of counts per day for identifying dead flies.
                Because of the limitations of the shiny.io server plotting many individual actograms may not work. The maximum number 
                of plots that can be displayed at once depends on the number of days in the experiment and the number of monitor files.
                If the threshold is exceeded, ShinyR-DAM will not freeze, but it will not display the output either. In this case, 
                please consider limiting the number of monitor files you upload for the analysis, or try using ShinyR-DAM on your local computer.   
                
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
                chiSqPeriodogram function by allowing users to specify the period testing range, and 
                period testing resolution. To allow for filtering out arrhythmic individuals, we 
                provided an adjustable rhythmicity threshold [Qp.act/Qp.sig] filter. Individuals with 
                Qp.act/Qp.sig below that threshold are removed from the analysis. 
                '), 
         style = "font-size: 19px"),
       
       tags$div(
         tags$a(href="https://CRAN.R-project.org/package=xsp", " 'xsp' CRAN repository"), style = "font-size: 19px"), 
       
       tags$br(),
       
      
       hr(),
       
       tags$h3(strong('Acknowledgements')),
       tags$div(
         tags$p('The authors would like to acknowledge the authors of R packages we used in the ShinyR-DAM code: 
          colourpicker - (Dean Attali, 2016), 
          data.table - (Matt Dowle and Arun Srinivasan, 2017),  
          dplyr - (Hadley Wickham and Romain Francois, 2016),  
          ggplot2 - (Hadley Wickham, 2009, p. 2), 
          gridExtra - (Baptiste Auguie, 2016), 
          gtools - (Gregory R. Warnes, Ben Bolker and Thomas Lumley, 2015), 
          Kmisc - (Kevin Ushey, 2013), 
          lubridate - (Garrett Grolemund, Hadley Wickham, 2011), 
          plyr - (Hadley Wickham, 2011), scales - (Hadley Wickham, 2016), 
          shiny - (Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson, 2017), 
          xsp - (Hitoshi Iuchi and Rikuhiro G. Yamada, 2017), 
          zoo - (Achim Zeileis and Gabor Grothendieck, 2005).  
           '), 
         style = "font-size: 19px"),
       
       tags$br(),
       tags$div(
         tags$p('We would like to thank Mark Spencer, Priscilla Erickson, PhD, Herman Wijnen, PhD, and Stephen Turner, PhD for critical feedback and guidance.   
             '),
         style = "font-size: 19px"),
       
       hr(),
       
       
       tags$h3(strong('Version changelog')),
       tags$div(
         tags$h3(strong('2.0')),
         tags$ul(
           tags$li("Updated interface with sidebar menus"), 
           tags$li("Added overlapping activity and sleep profiles"), 
           tags$li("Option for hiding or showing SEM error bars in activity and sleep profiles"),
           tags$li("Option for displaying activity and sleep profiles on data acquisition or Zeitgeber time scale"), 
           tags$li("Added activity and sleep daily profiles"),
           tags$li("Added selection of displaying mean, median or individual actograms"), 
           tags$li("Added choice of displaying dead fly individual actograms"),
           tags$li("Added Double Plotted actograms"), 
           tags$li("Binning actograms into average values"),
           tags$li("Periodograms properly respond to filtering by rhythmicity threshold"),
           tags$li("To increase the code transparency each ShinyR-DAM tab code was split into server and ui files and sourced in the app")
         ),
         tags$h3(strong('2.1')),
         tags$ul(
           tags$li("Disambiguation of column names in tables and CSV files"),
           tags$li("Updated Documentation")
         ),
         style = "font-size: 19px")
       
       
       )