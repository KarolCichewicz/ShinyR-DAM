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
data_freq <- reactive(1440/ as.numeric(input$data_recording_frequency))

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
  m <- monitor_data_status_1()
  
### Yes, I know it's a litttle messy, but it works!  
    
  if(all(p$value == 1)){ 
    "Data correct. All DAM data status codes are 1"
  }else {
    print("Error: Data incorrect. Data status codes other than 1 were detected in one or more Monitor files")
    print("Make sure you correctly selected the dates to analyze,")
    print("excluding day 0 when you connected your monitors to the system,")
    print("and the day you transfered your monitor files from the computer recording the data.")
    print("This program can only analyze FULL DAYS of locomotor activity")
    print("The following status codes were detected")
    print(unique(filter(select(p, value), value != 1)$value))
    print("Errors were found in this number of rows (minutes of the recorded data if the acquisition frequency is 1 min):")  
    print(nrow(filter(p, value != 1)))
    print("Errors were found in the following monitors:")  
    print(unique(filter(melt_(m), value != 1))$variable)
    print("The first ten rows of data with errors:")
    print(head(filter(p, value != 1))[,c(1,2,4)], 10)
    print("More about the DAM system error codes can be found at: http://www.trikinetics.com/Downloads/DAMSystem%20User's%20Guide%203.0.pdf")
    print("Please check the condition of your DAM hardware, considering the nature of error codes")
    print("If errors occur in low number of records, you may consider ignoring them.") 
    print("If errors are not associated with missing data records, they will not substantially affect the analysis.")
    print("Missing data records will trigger the accompanying error printed in the Data and settings validation:")
    print("Error: The number of minutes in the recorded data does not match the number of days specified in settings.")
    
  }
})


# Checks if the number of Monitors and the number of specified conditions match. Sanity check.
Monitors_vs_conditions_check <- reactive({
  cond <- conditions()
  fil <- input$file1
  
  if(length(cond) == length(fil$name)*32){ 
    ("Input data correct. Total number of flies in Conditions matches the total number of channels in the uploaded Monitors files")
  }else {
    print("Error: Input data or settings are incorrect; Total number of flies in Conditions doesn't match the total number of channels in the uploaded Monitor files")
    print("Number of flies specified in conditions:")
    print(length(conditions()))
    print("Number of channels in the uploaded Monitor files:")
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
    print("LD and DD days cannot overlap. Please correct your settings.")
    print("LD days:")
    print(LD())
    print("DD days")
    print(DD())
  }
})

# Checks for missing data records or incorrectly set recording frequency.   
Data_range_correctness <- reactive({
  
  
  if(nrow(na.omit(s_1())) == data_freq()*number_of_days()){ 
    print("Input data correct. The number of minutes in the recorded data matches the number of days specified in settings.")
  }else {
    print("Error: The number of minutes in the recorded data does not match the number of days specified in settings.")
    print("Your data includes an incomplete day of recorded data, or data acquisition frequency is set to an incorrect value.") 
    print("Please adjust your settings or check your monitor files for missing records")
    nrow_each <- function(x)
    {nrow(filter(na.omit(s_1()), date==x))}
    print(data.frame(Dates_of_experiment=unique(s_1()$date), weekday=weekdays(unique(s_1()$date)), Number_of_data_readings=sapply(unique(s_1()$date), FUN=nrow_each)))
    print("Based on the acquisition frequency setting, ShinyR-DAM expects the following number of data readings per day:")
    print(data_freq())
    print("If the damage is not substantial to the interpretation of the experiment,")
    print("you may consider manually filling the missing records in monitor files with 0s.")
    print("The following data frame showns the number of data records in each channel.")
    print("It should give you a clue what monitor files are affected")
    
    d <- melt(colSums(!is.na(s_1())))
    
    print(d)
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
LD_start <- reactive(as.POSIXct(strptime(input$light_onset_time, format="%H:%M")))
start <- reactive(as.POSIXct(strptime("00:00", format="%H:%M")))
end <- reactive(as.POSIXct(strptime("23:59", format="%H:%M")))
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
  
  #s8 <<- s_8()
  #ac <<- all_channels()
  
  d <- s_8()
  d[with(d, order(date, Dec_time)),]
  d
})

# roll founction calculates sum of counts per day. data_freq is the number of minutes in each day.    
roll <- reactive({function(x){
  library(dplyr)
  library(zoo)

  rollapply(select(s_9(), x), width = data_freq(), by = data_freq(), FUN = sum, align = "left")}
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
  p <- p[c("Condition", "variable", "Light_cycle", "date", "value")]
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
  k$Condition <- factor(k$Condition, levels = unique(conditions()))
  k <- arrange(k, Condition)
  k
})

# Object summarizing locomotor activity in LD and DD
general_summary <- reactive({
  
  #sbfa <<- summary_by_fly_alive()
  #mbda <<- melted_by_day_alive() 
  #mbd <<- melted_by_day()
  #abd_1 <<- activity_by_day_1()
  
  
  withProgress(message = 'Calculating summary by Condition', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("In progress"))

    d <- ddply(summary_by_fly_alive(), c("Condition", "Light_cycle"), summarise,
               Mean = mean(mean_value), SD = sd(mean_value),
               SEM = sd(mean_value)/sqrt(length(mean_value)),
               N_of_alive_flies=length(Condition))
    
    d <- cbind(d, N_of_dead_flies=rep(all_flies_count()$N_all_flies, each=length(unique(d$Light_cycle))) - d$N_of_alive_flies)
    d$N_of_all_flies <- d$N_of_alive_flies + d$N_of_dead_flies
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



### Monitor layout table ###   
output$Monitor_layout <- renderTable({
  
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