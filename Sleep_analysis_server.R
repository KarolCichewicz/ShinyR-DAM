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
  d <- data.frame(s_9()[1:(nrow(s_9())-4),  c(1:4,as.numeric(length(colnames(s_9())) - 2), as.numeric(length(colnames(s_9())) - 1), as.numeric(length(colnames(s_9())) - 3), as.numeric(length(colnames(s_9())) - 4), as.numeric(length(colnames(s_9()))))], melted_sleep()[,c(2:3)], row.names=NULL)
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

### WORK IN Progress


# Filters only alive flies
sleep_df_matched_alive <- reactive({filter(sleep_df_matched_1(), Channel %in% alive())})



# Filters only LD data
sleep_df_matched_alive_LD <- reactive({filter(sleep_df_matched_alive(), Light_cycle=="LD")
})


# Calculates mean sleep and sem in each timepoint for each Condition
mean_of_sleep_per_condition <- reactive({
  withProgress(message = 'Calculating sleep 3/3', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("In progress"))
    
    
    DT <- data.table(sleep_df_matched_alive_LD())
    d <- DT[,list(mean_of_sleep_counts=as.numeric(mean(sleep_counts)), sem=as.numeric(sd(sleep_counts)/sqrt(length(sleep_counts)))), by=c("Dec_time", "Dec_ZT_time", "Condition")]
    d$Condition <- factor(d$Condition, levels = unique(conditions()))
    d <- arrange(d, Condition)
    d <- na.omit(d)
    d
  })})

# Calculates vectors of average values over X min non overlapping windows. Originaly it used 30 min windows, I later made it adjustable.
mean_of_sleep_per_condition_30_min <- reactive({rollapply(mean_of_sleep_per_condition()$mean_of_sleep_counts, width = as.numeric(input$sleep_profile_window), by = as.numeric(input$sleep_profile_window), FUN = mean)})

sem_of_sleep_per_condition_30_min <- reactive({rollapply(mean_of_sleep_per_condition()$sem, width = as.numeric(input$sleep_profile_window), by = as.numeric(input$sleep_profile_window), FUN = mean)})   

# Adds mean and sem averaged columns into the original data frame by repeating each element X times
mean_of_sleep_per_condition_1 <- reactive({
  withProgress(message = 'Calculating sleep profiles', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("In progress"))
    
    # It was necessary to add NAs into a dataframe, if average for one day was calculated
    ff <- nrow(mean_of_sleep_per_condition()) - length(rep(mean_of_sleep_per_condition_30_min(), each=as.numeric(input$sleep_profile_window)))
    
    d <- mean_of_sleep_per_condition()
    d$mean_binned_sleep <- round(c(rep(mean_of_sleep_per_condition_30_min(), each=as.numeric(input$sleep_profile_window)), rep(NA, ff)), 10)
    d$sem_binned_sleep <- round(c(rep(sem_of_sleep_per_condition_30_min(), each=as.numeric(input$sleep_profile_window)), rep(NA, ff)), 10)
    d
  })})

# Sampling the data frame for every Xth row
sleep_30min_mean_2 <- reactive({mean_of_sleep_per_condition_1()[seq(1, nrow(mean_of_sleep_per_condition_1()), by = as.numeric(input$sleep_profile_window)),]})

# Drops unnecessary columns
sleep_30min_mean <- reactive({
  p <- select(sleep_30min_mean_2(), Dec_time, Dec_ZT_time, Condition, mean_binned_sleep, sem_binned_sleep)
  p$Condition <- factor(p$Condition, levels = unique(p$Condition))
  p <- arrange(p, Condition)
  p <- na.omit(p)
  p
})

# Quantitative sleep
# mean sum of sleep per condition
# Calculates sum of sleep per individual per day in Light and Dark phase
sleep_per_individual_light_phase <- reactive({
  p <- sleep_df_matched_alive_LD()
  p <- merge(p, Light_status_df(), by = "Dec_ZT_time", all.x=TRUE, all.y=TRUE)
  DT<- data.table(p)
  d <- DT[,list(mean_sleep_per_ind=as.numeric(mean(sleep_counts))), by=c("Channel","Condition", "Light_status")]
  d
})

sleep_per_condition <- reactive({
  DT<- data.table(sleep_per_individual_light_phase())
  d <- DT[,list(mean_of_sleep_per_condition=as.numeric(mean(mean_sleep_per_ind)), 
                sem=as.numeric(sd(mean_sleep_per_ind)/sqrt(length(mean_sleep_per_ind)))), 
          by=c("Condition", "Light_status")]
  
  d$Condition <- factor(d$Condition, levels = unique(conditions()))
  d <- arrange(d, Condition)
  d <- na.omit(d)
  d
})

sleep_per_condition_Day <- reactive(filter(sleep_per_condition(), Light_status=="Day")) 
sleep_per_condition_Night <- reactive(filter(sleep_per_condition(), Light_status=="Night"))


# Sleep data link average profile    
output$download_average_sleep_profiles_in_LD <- downloadHandler(
  filename = function() {
    paste("Average_sleep_profiles_in_LD", ".csv", sep="")
  },
  content = function(file) {
    write.csv(sleep_30min_mean(), file)
  }
)

# Sleep link - individual fly data     
output$download_ind_day_night_sleep <- downloadHandler(
  filename = function() {
    paste("Individual_day_night_sleep", ".csv", sep="")
  },
  content = function(file) {
    write.csv(sleep_per_individual_light_phase(), file)
  }
)    


output$download_daily_sleep <- downloadHandler(
  filename = function() {
    paste("Daily_sleep_profiles",  ".csv", sep="")
  },
  content = function(file) {
    write.csv(mean_of_sleep_per_condition_by_day(), file)
  }
)  







# Defines the max sleep value displayed on the profile plot  
slp_profile_max_y <- eventReactive(input$refresh_2,{
  input$max_slp_value
}) 






############ Sleep by day plot ############  WORK zone



# Calculates mean sleep and sem in each timepoint for each Condition
mean_of_sleep_per_condition_by_day <- reactive({
  withProgress(message = 'Calculating sleep 3/3', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("In progress"))
    
    DT <- data.table(sleep_df_matched_alive())
    d <- DT[,list(mean_of_sleep_counts=as.numeric(mean(sleep_counts)), sem=as.numeric(sd(sleep_counts)/sqrt(length(sleep_counts)))), by=c("Dec_time", "Condition", "date", "Dec_ZT_time")]
    d$Condition <- factor(d$Condition, levels = unique(conditions()))
    d <- arrange(d, Condition)
    d
  })})




output$sleep_profile_by_day <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate({
    
    binning_value <- input$sleep_profile_window / input$data_recording_frequency #Generated a value for binning 
    df_sl <- mean_of_sleep_per_condition_by_day()
    
    
    
    z <- lapply(unique(df_sl$Condition), function(x) {
      pp <- filter(df_sl, Condition == x)
      pp$Order_column <- c(1:nrow(pp))
      binned_sleep <- round(rep(rollapply(pp$mean_of_sleep_counts, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value), 10)
      binned_sem <-   round(rep(rollapply(pp$sem, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value), 10)
      
      p1 <- pp[1:length(binned_sleep),]
      p1$binned_sleep <- binned_sleep
      p1$binned_sem <- binned_sem
      p1
    }
    )
    
    binned_sleep <- rbindlist(z)
    
    binned_sleep$Condition <- factor(binned_sleep$Condition, levels = unique_conditions())
    binned_sleep <- arrange(binned_sleep, Condition)
    
    
    df_sl2 <- binned_sleep[seq(1, nrow(binned_sleep), by = binning_value),] #Reduces the number of rows after binning
    df_sl2$Condition <- factor(df_sl2$Condition, levels = unique_conditions())
    df_sl2 <- arrange(df_sl2, Condition)
    #b1 <- seq(0,length(unique(binned_sleep$Order_column)), (1440/4)/input$data_recording_frequency)
    #b1 <- c(b1, unique(binned_sleep$Order_column)[length(unique(binned_sleep$Order_column))])
    
    b1 <- seq(0, 1440 *length(unique(df_sl2$date)), 360)
    
    b2 <-  seq(0, length(unique(binned_sleep$Order_column)), b1[3])
    b2 <- as.vector(b2)
    b2 <- b2[1:length(unique(binned_sleep$date))]
    b2 <- b2*2
    b2 <- b2 + b1[3]
    
    sleep_profile_by_day <- ggplot(df_sl2, aes(x=Order_column, y=binned_sleep, ymax=3, ymin=0, colour = Condition)) +
      geom_line()+
      geom_point()+ 
      theme_bw()+
      
      geom_vline(xintercept = c(seq(0,(df_sl2$Order_column)[length(unique(df_sl2$Order_column))], 1440), b1[length(b1)])) + (
        
        if (input$date_disp_1 == "Yes")(
          annotate("text", x= b2, y=input$max_slp_value, label= unique(df_sl2$date), size = 7)
        )) +
      
      scale_x_continuous(breaks = b1, 
                         labels=c(rep(c("0", "6", "12", "18"), times=length(unique(df_sl2$date))), "0"))+
      
      labs(title= "", x= "Time [H]", y = "Sleep")+ 
      coord_cartesian(ylim=c(0,as.numeric(slp_profile_max_y())))+
      scale_colour_manual(values=Plot_colors())+
      theme(legend.text=element_text(size=18))+
      theme(legend.title = element_text(size=18))+
      theme(axis.text.x=element_text(hjust=0.5, size=15))+
      theme(axis.text.y=element_text(size=15))+
      theme(axis.title=element_text(size=18))+
      guides(colour = guide_legend(override.aes = list(size=1))) + (
        if (input$display_error_bars_sl == "Yes")(
          geom_errorbar(aes(ymax=binned_sleep+binned_sem ,ymin=binned_sleep-binned_sem), width=0.3)
        ))
    
    sleep_profile_by_day
    
  })
})


output$sl_profiles_by_day <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate(
    plotOutput("sleep_profile_by_day", height = input$sl_profile_height, 
               width = input$sl_profile_width)
  )
})  


output$sleep_profile_by_day_split <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate({
    binning_value <- input$sleep_profile_window / input$data_recording_frequency #Generated a value for binning 
    df_sl <- mean_of_sleep_per_condition_by_day()
    
    z <- lapply(unique(df_sl$Condition), function(x) {
      pp <- filter(df_sl, Condition == x)
      pp$Order_column <- c(1:nrow(pp))
      binned_sleep <- round(rep(rollapply(pp$mean_of_sleep_counts, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value), 10)
      binned_sem <-   round(rep(rollapply(pp$sem, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value), 10)
      
      p1 <- pp[1:length(binned_sleep),]
      p1$binned_sleep <- binned_sleep
      p1$binned_sem <- binned_sem
      p1
    }
    )
    
    binned_sleep <- rbindlist(z)
    
    binned_sleep$Condition <- factor(binned_sleep$Condition, levels = unique_conditions())
    binned_sleep <- arrange(binned_sleep, Condition)
    
    
    df_sl2 <- binned_sleep[seq(1, nrow(binned_sleep), by = binning_value),] #Reduces the number of rows after binning
    df_sl2$Condition <- factor(df_sl2$Condition, levels = unique_conditions())
    df_sl2 <- arrange(df_sl2, Condition)
    
    b1 <- seq(0, 1440 *length(unique(df_sl2$date)), 360)
    
    b2 <-  seq(0, length(unique(binned_sleep$Order_column)), b1[3])
    b2 <- as.vector(b2)
    b2 <- b2[1:length(unique(binned_sleep$date))]
    b2 <- b2*2
    b2 <- b2 + b1[3]
    
    
    
    
    sleep_profile_by_day_x <-   lapply(unique_conditions(), function(x) ggplot(filter(df_sl2, Condition==x), aes(x=Order_column, y=binned_sleep, ymax=3, ymin=0, colour = Condition)) +
      geom_line()+
      geom_point()+ 
      theme_bw()+
      
      geom_vline(xintercept = c(seq(0,(df_sl2$Order_column)[length(unique(df_sl2$Order_column))], 1440), b1[length(b1)])) + (
        if (input$date_disp_1 == "Yes")(
          annotate("text", x= b2, y=input$max_slp_value, label= unique(df_sl2$date), size = 7)
        )) +
      scale_x_continuous(breaks = b1, 
                         labels=c(rep(c("0", "6", "12", "18"), times=length(unique(df_sl2$date))), "0"))+
      
      # displays day annotations  
      labs(title= "", x= "Time [H]", y = "Sleep")+ 
      coord_cartesian(ylim=c(0,as.numeric(slp_profile_max_y())))+
      scale_colour_manual(values=Plot_colors())+
      theme(legend.text=element_text(size=18))+
      theme(legend.title = element_text(size=18))+
      theme(axis.text.x=element_text(hjust=0.5, size=15))+
      theme(axis.text.y=element_text(size=15))+
      theme(axis.title=element_text(size=18))+
      guides(colour = guide_legend(override.aes = list(size=1))) + (
        if (input$display_error_bars_sl == "Yes")(
          geom_errorbar(aes(ymax=binned_sleep+binned_sem ,ymin=binned_sleep-binned_sem), width=0.3)
        ))
      )
    
    
    marrangeGrob(sleep_profile_by_day_x, ncol=1, nrow = length(unique(conditions())), top ="")
    
  })
})


output$sl_profiles_by_day_split <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate(
    plotOutput("sleep_profile_by_day_split", height = (input$sl_profile_height * as.numeric(length(unique_conditions()))), 
               width = input$sl_profile_width)
    
  )
})





# Generates average sleep profiles in LD   
output$sleep_profiles_av <- renderPlot({
  withProgress(message = 'Drawing sleep profiles ZT', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("In progress"))
    
    
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    

    isolate({
      if (input$sl_scale == "Data_acq") {
        scale <- "Dec_time"
      } else {
        scale <- "Dec_ZT_time"
      }
      
      brakes_vector <- c(0, 360, 720, 1080, 1440)
      
      sleep_plots_30_min <-    ggplot(sleep_30min_mean(), aes(get(scale), y=mean_binned_sleep, ymax=1, ymin=0, colour=Condition)) +
                                     geom_point() +
                                     geom_line()+
                                     theme_bw()+
        geom_vline(xintercept = c(0, 1440))+
        scale_x_continuous(breaks = brakes_vector, labels=c("0", "6", "12", "18", "0"))+
        labs(title= "", x= "Time [H]", y = "Sleep")+ 
        coord_cartesian(ylim=c(0,as.numeric(slp_profile_max_y())))+
        scale_colour_manual(values=Plot_colors())+
        theme(legend.text=element_text(size=18))+
        theme(legend.title = element_text(size=18))+
        theme(axis.text.x=element_text(hjust=0.5, size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title=element_text(size=18))+
        guides(colour = guide_legend(override.aes = list(size=1))) + (
          if (input$display_error_bars_sl == "Yes")(
            geom_errorbar(aes(ymax=mean_binned_sleep-sem_binned_sleep,ymin=mean_binned_sleep+sem_binned_sleep), width=0.3)
          ))
      
      
      sleep_plots_30_min
    })
  })})        




output$average_LD_sleep_profiles <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate(
    plotOutput("sleep_profiles_av", height = input$sl_profile_height, 
               width = input$sl_profile_width)
  )
})  




output$sleep_profiles_av_split <- renderPlot({
  withProgress(message = 'Drawing sleep profiles ZT', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("In progress"))
    
    
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    
    isolate({
      if (input$sl_scale == "Data_acq") {
        scale <- "Dec_time"
      } else {
        scale <- "Dec_ZT_time"
      }
      
      # Finding brake points for the plot
      
      brakes_vector <- c(0, 360, 720, 1080, 1440)
      
      
      sleep_plots_30_min_x <- lapply(unique_conditions(), function(x) ggplot(filter(sleep_30min_mean(), Condition == x), aes(get(scale), y=mean_binned_sleep, ymax=1, ymin=0, colour=Condition)) +
        geom_point() +
        geom_line()+
        theme_bw()+
        geom_vline(xintercept = c(0, 1440))+
        scale_x_continuous(breaks = brakes_vector, labels=c("0", "6", "12", "18", "0"))+
        labs(title= "", x= "Time [H]", y = "Sleep")+ 
        coord_cartesian(ylim=c(0,as.numeric(slp_profile_max_y())))+
        scale_colour_manual(values=Plot_colors())+
        theme(legend.text=element_text(size=18))+
        theme(legend.title = element_text(size=18))+
        theme(axis.text.x=element_text(hjust=0.5, size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title=element_text(size=18))+
        guides(colour = guide_legend(override.aes = list(size=1))) + (
          if (input$display_error_bars_sl == "Yes")(
            geom_errorbar(aes(ymax=mean_binned_sleep-sem_binned_sleep,ymin=mean_binned_sleep+sem_binned_sleep), width=0.3)
          ))
      )
      
      marrangeGrob(sleep_plots_30_min_x, ncol=1, nrow = length(unique(conditions())), top ="")
      
    })
  })})        




output$average_LD_sleep_profiles_split <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate(
    plotOutput("sleep_profiles_av_split", height = (input$sl_profile_height * as.numeric(length(unique_conditions()))), 
               width = input$sl_profile_width)
    
  )
})  




#Total sleep

sleep_per_c <- reactive({
  DT<- data.table(sleep_per_individual_light_phase())
  d <- DT[,list(mean_of_sleep_per_condition=as.numeric(mean(mean_sleep_per_ind)), 
                sem=as.numeric(sd(mean_sleep_per_ind)/sqrt(length(mean_sleep_per_ind)))), 
          by=c("Condition")]
  
  d$Condition <- factor(d$Condition, levels = unique(conditions()))
  d <- arrange(d, Condition)
  d <- na.omit(d)
  d
})

# Total sleep
output$Total_sleep <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate({
    barplot_sleep_total<- ggplot(sleep_per_c(), aes(x=Condition, y=mean_of_sleep_per_condition, fill=Condition, width=.5)) + 
      geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
      labs(x="", y="Average sleep per individual") +                    #adds/removes axis lables
      theme_bw()+
      geom_errorbar(data=sleep_per_c(), aes(ymax=mean_of_sleep_per_condition+sem,ymin=mean_of_sleep_per_condition-sem), width=0.1)+
      scale_x_discrete(limits=unique_conditions())+
      theme(legend.text=element_text(size=16))+
      theme(legend.position="none")+
      theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
      labs(title= "Total sleep in LD", size= 14)+
      theme(plot.title = element_text(size = rel(2), hjust=0.5))+
      theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
      theme(axis.title.y = element_text(size=14))+
      scale_fill_manual(values=Plot_colors())+
      coord_cartesian(ylim=c(min(sleep_per_c()$mean_of_sleep_per_condition)-(sleep_per_c()$sem), max(sleep_per_c()$mean_of_sleep_per_condition)+sleep_per_c()$sem))
    
    barplot_sleep_total
    
  })
})

# Table - daytime sleep    
output$Total_sleep_table <- renderTable({
  go_on_files()
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_2 == 0)
    return()
  
  isolate({sleep_per_c()})
})





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


#Sleep Bout analysis


  slp1 <- reactive({
    
    slp <- sleep_df_matched_alive_LD()
    
    slp1 <- data.frame(slp , bout=sequence(rle(as.character(slp$sleep_counts))$lengths))
    
    # Splits the sleep bouts into separate lists
    d <- split(slp1$bout, cumsum(c(1, diff(slp1$bout) != 1)))
    
    # finds the max value in each sleep bout, which is the sleep bout length
    p <- as.numeric(lapply(d, function(x) x[which.max(abs(x))]))
    
    n <- lapply(p, function(i) {
      c(i, rep(NA, i-1))
    })
    
    slp2 <- data.frame(slp1, bout_length = unlist(n))
    slp2
    
  })
  
  
  
  
  mean_act_slp_bout_length <- reactive({
    
    
    DT <- data.table(slp1())
    d <- DT[,list(mean_sleep_bout_length_ind=as.numeric(mean(na.omit(bout_length)))
    ),
    by=c("Channel", "Condition", "Light_cycle", "sleep_counts")]
    
    d$Bout_type <- ifelse(d$sleep_counts == 1, "sleep", "activity")  
    
    DT1 <- data.table(d)
    d <- DT1[,list(Mean_bout_length=as.numeric(mean(mean_sleep_bout_length_ind)),
                   SEM=as.numeric(sd(mean_sleep_bout_length_ind)/sqrt(length(mean_sleep_bout_length_ind)))
    ), by=c("Condition", "Light_cycle", "Bout_type")]
    d$Condition <- factor(d$Condition, levels = unique(conditions()))
    d <- arrange(d, Condition)
    d
    
  })

  
  mean_slp_bout_length <- reactive({filter(mean_act_slp_bout_length(), Bout_type == "sleep")})
  mean_act_bout_length <- reactive({filter(mean_act_slp_bout_length(), Bout_type == "activity")})
  
### Sleep Bout Length plot  
  output$bout_length_sleep_plot <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({
    
      barplot_sleep_bout_length <- ggplot(mean_slp_bout_length(), aes(x=Condition, y=Mean_bout_length, fill=Condition, width=.5)) + 
        geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
        labs(x="", y="Average bout length [min]") +                    #adds/removes axis lables
        theme_bw()+
        geom_errorbar(data=mean_slp_bout_length(), aes(ymax=Mean_bout_length+SEM,ymin=Mean_bout_length-SEM), width=0.1)+
        scale_x_discrete(limits=unique_conditions())+
        theme(legend.text=element_text(size=16))+
        theme(legend.position="none")+
        theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
        labs(title= "Sleep bout length in LD", size= 14)+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(size=14))+
        scale_fill_manual(values=Plot_colors())
       
      barplot_sleep_bout_length
      
    })
  })
  
  

    
 ### Sleep Bout Length table        
  output$Bout_length_sleep_table <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({mean_slp_bout_length()})
  })     
    
  
  
  ### Activity Bout Length plot  
  output$bout_length_activity_plot <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({
      
      barplot_activity_bout_length <- ggplot(mean_act_bout_length(), aes(x=Condition, y=Mean_bout_length, fill=Condition, width=.5)) + 
        geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
        labs(x="", y="Average bout length [min]") +                    #adds/removes axis lables
        theme_bw()+
        geom_errorbar(data=mean_act_bout_length(), aes(ymax=Mean_bout_length+SEM,ymin=Mean_bout_length-SEM), width=0.1)+
        scale_x_discrete(limits=unique_conditions())+
        theme(legend.text=element_text(size=16))+
        theme(legend.position="none")+
        theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
        labs(title= "Activity bout length in LD", size= 14)+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(size=14))+
        scale_fill_manual(values=Plot_colors())
      
      barplot_activity_bout_length
      
    })
  })
  
  
  ### Activity Bout Length table        
  output$Bout_length_activity_table <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({mean_act_bout_length()})
    
  })     
  
  
  mean_act_sleep_bout_number <- reactive({
    
    ss <- slp1()
    ss$bout_event <- ifelse(ss$bout_length != "NA", "TRUE", "FALSE")
    
    ss$bout_event[is.na(ss$bout_event)] <- FALSE
    
    DT <- data.table(ss)
    d <- DT[,list(
      bout_number = as.numeric(table(bout_event)["TRUE"]) / length(unique(date))
    ),
    by=c("Channel", "Condition", "Light_cycle", "sleep_counts")]
    
    d$Bout_type <- ifelse(d$sleep_counts == 1, "sleep", "activity") 
    
    DT1 <- data.table(d)
    d <- DT1[,list(Mean_bout_number = as.numeric(mean(bout_number)),
                   SEM=as.numeric(sd(bout_number)/sqrt(length(bout_number)))
    ), by=c("Condition", "Light_cycle", "Bout_type")]
    d$Condition <- factor(d$Condition, levels = unique(conditions()))
    d <- arrange(d, Condition)
    d
    
    
  })
  
  
  mean_slp_bout_number <- reactive({filter(mean_act_sleep_bout_number(), Bout_type == "sleep")})
  mean_act_bout_number <- reactive({filter(mean_act_sleep_bout_number(), Bout_type == "activity")})
  
  
  
  ### Sleep Bout Number plot  
  output$bout_number_sleep_plot <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({
      
      barplot_sleep_bout_number <- ggplot(mean_slp_bout_number(), aes(x=Condition, y=Mean_bout_number, fill=Condition, width=.5)) + 
        geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
        labs(x="", y="Average bout number") +                    #adds/removes axis lables
        theme_bw()+
        geom_errorbar(data=mean_slp_bout_number(), aes(ymax=Mean_bout_number+SEM,ymin=Mean_bout_number-SEM), width=0.1)+
        scale_x_discrete(limits=unique_conditions())+
        theme(legend.text=element_text(size=16))+
        theme(legend.position="none")+
        theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
        labs(title= "Sleep bout number in LD", size= 14)+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(size=14))+
        scale_fill_manual(values=Plot_colors())
      
      barplot_sleep_bout_number
      
    })
  })
  
  ### Sleep Bout Number table        
  output$Bout_number_sleep_table <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({mean_slp_bout_number()})
  })     
  
  
  ### Activity Bout Number plot  
  output$bout_number_activity_plot <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({
      
      barplot_activity_bout_number <- ggplot(mean_act_bout_number(), aes(x=Condition, y=Mean_bout_number, fill=Condition, width=.5)) + 
        geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
        labs(x="", y="Average bout number") +                    #adds/removes axis lables
        theme_bw()+
        geom_errorbar(data=mean_act_bout_number(), aes(ymax=Mean_bout_number+SEM,ymin=Mean_bout_number-SEM), width=0.1)+
        scale_x_discrete(limits=unique_conditions())+
        theme(legend.text=element_text(size=16))+
        theme(legend.position="none")+
        theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
        labs(title= "Activity bout number in LD", size= 14)+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(size=14))+
        scale_fill_manual(values=Plot_colors())
      
      barplot_activity_bout_number
      
    })
  })
  
  
  ### Activity Bout Number table        
  output$Bout_number_activity_table <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({mean_act_bout_number()})
  })     
  
  
  #### Sleep actiivty length ratios
  
  mean_act_sleep_length <- reactive({
    
    DT <- data.table(slp1())
    d <- DT[,list(
      sleep_length = sum(sleep_counts == 1) / length(unique(date)),
      activity_length = sum(sleep_counts != 1) / length(unique(date))
    ),
    by=c("Channel", "Condition", "Light_cycle")]   
    
    DT1 <- data.table(d)
    p <- DT1[,list(Mean_sleep_length = as.numeric(mean(sleep_length)),
                   SEM_sleep_length = as.numeric(sd(sleep_length)/sqrt(length(sleep_length))),
                   Mean_activity_length = as.numeric(mean(activity_length)),
                   SEM_activity_length = as.numeric(sd(activity_length)/sqrt(length(activity_length))),
                   Activity_length_to_sleep_length = as.numeric(mean(activity_length/sleep_length)),
                   SEM_activity_length_to_sleep_length = as.numeric(sd(activity_length/sleep_length)/sqrt(length(activity_length/sleep_length)))
    ),
    by=c("Condition", "Light_cycle")]
    p
  })
  
  
  

  mmm_df <- reactive({
  
  mm <- select(mean_act_sleep_length(), Condition, Light_cycle, Mean_sleep_length, SEM_sleep_length, Mean_activity_length, SEM_activity_length)
  mmm <- melt(mm, id.vars = c("Condition", "Light_cycle"))
  mmm_value <- filter(mmm, variable %in% c("Mean_sleep_length", "Mean_activity_length"))
  mmm_sem <-  filter(mmm, variable %in% c("SEM_sleep_length", "SEM_activity_length"))
  d <- data.frame(mmm_value, SEM=mmm_sem$value)
  d$Condition <- factor(d$Condition, levels = unique(conditions()))
  d <- arrange(d, Condition)
  d
  })
  
  
  ### Sleep activity lengths  
  output$sleep_activity_lengths_plot <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({
      
      sleep_activity_lengths_barplot <- ggplot(mmm_df(), aes(factor(Condition), value, fill = variable)) + 
        geom_bar(stat="identity", position = "dodge", colour="black") +
        geom_errorbar(data=mmm_df(), aes(ymin=value - SEM, ymax=value + SEM), position = position_dodge(0.9),  width=0.1) +
        scale_fill_manual(values=c("#0472CC", "#C7142E"))+
        theme_bw()+
        theme(legend.text=element_text(size=16))+
        theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
        labs(title= "Sleep vs activity length", size= 14)+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
        theme(axis.title.y = element_text(size=14))+
        theme(legend.title=element_blank())+
        theme(axis.title.x =  element_blank())+
        labs(x="", y="Time [min]")
      
      sleep_activity_lengths_barplot
      
    })
  })
  
  
  ### Sleep activity length       
  output$sleep_activity_length_table <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({mmm_df()})
  })     
  
  
  
  act_slp_ratio <- reactive({
    
    d <- select(mean_act_sleep_length(), Condition, Light_cycle, Activity_length_to_sleep_length, SEM_activity_length_to_sleep_length)
    colnames(d) <- c("Condition", "Light_cycle", "Activity_to_sleep_length", "SEM")
    d$Condition <- factor(d$Condition, levels = unique(conditions()))
    d <- arrange(d, Condition)
    d
  })
  
  
 
  ### Sleep activity ratios  
  output$sleep_activity_ratios_plot <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({
      
     d <- as.data.frame(act_slp_ratio())
      
     sl_act_rat <-  ggplot(d, aes(Condition, Activity_to_sleep_length, fill = Condition, width=.5)) + 
       geom_bar(stat = "summary", fun.y = "mean", colour="black") +
       scale_fill_manual(values=Plot_colors())+
       geom_errorbar(data=d, aes(ymin=Activity_to_sleep_length - SEM, ymax=Activity_to_sleep_length + SEM),  width=0.1) +
       theme_bw()+
       theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
       labs(title= "Activity/Sleep length ratio", size= 14)+
       theme(plot.title = element_text(size = rel(2), hjust=0.5))+
       theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
       theme(axis.title.y = element_text(size=14))+
       theme(legend.position="none")+
       theme(axis.title.x =  element_blank())+
       labs(x="", y="Activity/Sleep ratio")
     
     sl_act_rat
     
    })
  })
  
  
  
  
  ### Sleep activity length ratio      
  output$sleep_activity_length_ratio_table <- renderTable({
    go_on_files()
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh_2 == 0)
      return()
    
    isolate({act_slp_ratio()})
  })
  
  
  
  individual_slp_act_bout_df <- reactive({
    p <- slp1()
    pp <- na.omit(p)
    a <- select(pp, Channel, Condition, Light_cycle, date, time, ZT_time, Dec_ZT_time, value, sleep_counts, bout, bout_length)
    a
  })
 
  
  output$download_slp_act_bout_data <- downloadHandler(
    filename = function() {
      paste("Individual_sleep_activity_bout_data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(individual_slp_act_bout_df(), file)
    }
  )