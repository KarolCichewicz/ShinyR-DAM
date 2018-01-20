### OUTPUT of the Daily Locomotor Activity Analysis ###   


#Activity profiles
melted_alive <- reactive(filter(melted(), variable %in% alive()))
melted_alive_LD <- reactive(filter(melted_alive(), Light_cycle=="LD"))

#Claculates mean and SEM values for every timepoint per condition in LD
activity_per_condition<- reactive({

  
  DT<- data.table(melted_alive_LD())
  p<- DT[,list(mean=as.numeric(mean(value/input$data_recording_frequency)), SEM=as.numeric(sd(value/input$data_recording_frequency)/sqrt(length(value)))), by=c("Dec_time", "Dec_ZT_time", "Condition")]
  p$Condition <- factor(p$Condition, levels = unique(conditions()))
  p <- arrange(p, Condition)
  p <- na.omit(p)
  p
})



# Averaging activity into X min intervals. The original approach was just "binning" the data over 30 min, hence the object names. Legacy... 

# Calculating vectors of average values over X min non overlapping windows
mean_of_activity_per_condition_30_min <- reactive(rollapply(activity_per_condition()$mean, width = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), by = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), FUN = mean))
sem_of_activity_per_condition_30_min <- reactive(rollapply(activity_per_condition()$SEM, width = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), by = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency), FUN = mean))

#Adds a mean and SEM averaged columns into the original data frame by repeating each element X times
activity_per_condition_1 <- reactive({
  d <- data.frame(activity_per_condition(), 
                  binned_mean = round(rep(mean_of_activity_per_condition_30_min(), each=as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency)), 10),
                  binned_sem = round(rep(sem_of_activity_per_condition_30_min(), each=as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency))), 10)
  d
})

# Sampling the data frame for every Xth row
activity_30min_mean_2 <- reactive(activity_per_condition_1()[seq(1, nrow(activity_per_condition_1()), by = as.numeric(input$act_profile_window)/as.numeric(input$data_recording_frequency)),])

#Drops unnecessary columns containing not averaged mean and SEM data
activity_30min_mean <- reactive(select(activity_30min_mean_2(), Dec_time, Dec_ZT_time, Condition, binned_mean, binned_sem))


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
  d<- DT[,list(Mean = mean(sum_of_activity_per_ind),
               SEM = sd(sum_of_activity_per_ind)/sqrt(length(sum_of_activity_per_ind))), by=c("Condition", "Light_status")]
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
                  mean_activity_per_fly_Day= d$sum_of_activity_per_ind,
                  mean_activity_per_fly_Night=n$sum_of_activity_per_ind,
                  Night_Day_activity_ratio=n$sum_of_activity_per_ind/d$sum_of_activity_per_ind) 
  p   
})

#    
Night_Day_ratio<- reactive({
  k <- ddply(activity_light_phase_individual_2(), c("Condition"), summarise,
             Mean_night_day_counts_ratio = mean(Night_Day_activity_ratio),
             SEM = sd(Night_Day_activity_ratio)/sqrt(length(Condition)))
  
  k$Condition <- factor(k$Condition, levels = unique(conditions()))
  k <- arrange(k, Condition)
  k <- na.omit(k)
  k
  
})




#Activity profile day by day


output$activity_profile_by_day <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_1 == 0)
    return()
  
  isolate({
   
    binning_value <- input$act_profile_window / input$data_recording_frequency #Generated a value for binning 
    df_mm <- mean_and_median()
    
    df_mm$Order_column <- rep(c(1:nrow(filter(df_mm, Condition == df_mm$Condition[1]))), 
                              length(unique(df_mm$Condition)))
    
    
    df_mm$binned_mean <- rep(rollapply(df_mm$mean, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value)
    df_mm$binned_sem <- rep(rollapply(df_mm$sem, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value)
    
    df_mm$Condition <- factor(df_mm$Condition, levels = unique_conditions())
    df_mm <- arrange(df_mm, Condition)
    
    
    df_mm2 <- df_mm[seq(1, nrow(df_mm), by = binning_value),] #Reduces the number of rows after binning
    
    df_mm2$Condition <- factor(df_mm2$Condition, levels = unique_conditions())
    df_mm2 <- arrange(df_mm2, Condition)
    
    
    b1 <- seq(0,length(unique(df_mm$Order_column)), (1440/4)/input$data_recording_frequency)
    b2 <-  seq(0, length(unique(df_mm$Order_column)), b1[3])
    b2 <- as.vector(b2)
    b2 <- b2[1:length(unique(df_mm$date))]
    b2 <- b2*2
    b2 <- b2 + b1[3]
    
    
    
    act_profile_by_day <- ggplot(df_mm2, aes(x=Order_column, y=binned_mean, ymax=3, ymin=0, colour = Condition)) +
      geom_line()+
      geom_point()+ 
      theme_bw()+
      scale_x_continuous(breaks = b1, 
                         labels=c(rep(c("0", "6", "12", "18"), times=length(unique(df_mm$date))), "0"))+ # X axis labels
      geom_vline(xintercept = seq(0,length(unique(df_mm$Order_column)), 1440/input$data_recording_frequency))+ (
        if (input$date_disp == "Yes")(
          annotate("text", x= b2, y=input$ac_profile_y_lim, label= unique(df_mm$date), size = 7)
        )) +
      # displays day annotations  
      labs(title= "", x= "Time of the day [H]", y = "Average counts per min")+ 
      coord_cartesian(ylim=c(0,as.numeric(ac_profile_max_y())))+
      scale_colour_manual(values=Plot_colors())+
      theme(legend.text=element_text(size=18))+
      theme(legend.title = element_text(size=18))+
      theme(axis.text.x=element_text(hjust=0.5, size=15))+
      theme(axis.text.y=element_text(size=15))+
      theme(axis.title=element_text(size=18))+
      guides(colour = guide_legend(override.aes = list(size=1))) + (
        if (input$display_error_bars == "Yes")(
          geom_errorbar(aes(ymax=binned_mean+binned_sem ,ymin=binned_mean-binned_sem), width=0.3)
        ))
    
    act_profile_by_day
    
  })
})


output$act_profiles_by_day <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_1 == 0)
    return()
  
  isolate(
    plotOutput("activity_profile_by_day", height = input$ac_profile_height, 
               width = input$ac_profile_width)
  )
  
})    







output$activity_profile_by_day_split <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_1 == 0)
    return()
  
  isolate({
    
    binning_value <- input$act_profile_window / input$data_recording_frequency #Generated a value for binning 
    df_mm <- mean_and_median()
    
    df_mm$Order_column <- rep(c(1:nrow(filter(df_mm, Condition == df_mm$Condition[1]))), 
                              length(unique(df_mm$Condition)))
    
    
    df_mm$binned_mean <- rep(rollapply(df_mm$mean, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value)
    df_mm$binned_sem <- rep(rollapply(df_mm$sem, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value)
    
    df_mm$Condition <- factor(df_mm$Condition, levels = unique_conditions())
    df_mm <- arrange(df_mm, Condition)
    
    
    df_mm2 <- df_mm[seq(1, nrow(df_mm), by = binning_value),] #Reduces the number of rows after binning
    
    df_mm2$Condition <- factor(df_mm2$Condition, levels = unique_conditions())
    df_mm2 <- arrange(df_mm2, Condition)
    
    b1 <- seq(0,length(unique(df_mm$Order_column)), (1440/4)/input$data_recording_frequency)
    b2 <-  seq(0, length(unique(df_mm$Order_column)), b1[3])
    b2 <- as.vector(b2)
    b2 <- b2[1:length(unique(df_mm$date))]
    b2 <- b2*2
    b2 <- b2 + b1[3]
    
    plots_activity_x <- lapply(unique_conditions(), function(x) 
      ggplot(filter(df_mm2, Condition==x), aes(x=Order_column, y=binned_mean, ymax=3, ymin=0, colour= Condition)) +
        theme_bw()+
        geom_line()+
        geom_point()+
        scale_x_continuous(breaks = b1, 
                           labels=c(rep(c("0", "6", "12", "18"), times=length(unique(df_mm$date))), "0"))+ (
          if (input$date_disp == "Yes")(
        annotate("text", x= b2, y=input$ac_profile_y_lim, label= unique(df_mm$date), size = 7)
                             )) +
        labs(title= "", x= "Time of the day [H]", y = "Average counts per min")+ 
        coord_cartesian(ylim=c(0,as.numeric(ac_profile_max_y())))+
        scale_colour_manual(values=Plot_colors())+
        theme(legend.text=element_text(size=18))+
        theme(legend.title = element_text(size=18))+
        theme(axis.text.x=element_text(hjust=0.5, size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title=element_text(size=18))+
        guides(colour = guide_legend(override.aes = list(size=1)))+ #edits the point size in a legend
        geom_vline(xintercept = seq(0,length(unique(df_mm$Order_column)), 1440/input$data_recording_frequency)) + (
          if (input$display_error_bars == "Yes")(
            geom_errorbar(aes(ymax=binned_mean+binned_sem ,ymin=binned_mean-binned_sem), width=0.3)
          ))
      
      
    )               
    
    
    marrangeGrob(plots_activity_x, ncol=1, nrow = length(unique(conditions())), top ="")
    
    
  })
})


output$act_profiles_by_day_split <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_1 == 0)
    return()
  
  isolate(
    plotOutput("activity_profile_by_day_split", height = (input$ac_profile_height * as.numeric(length(unique_conditions()))), 
               width = input$ac_profile_width
    )
  )
})    


# Work in progress

output$download_daily_act_profiles <- downloadHandler(
  filename = function() {
    paste("Daily_activity_profiles", ".csv", sep="")
  },
  content = function(file) {
    write.csv(mean_and_median(), file)  # this is not a corrtect object. I will have to throw some stuff above into a reactive....
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
      
      if (input$act_scale == "Data_acq") {
        scale <- "Dec_time"
      } else {
        scale <- "Dec_ZT_time"
      }
      
    
      brakes_vector <- c(0, 360, 720, 1080, 1440)
      
      d <- activity_30min_mean()
      
      
      plots_activity_30_min <-  ggplot(d, aes(get(scale), y=binned_mean, ymax=3, ymin=0, colour=Condition)) +
        geom_point()+
        geom_line()+
        geom_vline(xintercept = c(0, 1440))+
        labs(title= "", x= "Time of the day [H]", y = "Average counts per min")+
        coord_cartesian(ylim=c(0,as.numeric(ac_profile_max_y())))+
        scale_x_continuous(breaks = brakes_vector, labels=c("0", "6", "12", "18", "0"))+
        theme_bw()+
        scale_colour_manual(values=Plot_colors())+
        theme(legend.text=element_text(size=18))+
        theme(legend.title = element_text(size=18))+
        theme(axis.text.x=element_text(hjust=0.5, size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title=element_text(size=18))+
        guides(colour = guide_legend(override.aes = list(size=1)))+ (   #edits the point size in a legend
          if (input$display_error_bars == "Yes")(
            geom_errorbar(aes(ymax=binned_mean + binned_sem,ymin=binned_mean - binned_sem), width=0.3)
          ))
        
      
      plots_activity_30_min
      
      
    })})})


output$act_profile_30_min_overlap <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_1 == 0)
    return()
  
  isolate(
    plotOutput("activity_profile_30_min", height = input$ac_profile_height, 
               width = input$ac_profile_width)
    )
  
}) 


      
      
      
output$activity_profile_30_min_split <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_1 == 0)
    return()
  
  withProgress(message = 'Calculating activity profiles', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("In progress"))
    
    isolate({
      
      if (input$act_scale == "Data_acq") {
        scale <- "Dec_time"
      } else {
        scale <- "Dec_ZT_time"
      }
      
      
      brakes_vector <- c(0, 360, 720, 1080, 1440)
      
      d <- activity_30min_mean()
      
      plots_activity_30_min_x <- lapply(unique_conditions(), function(x) ggplot(filter(d, Condition==x), 
                                                            aes(get(scale), y=binned_mean, ymax=3, ymin=0, colour=Condition)) +
                                          geom_point()+
                                          geom_line()+
                                          geom_vline(xintercept = c(0, 1440))+
                                          labs(title= "", x= "Time of the day [H]", y = "Average counts per min")+
                                          coord_cartesian(ylim=c(0,as.numeric(ac_profile_max_y())))+
                                          scale_x_continuous(breaks = brakes_vector, labels=c("0", "6", "12", "18", "0"))+
                                          theme_bw()+
                                          scale_colour_manual(values=Plot_colors())+
                                          theme(legend.text=element_text(size=18))+
                                          theme(legend.title = element_text(size=18))+
                                          theme(axis.text.x=element_text(hjust=0.5, size=15))+
                                          theme(axis.text.y=element_text(size=15))+
                                          theme(axis.title=element_text(size=18))+
                                          guides(colour = guide_legend(override.aes = list(size=1)))+ (   #edits the point size in a legend
                                            if (input$display_error_bars == "Yes")(
                                              geom_errorbar(aes(ymax=binned_mean + binned_sem, ymin=binned_mean - binned_sem), width=0.3)
                                            ))
                                           )
                                          
                                        
      

      marrangeGrob(plots_activity_30_min_x, ncol=1, nrow = length(unique(conditions())), top ="")


    })})})
      
      

output$act_profiles_average_split <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_1 == 0)
    return()
  
  isolate(
    plotOutput("activity_profile_30_min_split", height = (input$ac_profile_height * as.numeric(length(unique_conditions()))), 
               width = input$ac_profile_width
    )
  )
})


# Activity profiles download links

# Activity profiles data file
output$download_average_act_profiles <- downloadHandler(
  filename = function() {
    paste("Average_activity_profiles_in_LD", ".csv", sep="")
  },
  content = function(file) {
    write.csv(activity_30min_mean(), file)
  }
) 



output$Daytime_activity <- renderPlot({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$go == 0)
    return()
  
  isolate({
    
    barplot_activity_phase_Day<- ggplot(activity_condition_Day(), aes(x=Condition, y=Mean, fill=Condition, width=.5)) + 
      geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
      labs(x="", y="Locomotor activity [counts/min]") +                    #adds/removes axis lables
      theme_bw()+
      geom_errorbar(data=activity_condition_Day(), aes(ymax=Mean+SEM,ymin=Mean-SEM), width=0.1)+
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
    
    barplot_activity_phase_Nigth<- ggplot(activity_condition_Night(), aes(x=Condition, y=Mean, fill=Condition, width=.5)) + 
      geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
      labs(x="", y="Locomotor activity [counts/min]") +                    #adds/removes axis lables
      theme_bw()+
      geom_errorbar(data=activity_condition_Night(), aes(ymax=Mean+SEM,ymin=Mean-SEM), width=0.1)+
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
    
    barplot_Night_Day_ratio<- ggplot(Night_Day_ratio(), aes(x=Condition, y=Mean_night_day_counts_ratio, fill=Condition, width=.5)) + 
      geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
      labs(x="", y="Mean of individual Nighttime counts/ Daytime counts ratios") +                    #adds/removes axis lables
      theme_bw()+
      geom_errorbar(data=Night_Day_ratio(), aes(ymax=Mean_night_day_counts_ratio+SEM,ymin=Mean_night_day_counts_ratio-SEM), width=0.1)+
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
