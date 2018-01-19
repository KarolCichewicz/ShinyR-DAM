######################## Actograms ########################

# Actograms input settings parameteres  
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


# Calculates a dataset for plotting mean and median actograms  
mean_and_median <- reactive({
  DT<- data.table(melted_alive())
  DT[,list(median=as.numeric(median(value)), mean=as.numeric(mean(value)), sem=as.numeric(sd(value)/sqrt(length(value)))), 
     by=c("Dec_time", "Condition", "date", "Dec_ZT_time")]
})


# Function defining a mean actogram plot   
mean_actogram <- reactive({function(x){
  
  
  if (input$refresh == 0)
    return()
  
  # Binning actogram mean values - deccreases data resolution for plotting
  y <- filter(mean_and_median(), Condition==x) # Filteres a condition 
  binning_value <- input$actogram_bin / input$data_recording_frequency #Generated a value for binning 
  y$binned_mean <- rep(rollapply(y$mean, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value)
  
  # Uses a plotting function for a single plotted actogram if Single is selected by the user, or a double plotted actogram if the other (Double) option was selected
  if (input$Double_Single == 'SP') {
    isolate(
      ggplot(y, aes(Dec_time, y=binned_mean, ymax=binned_mean, ymin=min(binned_mean))) +
        geom_ribbon(fill=input$actogram_color) +
        facet_grid(date ~ .)+
        labs(title= x, x= "", y = "Counts/recording frequency")+
        theme_bw()+
        scale_x_continuous(breaks = c(0, 360,720, 1080, (1440-input$data_recording_frequency)), labels=c("0 h", "6 h", "12 h", "18 h", "0 h")) +
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
    )
  } else {  
    
    # Thsese 3 lines double the actogram data
    b <- arrange(y, date, Dec_time)
    qqq <- lapply(unique(b$date), function(w) filter(b, date==w))
    zr1 <- do.call("rbind", replicate(2, qqq, simplify = T))
    
    # Generates a doubled dec time X scale
    zr1$Dec_time_double <- c(rep(1:data_freq(), length(unique(zr1$date))), rep((data_freq()+1):(data_freq()*2), length(unique(zr1$date))))*input$data_recording_frequency 
    
    zr2 <- arrange(zr1, date, Dec_time_double)
    zr3 <- zr2[(data_freq()+1):nrow(zr2),]     #drops the first repeated day
    zr3 <- arrange(zr3, date, Dec_time_double)
    
    # Generates another doubled dec time X scale after dropping the 1st day
    zr3$Dec_time_double2 <- ((c(rep(1:(data_freq()*2), length(unique(zr3$date))))[1:nrow(zr3)])*input$data_recording_frequency)
    
    # Generates double days for faceting the actograms
    zr3$date2 <- (c(rep(c(1:length(unique(zr3$date))), each=(data_freq()*2)))[1:nrow(zr3)])
    
    isolate(
      ggplot(zr3, aes(Dec_time_double2, y=binned_mean, ymax=binned_mean, ymin=min(binned_mean))) +
        geom_ribbon(fill=input$actogram_color)+
        theme_bw()+
        facet_grid(date2 ~ .)+
        labs(title= x, x= "", y = "Number of counts/recording frequency")+
        scale_x_continuous(breaks = c(1, 360,720, 1080, 1440, 1800, 2160, 2520, 2880), 
                           labels=c("0 h", "6 h", "12 h", "18 h", "0 h", "6 h", "12 h", "18 h", "0 h"))+
        coord_cartesian(xlim=c(1,2880))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
    )}
}           
})


# Plots mean actograms into a marrangeGrob object
output$mean_actograms <- renderPlot({
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


# Generates a UI element with dmean actogram plots - allows heigth and width customization   
output$Mean_act <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh == 0)
    return()
  
  isolate(
    plotOutput("mean_actograms", height = (as.numeric(ac_heigth())*as.numeric(length(unique_conditions())*as.numeric(length(range_of_days())))), 
               width = ac_width()
    )
  )
})    



# Function defining a median actogram plot    
median_actogram <- reactive({function(x){
  if (input$refresh == 0)
    return()
  
  y <- filter(mean_and_median(), Condition==x) # Filteres a condition 
  binning_value <- input$actogram_bin / input$data_recording_frequency  #Generated a value for binning 
  
  # Binning actogram mean values
  y$binned_median <- rep(rollapply(y$median, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value)
  
  if (input$Double_Single == 'SP') {
    
    isolate(
      ggplot(y, aes(Dec_time, y=binned_median, ymax=binned_median, ymin=min(binned_median))) +
        geom_ribbon(fill=input$actogram_color) +
        facet_grid(date ~ .)+
        labs(title= x, x= "", y = "Counts/acquisition frequency")+
        theme_bw()+
        scale_x_continuous(breaks = c(0, 360,720, 1080, (1440-input$data_recording_frequency)), labels=c("0 h", "6 h", "12 h", "18 h", "0 h")) +
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
    )
  } else {
    
    # Thsese 3 lines double the actogram data
    b <- arrange(y, date, Dec_time)
    qqq <- lapply(unique(b$date), function(w) filter(b, date==w))
    zr1 <- do.call("rbind", replicate(2, qqq, simplify = T))
    
    # Generates a doubled dec time X scale
    zr1$Dec_time_double <- c(rep(1:data_freq(), length(unique(zr1$date))), rep((data_freq()+1):(data_freq()*2), length(unique(zr1$date))))*input$data_recording_frequency 
    
    zr2 <- arrange(zr1, date, Dec_time_double)
    zr3 <- zr2[(data_freq()+1):nrow(zr2),] #drops the first repeated day
    zr3 <- arrange(zr3, date, Dec_time_double)
    
    # Generates another doubled dec time X scale after dropping the 1st day
    zr3$Dec_time_double2 <- ((c(rep(1:(data_freq()*2), length(unique(zr3$date))))[1:nrow(zr3)])*input$data_recording_frequency)
    
    # Generates double days for faceting the actograms
    zr3$date2 <- (c(rep(c(1:length(unique(zr3$date))), each=(data_freq()*2)))[1:nrow(zr3)])
    
    isolate(
      ggplot(zr3, aes(Dec_time_double2, y=binned_median, ymax=binned_median, ymin=min(binned_median))) +
        geom_ribbon(fill=input$actogram_color)+
        theme_bw()+
        facet_grid(date2 ~ .)+
        labs(title= x, x= "", y = "Number of counts/recording frequency")+
        scale_x_continuous(breaks = c(1, 360,720, 1080, 1440, 1800, 2160, 2520, 2880), 
                           labels=c("0 h", "6 h", "12 h", "18 h", "0 h", "6 h", "12 h", "18 h", "0 h"))+
        coord_cartesian(xlim=c(1,2880))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
    )
  }
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





### Individual actograms - all 3 components

# Function defining individual actograms
ind_act <- reactive({function(x){
  if (input$refresh == 0)
    return()
  
  
  
  
  y <- filter(melted(), variable== x) # Filteres a condition 
  
  binning_value <- input$actogram_bin / input$data_recording_frequency #Generated a value for binning 
  
  # Binning actogram values
  y$binned_value <- rep(rollapply(y$value, width = binning_value, by = binning_value, FUN = mean, align = "left"), each=binning_value)
  
  if (input$Double_Single == 'SP') {    
    
    isolate(
      ggplot(y, aes(Dec_time, y=as.numeric(binned_value), ymax=as.numeric(binned_value), ymin=min(as.numeric(binned_value)))) +
        geom_ribbon(fill=input$actogram_color) +
        facet_grid(date ~ .)+
        labs(title= x, x= "", y = "Counts/recording frequency")+
        theme_bw()+
        scale_x_continuous(breaks = c(0, 360,720, 1080, (1440-input$data_recording_frequency)), labels=c("0 h", "6 h", "12 h", "18 h", "0 h")) +
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
    )
  } else {
    
    # Thsese 3 lines double the actogram data
    b <- arrange(y, date, Dec_time)
    qqq <- lapply(unique(b$date), function(w) filter(b, date==w))
    zr1 <- do.call("rbind", replicate(2, qqq, simplify = T))
    
    # Generates a doubled dec time X scale
    zr1$Dec_time_double <- c(rep(1:data_freq(), length(unique(zr1$date))), rep((data_freq()+1):(data_freq()*2), length(unique(zr1$date))))*input$data_recording_frequency 
    
    zr2 <- arrange(zr1, date, Dec_time_double)
    zr3 <- zr2[(data_freq()+1):nrow(zr2),] #drops the first repeated day
    zr3 <- arrange(zr3, date, Dec_time_double)
    
    # Generates another doubled dec time X scale after dropping the 1st day
    zr3$Dec_time_double2 <- ((c(rep(1:(data_freq()*2), length(unique(zr3$date))))[1:nrow(zr3)])*input$data_recording_frequency)
    
    # Generates double days for faceting the actograms
    zr3$date2 <- (c(rep(c(1:length(unique(zr3$date))), each=(data_freq()*2)))[1:nrow(zr3)])
    
    
    
    isolate(
      ggplot(zr3, aes(Dec_time_double2, y=binned_value, ymax=binned_value, ymin=min(binned_value))) +
        geom_ribbon(fill=input$actogram_color)+
        theme_bw()+
        facet_grid(date2 ~ .)+
        labs(title= x, x= "", y = "Number of counts/recording frequency")+
        scale_x_continuous(breaks = c(1, 360,720, 1080, 1440, 1800, 2160, 2520, 2880), 
                           labels=c("0 h", "6 h", "12 h", "18 h", "0 h", "6 h", "12 h", "18 h", "0 h"))+
        coord_cartesian(xlim=c(1,2880))+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        theme(axis.text=element_text(size=14))+
        theme(text = element_text(size=16))+
        coord_cartesian(ylim=c(0,as.numeric(ac_max_counts())))
    )
  }
}
})


# Plots all individual actograms using marrangeGrob 
output$individual_actograms <- renderPlot({
  withProgress(message = 'Drawing actograms', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("Plotting individual actograms"))
    
    if (is.null(inFile()))
      return(NULL)
    if (input$refresh == 0)
      return()
    
    isolate(
      if (input$Mean_median == 'Individual_alive'){
        d <- alive()
      } else {
        d <- dead_flies()
      }
    )
    
    isolate(
      marrangeGrob(lapply(d, function(x) FUN=ind_act()(x)), ncol=4, nrow = ((length(d)*0.25)+1), top ="")
    )
  })
})


# Generates UI element with individual actograms, resizes the plots
output$Individual_act <- renderUI({
  
  withProgress(message = 'Drawing actograms', min=0, max=0.6, value = 0.5, {
    incProgress(0.8, detail = paste("Rendering plots"))
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh == 0)
    return()
  
  isolate(
    if (input$Mean_median == 'Individual_alive'){
      d <- alive()
    } else {
      d <- dead_flies()
    }
  )
  
  isolate(
    plotOutput("individual_actograms", 
               height = 0.25*as.numeric(ac_heigth())*as.numeric(length(d)*as.numeric(length(range_of_days()))), 
               width = 1.9*as.numeric(ac_width()))
    
  )
  })
})


output$download_mean_and_median_actogram_data <- downloadHandler(
  filename = function() {
    paste("Mean_and_median_actogram_data", ".csv", sep="")
  },
  content = function(file) {
    write.csv(mean_and_median(), file)
  }
)


output$download_individual_actogram_data <- downloadHandler(
  filename = function() {
    paste("Individual_actogram_data", ".csv", sep="")
  },
  content = function(file) {
    write.csv(melted(), file)
  }
)

output$download_list_of_alive_flies <- downloadHandler(
  filename = function() {
    paste("List_of_alive_flies", ".csv", sep="")
  },
  content = function(file) {
    write.csv(alive(), file)
  }
)

output$download_list_of_dead_flies <- downloadHandler(
  filename = function() {
    paste("List_of_dead_flies", ".csv", sep="")
  },
  content = function(file) {
    write.csv(dead_flies(), file)
  }
)