# This function was adapted from the xps package. I added parameters allowing to change the window of Period testing, and adjuts the resolution  
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
    Condition = rep(conditions(), each=nrow(df_m_Qp.act())/length(conditions())),
    channel = df_m_Qp.act()$col,
    Period = rep(k(), nrow(df_m_Qp.act())/length(k())),
    Qp.act = df_m_Qp.act()$value,
    Qp.sig = df_m_Qp.sig()$value,
    Act_Sig_ratio = df_m_Qp.act()$value/df_m_Qp.sig()$value
  )
})

# Dataframe of individual fly periods - alive flies  
df_period_alive <- reactive({
  p <- filter(df_period(), channel %in% alive())
  p$Condition <- factor(p$Condition, levels = unique(conditions()))
  p <- arrange(p, Condition)
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
individual_period_peaks_alive_rhythmic <- reactive({
  p <- do.call("rbind",lapply(unique(df_period_alive()$channel), peak_find()))
  p <- filter(p, Act_Sig_ratio > input$rhythmicity_threshold)
  p$Condition <- factor(p$Condition, levels = unique(conditions()))
  p <- arrange(p, Condition)
  p
})



# Mean_period_peaks_by_condition - summarizes the data. 
Mean_period_peaks_by_condition<- reactive({
  {
    DT<- data.table(individual_period_peaks_alive_rhythmic())
    p<- DT[,list(
      Median_period=as.numeric(median(Period)),
      Mean_period=as.numeric(mean(Period)),
      SEM_period=as.numeric(sd(Period)/sqrt(length(Period))),
      N_of_rhythmic_flies = length(Qp.act),
      
      Mean_Qp.act=as.numeric(mean(Qp.act)), 
      SEM_Qp.act=as.numeric(sd(Qp.act)/sqrt(length(Qp.act))),
      Mean_Qp.sig = mean(Qp.sig),
      
      "Mean_Qp.act/Qp.sig" = as.numeric(mean(Act_Sig_ratio)),
      "SEM_Qp.act/Qp.sig" = as.numeric(sd(Act_Sig_ratio)/sqrt(length(Act_Sig_ratio)))),
      by=c("Condition")]
    p$Condition <- factor(p$Condition, levels = unique(conditions()))
    p <- arrange(p, Condition)
    p
  }
})














#df_period_alive_rhythmic <- reactive({filter(df_period_alive(), channel %in% unique(individual_period_peaks_alive_rhythmic()$channel))})


#filter(dfpa, channel %in% unique(ippar$channel))

# Calculates mean Period by Condition  
Mean_period_by_condition_rhythmic <- reactive({
  
  dfpa <- df_period_alive()  
  ippar <- individual_period_peaks_alive_rhythmic()
  
  df_period_alive_rhythmic <- filter(dfpa, channel %in% unique(ippar$channel))
  
  DT<- data.table(df_period_alive_rhythmic)
  p<- DT[,list(
    Mean_Qp.act = as.numeric(mean(na.omit(Qp.act))), 
    SEM_Qp.act=as.numeric(sd(na.omit(Qp.act))/sqrt(length(na.omit(Qp.act)))),
    N_of_rhythmic_flies = length(Qp.act),
    Mean_Qp.sig = as.numeric(mean(na.omit(Qp.sig))),
    
    "Mean_Qp.act_Qp.sig_ratio" = as.numeric(mean(na.omit(Act_Sig_ratio))),
    "SEM_Qp.act_Qp.sig_ratio" = as.numeric(sd(na.omit(Act_Sig_ratio))/sqrt(length(na.omit(Act_Sig_ratio))))),
    
    by=c("Condition", "Period")]
  p$Condition <- factor(p$Condition, levels = unique(conditions()))
  p <- arrange(p, Condition)
  p
})

# Function finding a peak value in a mean periodogram. Limited by the rhythmicity_threshold.    
peak_find_mean <- reactive({
  function(x){
    p <- filter(Mean_period_by_condition_rhythmic(), Condition == x)
    k <- p[which(max(p$Mean_Qp.act_Qp.sig_ratio) == p$Mean_Qp.act_Qp.sig_ratio),]
    k
  }
})

# Period peaks detected from the mean periodograms  
period_peaks <- reactive({
  p <- do.call("rbind",lapply(unique(Mean_period_by_condition_rhythmic()$Condition), peak_find_mean()))
  p$Condition <- factor(p$Condition, levels = unique(conditions()))
  p <- arrange(p, Condition)
  p
})



########### Circadian plots #########

# Mean periodograms  
output$Mean_periodograms <- renderPlot({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate({
    ggplot(Mean_period_by_condition_rhythmic(), aes(x=Period, y=Mean_Qp.act, colour = Condition)) + 
      geom_point()+
      geom_line()+
      geom_line(data=Mean_period_by_condition_rhythmic(), aes(x=Period, y = Mean_Qp.sig), size=0.5, colour="black")+
      theme_bw()+
      labs(x="Circadian Period [h]", y="Mean Qp.act")+
      theme(plot.title = element_text(size = rel(2), hjust=0.5))+
      scale_x_continuous(breaks = seq(4, 56, 2)) +
      geom_vline(xintercept = 24, linetype = 2 ) +
      scale_colour_manual(values=Plot_colors())+
      theme(legend.text=element_text(size=18))+
      theme(legend.title = element_text(size=18))+
      theme(axis.text.x=element_text(hjust=0.5, size=15, face="bold"))+
      theme(axis.text.y=element_text(hjust=0.5, size=15, face="bold"))+
      theme(axis.title=element_text(size=18))+
      guides(colour = guide_legend(override.aes = list(size=1))) + (
        if (input$display_error_bars_per == "Yes")(
          geom_ribbon(aes(ymin=Mean_Qp.act-SEM_Qp.act, ymax=Mean_Qp.act+SEM_Qp.act), linetype=1, alpha=0.01)
        ))
    
  })
})




output$per_profiles <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate(
    plotOutput("Mean_periodograms", height = input$per_height, 
               width = input$per_width)
  )
})  





# Mean periodograms  
output$Mean_periodograms_split <- renderPlot({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate({
    
    plot_x <- lapply(unique_conditions(), function(x) ggplot(filter(Mean_period_by_condition_rhythmic(), Condition==x), aes(x=Period, y=Mean_Qp.act, colour = Condition)) + 
                       geom_point()+
                       geom_line()+
                       geom_line(data=Mean_period_by_condition_rhythmic(), aes(x=Period, y = Mean_Qp.sig), size=0.5, colour="black")+
                       theme_bw()+
                       labs(x="Circadian Period [h]", y="Mean Qp.act")+
                       theme(plot.title = element_text(size = rel(2), hjust=0.5))+
                       scale_x_continuous(breaks = seq(4, 56, 2)) +
                       geom_vline(xintercept = 24, linetype = 2 ) +
                       scale_colour_manual(values=Plot_colors())+
                       theme(legend.text=element_text(size=18))+
                       theme(legend.title = element_text(size=18))+
                       theme(axis.text.x=element_text(hjust=0.5, size=15, face="bold"))+
                       theme(axis.text.y=element_text(hjust=0.5, size=15, face="bold"))+
                       theme(axis.title=element_text(size=18))+
                       guides(colour = guide_legend(override.aes = list(size=1)))+ (
                         if (input$display_error_bars_per == "Yes")(
                           geom_ribbon(aes(ymin=Mean_Qp.act-SEM_Qp.act, ymax=Mean_Qp.act+SEM_Qp.act), linetype=1, alpha=0.01)
                         ))
    )
    
    marrangeGrob(plot_x, ncol=1, nrow = length(unique(conditions())), top ="")
    
  })
})


output$per_profiles_split <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate(
    plotOutput("Mean_periodograms_split", height = (input$per_height * as.numeric(length(unique_conditions()))), 
               width = input$per_width)
    
  )
})

df_period_alive_rhythmic <- reactive({

# Shorten this even further  
dfpa <-   df_period_alive()  
ippar <- individual_period_peaks_alive_rhythmic()

d <- filter(dfpa, channel %in% unique(ippar$channel))

d$channel <- factor(d$channel, levels = unique(d$channel))
d <- arrange(d, channel)

d <- within(d, Condition_channel <- paste(Condition, channel, sep='_'))
d

})





# Individual periodograms    
output$individual_periodograms <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate({
    
    ggplot(df_period_alive_rhythmic(), aes(x=Period, y=Qp.act, colour = channel)) + 
      geom_point()+
      geom_line()+
      geom_line(data= df_period_alive_rhythmic(), aes(x=Period, y = Qp.sig), size=0.5, colour="black")+
      theme_bw()+
      labs(x="Circadian Period [h]", y="Qp.act")+
      theme(plot.title = element_text(size = rel(2), hjust=0.5))+
      scale_x_continuous(breaks = seq(4, 56, 2)) +
      geom_vline(xintercept = 24, linetype = 2 ) +
      theme(axis.text.x=element_text( hjust=0.5, size=15, face="bold"))+
      theme(axis.text.y=element_text( hjust=0.5, size=15, face="bold"))+
      theme(axis.title = element_text(size=18))+
      theme(legend.title = element_blank())+
      scale_fill_manual(values=Plot_colors())+
      theme(legend.position="none")+
      theme(strip.text = element_text(size=25))+
      #scale_colour_manual(values=   Plot_colors())+
      facet_wrap(~ Condition)
    
    
  })
})






# Individual periodograms2    
output$individual_periodograms2 <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate({
    
    ind_periodogram <- function(x){
      y <- filter(df_period_alive_rhythmic(), Condition_channel==x) # Filteres a condition
      
      ggplot(y, aes(x=Period, y=Qp.act, colour = channel)) + 
        geom_point()+
        geom_line()+
        geom_line(data= y, aes(x=Period, y = Qp.sig), size=0.5, colour="black")+
        theme_bw()+
        labs(title= x, x="", y="Qp.act")+
        theme(plot.title = element_text(size = rel(2), hjust=0.5))+
        scale_x_continuous(breaks = seq(4, 56, 2)) +
        geom_vline(xintercept = 24, linetype = 2 ) +
        theme(axis.text.x=element_text( hjust=0.5, size=15, face="bold"))+
        theme(axis.text.y=element_text( hjust=0.5, size=15, face="bold"))+
        theme(axis.title = element_text(size=18))+
        theme(legend.title = element_blank())+
        scale_fill_manual(values=Plot_colors())+
        theme(legend.position="none")+
        theme(strip.text = element_text(size=25))
    }
    
    list_of_chan <- unique(df_period_alive_rhythmic()$Condition_channel)
    
    p <- marrangeGrob(lapply(list_of_chan, function(x) FUN=ind_periodogram(x)), ncol=1, nrow = length(unique(df_period_alive_rhythmic()$Condition_channel)), 
                      top ="",   bottom=textGrob("Circadian Period [h]", gp=gpar(fontsize=20)))
    p
    
  })
})


output$ind_per2 <- renderUI({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate(
    plotOutput("individual_periodograms2", height = input$per_height * as.numeric(length(unique(df_period_alive_rhythmic()$Condition_channel))), 
               width = input$per_width)
    
  )
})
















# Box plot of individual Period peaks  
output$ind_period_peaks <- renderPlot({
  
  
  
  if (is.null(inFile()))
    return(NULL)
  geom_dotplot(binaxis = "y", stackdir = "center")
  if (input$refresh_3 == 0)
    return()
  
  isolate({
    ind_periods<- ggplot(na.omit(individual_period_peaks_alive_rhythmic()), aes(x=Condition, y=Period, colour = Condition)) +
      geom_boxplot(alpha=0.7)+
      geom_dotplot(binaxis = "y", binwidth = 0.1, stackdir = "center", dotsize = 0.5, aes(fill=Condition))+
      labs(y="Circadian Period [h]", x="") +  #adds/removes axis lables
      theme(legend.title=element_blank())+ #removes legend title
      theme_bw()+
      theme(axis.text.x=element_text(angle=50, hjust=1, size=18, face="bold"))+
      theme(axis.text.y=element_text(hjust=1, size=16, face="bold"))+
      theme(axis.title.y = element_text(color="black", size=18))+         #axis title
      theme(legend.title = element_blank())+
      theme(legend.text = element_text(size=18))+
      labs(title= "Circadian period peaks")+
      theme(plot.title = element_text(size = rel(2), hjust=0.5))+
      theme(legend.position="none")+
      scale_color_manual(values=Plot_colors())
    
    ind_periods
    
  })
})

# Box plot of individual Period strengths  
output$ind_period_strength <- renderPlot({
  if (is.null(inFile()))
    return(NULL)
  geom_dotplot(binaxis = "y", stackdir = "center")
  if (input$refresh_3 == 0)
    return()
  
  isolate({
    ind_strength<- ggplot(na.omit(individual_period_peaks_alive_rhythmic()), aes(x=Condition, y=Act_Sig_ratio, colour = Condition)) +
      geom_boxplot(alpha=0.7)+
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, aes(fill=Condition))+
      labs(y="Qp.act/Qp.sig", x="") +  #adds/removes axis lables
      theme(legend.title=element_blank())+ #removes legend title
      theme_bw()+
      theme(axis.text.x=element_text(angle=50, hjust=1, size=18, face="bold"))+
      theme(axis.text.y=element_text(hjust=1, size=16, face="bold"))+
      theme(axis.title.y = element_text(color="black", size=18))+         #axis title
      theme(legend.title = element_blank())+
      theme(legend.text = element_text(size=18))+
      labs(title= "Circadian period strength")+
      theme(plot.title = element_text(size = rel(2), hjust=0.5))+
      theme(legend.position="none")+
      scale_color_manual(values=Plot_colors())
    
    ind_strength
  })
})



# Table of averaged individual Period peaks.   
output$mean_of_period_peaks_by_condition <- renderTable({
  go_on_files()
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate({Mean_period_peaks_by_condition()})
})

# Table of Period peaks called from mean periodograms.    
output$period_peaks <- renderTable({
  go_on_files()
  if (is.null(inFile()))
    return(NULL)
  if (input$refresh_3 == 0)
    return()
  
  isolate({period_peaks()})
})

### Circadian Links ####

output$download_mean_period_by_condition_rhythmic <- downloadHandler(
  filename = function() {
    paste("Mean_periodograms_data", ".csv", sep="")
  },
  content = function(file) {
    write.csv(Mean_period_by_condition_rhythmic(), file)
  }
)


output$download_individual_periodograms_alive_rhythmic <- downloadHandler(
  filename = function() {
    paste("Individual_periodograms_rhythmic_alive", ".csv", sep="")
  },
  content = function(file) {
    write.csv(df_period_alive_rhythmic(), file)
  }
)  


output$download_individual_period_peaks_alive_rhythmic <- downloadHandler(
  filename = function() {
    paste("Individual_fly_period_peaks", ".csv", sep="")
  },
  content = function(file) {
    write.csv(individual_period_peaks_alive_rhythmic(), file)
  }
)  