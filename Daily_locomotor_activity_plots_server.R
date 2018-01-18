### OUTPUT of the Daily Locomotor Activity Analysis ###   

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
    colourInput(paste0('Condition_colour', i), paste('Color of Condition', i), value = "#BEBEBE")
  })
})


output$Order_monitors <- renderUI({
  Monitor_files <- input$file1
  lapply(setdiff(1:length(Monitor_files$name), 0), function(i) {
    selectInput(paste0('Monitor_order', i), paste('Monitor file order:', i), Monitor_files$name, selected = Monitor_files$name[i])
  })
})



# Data links   
# Activity per fly



# Activity per individual fly each day
output$downloadActivity_per_Condition_per_fly_all <- downloadHandler(
  filename = function() {
    paste("Individual daily locomotor activity data", ".csv", sep="")
  },
  content = function(file) {
    write.csv(melted_by_day(), file)
  }
)

# Daytime/ Nighttime activity per individual
output$download_D_N_individual_activity <- downloadHandler(
  filename = function() {
    paste("Daytime_Nighttime_activity_per_individual-", Sys.Date(),  ".csv", sep="")
  },
  content = function(file) {
    write.csv(activity_light_phase_individual_2(), file)
  }
)




# Activity in LD bar plot         
output$bar_plot_LD <- renderPlot({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$go == 0)
    return()
  
  isolate({ 
    
    barplot_LD<- ggplot(general_summary_LD(), aes(x=Condition, y=Mean, fill=Condition, width=.5)) + 
      geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
      labs(x="", y="Locomotor activity [counts/day]") +                    #adds/removes axis lables
      theme_bw()+
      geom_errorbar(data=general_summary_LD(), aes(ymax=Mean+SEM,ymin=Mean-SEM), width=0.1)+
      scale_x_discrete(limits=unique_conditions())+
      labs(title= "Mean activity per day in LD", size= rel(2))+
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
    ggplot(general_summary_DD(), aes(x=Condition, y=Mean, fill=Condition, width=.5)) + 
      geom_bar(stat = "summary", fun.y = "mean", colour="black") +         #plots bars,mean value
      labs(x="", y="Locomotor activity [counts/day]") +                    #adds/removes axis lables
      theme_bw()+
      geom_errorbar(data=general_summary_DD(), aes(ymax=Mean+SEM,ymin=Mean-SEM), width=0.1)+
      scale_x_discrete(limits=unique_conditions())+
      theme(legend.text=element_text(size=16))+
      theme(legend.position="none")+
      theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
      labs(title= "Mean activity per day in DD", size= rel(2))+
      theme(plot.title = element_text(size = rel(2), hjust=0.5))+
      theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
      theme(axis.title.y = element_text(size=14))+
      scale_fill_manual(values=Plot_colors())
  })
})



output$download_general_summary <- downloadHandler(
  filename = function() {
    paste("Daily_locomotor_activity", ".csv", sep="")
  },
  content = function(file) {
    write.csv(general_summary(), file)
  }
)






# Activity in LD box plot  
output$box_plot_LD <- renderPlot({
  go_on_files()
  
  if (is.null(inFile()))
    return(NULL)
  isolate({
    box_plot_LD<- ggplot(na.omit(summary_by_fly_LD()), aes(x=Condition, y=mean_value, fill=Condition)) +
      geom_boxplot(alpha=0.7)+
      geom_point()+
      labs(title= "Activity per day in LD")+
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
      labs(title= "Activity per day in DD")+
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
      labs(title= "Activity distributions in LD")+
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
      labs(title= "Activity distributions in DD")+
      labs(x="Locomotor activity [counts/day]", y="Density of measurements") +  #adds/removes axis lables
      theme(legend.title=element_blank())+ #removes legend title
      theme_bw()+
      theme(legend.title=element_text(size=rel(2), face="bold"))+
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


output$download_summary_by_fly <- downloadHandler(
  filename = function() {
    paste("Average_individual_daily_locomotor_activity", ".csv", sep="")
  },
  content = function(file) {
    write.csv(summary_by_fly_alive(), file)
  }
)








# Activity point plot by day   
output$point_plot_by_day <- renderPlot({
  
  if (input$go == 0)
    return()
  
  if (is.null(inFile()))
    return(NULL)
  
  isolate({
    
    point_plot_by_date<- ggplot(summary_by_date(), aes(x=date, y=mean, fill=Condition, color=Condition, width=.5))+
            geom_point(stat = "summary", fun.y = "mean", show.legend = FALSE) +         #plots bars,mean value
            labs(x="", y="Locomotor activity [counts/day]") +                    #adds/removes axis lables
            theme_bw()+
            geom_errorbar(data=summary_by_date(), aes(ymax=mean+sem,ymin=mean-sem), width=0.1)+
            theme(strip.text = element_text(size=25)) + #plot titles
            guides(size = guide_legend(order = 1))+
            theme(axis.title.x = element_text(color="black", size=16))+ 
            theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=15, face="bold"))+
            theme(axis.text.y=element_text(size=15, face="bold"))+
            scale_colour_manual(values=Plot_colors())+
            theme(legend.text=element_text(size=18))+
            theme(legend.title = element_text(size=18))+
            theme(axis.title=element_text(size=18))+
            guides(colour = guide_legend(override.aes = list(size=1)))
    
    point_plot_by_date
  })
})


output$download_activity_by_day <- downloadHandler(
  filename = function() {
    paste("Locomotor_activity_by_day", ".csv", sep="")
  },
  content = function(file) {
    write.csv(summary_by_date(), file)
  }
)




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


output$Daytime_nighttime_activity_plot <- renderPlot({
  
  if (is.null(inFile()))
    return(NULL)
  if (input$go == 0)
    return()
  
  isolate({
    
    Daytime_nighttime_activity_plot <- ggplot(activity_condition_phase(), aes(factor(Condition), Mean, fill = Light_status)) + 
      geom_bar(stat="identity", position = "dodge", colour="black") +
      geom_errorbar(data=activity_condition_phase(), aes(ymin=Mean - SEM, ymax=Mean + SEM), position = position_dodge(0.9),  width=0.1) +
      scale_fill_manual(values=c("#E8E823", "#0472CC"))+
      theme_bw()+
      theme(legend.text=element_text(size=16))+
      theme(axis.text.x=element_text(angle=50, vjust=0.9, hjust=1, size=13, face="bold"))+
      labs(title= "Daytime vs nighttime activity in LD", size= 14)+
      theme(plot.title = element_text(size = rel(2), hjust=0.5))+
      theme(axis.text.y=element_text(vjust=0.9, hjust=1, size=11, face="bold"))+
      theme(axis.title.y = element_text(size=14))+
      theme(legend.title=element_blank())+
      theme(axis.title.x =  element_blank())+
      labs(x="", y="Locomotor activity [counts/day]")
    
    Daytime_nighttime_activity_plot
    
  })
})






output$download_daytime_nighttime_activity <- downloadHandler(
  filename = function() {
    paste("Daytime_nighttime_activity_in_LD", ".csv", sep="")
  },
  content = function(file) {
    write.csv(activity_condition_phase(), file)
  }
)






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
      geom_errorbar(data=Night_Day_ratio(), aes(ymax=mean_Night_Day_counts_ratio+SEM,ymin=mean_Night_Day_counts_ratio-SEM), width=0.1)+
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

output$download_nighttime_daytime_activity_ratio <- downloadHandler(
  filename = function() {
    paste("Nighttime_daytime_activity_ratio", ".csv", sep="")
  },
  content = function(file) {
    write.csv(Night_Day_ratio(), file)
  }
)




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
