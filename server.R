library(shiny)
library(ggplot2)
library(googlesheets)
library(dplyr)
library(scales)


common_theme <- 
  theme(axis.title.x=element_text(size=20,vjust=-1),
        axis.title.y=element_text(size=20,vjust=1),
        axis.text.x=element_text(size=18,angle=90,hjust=0.5,vjust=0.5),
        axis.text.y=element_text(size=18),
        plot.background=element_blank(),
        panel.background=element_blank(),
        panel.grid.major=element_line(color='grey80'),
        panel.grid.minor=element_line(color='grey81'))

time_scale <-
  scale_x_datetime('Time',limits=c(ISOdate(2001,01,01,0,0,1,tz='EST'),ISOdate(2001,1,1,23,59,59,tz='EST')),
                   breaks=pretty_breaks(7)
                   ,labels=c('12:00 AM',
                             '3:00 AM',
                             '6:00 AM',
                             '9:00 AM',
                             '12:00 PM',
                             '3:00 PM',
                             '6:00 PM',
                             '9:00 PM',
                             '12:00 AM'))

shinyServer(function(input,output){

  speeds <-gs_title('download speeds') %>%  gs_read
  speeds$timestamp <- strptime(speeds$Timestamp,'%m/%d/%Y %H:%M:%S',tz = 'EST')
  speeds$day <- as.character.Date(strptime(speeds$Timestamp,'%m/%d/%Y',tz='EST'))
  speeds$fday <- factor(speeds$day)
  speeds$time <- as.numeric(format(speeds$timestamp,'%H',tz='EST'))+as.numeric(format(speeds$timestamp,'%M',tz='EST'))/60
  speeds$clocktime <- paste(format(speeds$timestamp,'%H',tz='EST'),format(speeds$timestamp,'%M',tz='EST'),sep=":")
  speeds$hour <- as.numeric(format(speeds$timestamp,'%H',tz='EST'))
  speeds$minute <- as.numeric(format(speeds$timestamp,'%M',tz='EST'))
  speeds$second <- as.numeric(format(speeds$timestamp,'%S',tz='EST'))
  speeds$plotTime <- ISOdate(2001,1,1,speeds$hour,speeds$minute,speeds$second,tz='EST')
  unique_ips <- unique(speeds$ip_address)
    
  output$choose_ip <- renderUI({
    selectInput(inputId = 'ip_address',
                label = 'Choose IP Address',
                choices = unique_ips,
                selected = unique_ips[1])
    })
  output$choose_dates <- renderUI({
    dateRangeInput(inputId='date_range',label='Dates',
                   start=min(speeds$Timestamp),end=max(speeds$Timestamp),format='mm-dd-yyyy')
  })
  plotSpeeds <- speeds %>%
    select(-timestamp) %>% 
    group_by(hour,ip_address) %>% 
    mutate(mean_dl_hour=mean(download),
           mean_ul_hour=mean(upload),
           mean_ping_hour=mean(ping))
  
  # Figure out most recent values
  n <- dim(speeds)[1]
  lastDownload <- round(speeds[(n-2):n,'download'] %>% summarise(mean_speed=mean(download)) %>% as.numeric)
  lastUpload <- round(speeds[(n-2):n,'upload'] %>% summarise(mean_speed=mean(upload)) %>% as.numeric)
  lastPing <- round(speeds[(n-2):n,'ping'] %>% summarise(mean_speed=mean(ping)) %>% as.numeric)
  lastTime <- speeds[n,'timestamp'] %>% format(.,'%I:%M %p %m/%d ') 
  
  output$downloadplot <- renderPlot({
    df <- plotSpeeds %>% filter(ip_address==input$ip_address,
                                as.Date(strptime(Timestamp,'%m/%d/%Y',tz='EST'))>=input$date_range[1],
                                as.Date(strptime(Timestamp,'%m/%d/%Y',tz='EST'))<=input$date_range[2])
    ggplot(df,aes(x=plotTime,y=download))+
      geom_point(aes(fill=fday),size=3,alpha=0.6,pch=21,color='black')+
      geom_smooth(size=2)+
      scale_y_continuous('Download Speed (Mbps)')+
      time_scale+
      common_theme+
      guides(fill=FALSE)
  })
  
  output$pingplot <- renderPlot({
    df <- plotSpeeds %>% filter(ip_address==input$ip_address,
                                as.Date(strptime(Timestamp,'%m/%d/%Y',tz='EST'))>=input$date_range[1],
                                as.Date(strptime(Timestamp,'%m/%d/%Y',tz='EST'))<=input$date_range[2])
    ggplot(df,aes(x=plotTime,y=ping))+
      geom_smooth(size=2)+
      geom_point(aes(fill=fday),size=3,alpha=0.6,pch=21,color='black')+
      scale_y_continuous('Download Speed (Mbps)')+
      time_scale+
      common_theme+
      guides(fill=FALSE)
  })
  
  output$uploadplot <- renderPlot({
    df <- plotSpeeds %>% filter(ip_address==input$ip_address,
                                as.Date(strptime(Timestamp,'%m/%d/%Y',tz='EST'))>=input$date_range[1],
                                as.Date(strptime(Timestamp,'%m/%d/%Y',tz='EST'))<=input$date_range[2])
    ggplot(df,aes(x=plotTime,y=upload))+
      geom_smooth(size=2)+
      geom_point(aes(fill=fday),size=3,alpha=0.6,pch=21,color='black')+
      scale_y_continuous('Download Speed (Mbps)')+
      time_scale+
      common_theme+
      guides(fill=FALSE)
  })
  
  output$lastSpeed <- renderText({
    # paste(format(speeds$plotTime[1],'%Z'),format(speeds$timestamp[1],'%Z'),format(ISOdate(2001,1,1,0,30,1,tz='EST'),'%Z'),format(Sys.time(),'%Z'),sep='---')
    paste('Down:',lastDownload,'Mbps\t\t\t||\t\t\tUp:',lastUpload,'Mbps\t\t\t||\t\t\tPing:',lastPing)
    })
  output$lastTime <- renderText({
    paste('Last Checked at:',lastTime)
  })
  
})
