#
##  This is a Shiny App for processing single microAeth AE51 data.
## Author: Qiang Yang
## last accessed on 2017-06-28

### data initially cleaning to remove extreme and negative values? 



# load packages
library(shiny)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
library(scales)
library(grid)
library(gridExtra)
library(reshape2)


# request space for large files
options(shiny.maxRequestSize=30*1024^2)



# optional, define individual plot theme
theme_individual <- theme_bw() +
  theme(aspect.ratio=3/6,
        axis.text.y   = element_text(size=18),
        axis.text.x   = element_text(size=15,angle = 60, hjust = 1),
        axis.title.y  = element_text(size=18),
        #axis.title.x  = element_text(size=18),
        panel.background = element_blank(),
        panel.grid.major = element_line(size=1, color="gray97", linetype="solid"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill=NA, size=1),
        plot.title = element_text(size=18,hjust = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size=15))




##########################
####  User Interface  ####
##########################

ui <- fluidPage(
    # Application title
    titlePanel("MicroAeth AE51 Data Processing"),
    
    h4("v1 @2017-06"),

    sidebarLayout(
      
      # side panel 
      sidebarPanel(
        textInput(inputId = "subject", label = "What's the Subject ID?", value = "BIKE0000"),
        
        selectInput(inputId = "session", label = "Which monitoring session is this?", choices = list(1,2,3,4,5,6,7,8,9,10), selected = 1),
        
        fileInput(inputId = "input_datafile_1", label = "Choose .csv file from local drive to upload", accept=c("csv"))
                   ),    # end of sidebar Panel
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Summary",
                   tableOutput("summary1")),
          tabPanel(title = "Time-series plots", 
                 # plotOutput("plot_BC"),
                   downloadButton(outputId = "download_plots", label = "Download plots in pdf"),
                   plotlyOutput("plot_all")),
          tabPanel(title = "Data table",
                   downloadButton(outputId = "download_data", label = "Download data in csv"),
                   #downloadButton(outputId = "download_data", label = "Download data table in csv"),
                   tableOutput("contents"))
                   )
                ) # end of main Panel
      
   ) # end of sidebar layout 
  )  # ebd of fluid page





##################
####  Server  ####
##################

server <- function(input, output) ({
    
  # AE51 raw file headers
    header_AE51 <- c("Date",	"Time",	"Ref", "Sen",	"ATN",	"Flow",	"PCB_temp",	"Status",	"Battery", "BC")
    
  # get subject info  
    subject <- reactive({
      input$subject
    })
    
  # get session info  
    session <- reactive({
      input$session
    })
    
    
    Data <- reactive({
      # read in data file
      infile <- input$input_datafile_1
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      
      rawdata <- data.frame(read.csv(infile$datapath, skip=17, header=F, col.names = header_AE51, stringsAsFactors = T))
      # add device ID and timestamp
      rawdata$subject <- subject()
      rawdata$session <- session()
      rawdata$device_id <- substr(data.frame(read.csv(infile$datapath, skip=3, header=F, stringsAsFactors = F, nrows = 1))[1,1],  
                                  13, 
                                  nchar(data.frame(read.csv(infile$datapath, skip=3, header=F, stringsAsFactors = F, nrows = 1))[1,1]))
      rawdata$timestamp <- as.POSIXct(paste(rawdata$Date, rawdata$Time))
      rawdata$BC <- rawdata$BC/1000
      
      # data summary
      data_summary <- data.frame(
        
        summary_variable <- c("subject", 
                              "session", 
                              "device ID",
                              "time base",
                              "average flow", 
                              "start datetime", 
                              "end datetime", 
                              "total run time", 
                              "BC mean", 
                              "BC median",
                              ""),
        
        summary_value <- c(subject(),
                           session(),
                           rawdata$device_id[1],
                           substr(data.frame(read.csv(infile$datapath, skip=6, header=F, stringsAsFactors = F, nrows = 1))[1,1],  
                                  12, 
                                  nchar(data.frame(read.csv(infile$datapath, skip=6, header=F, stringsAsFactors = F, nrows = 1))[1,1])),
                           paste(round(mean(rawdata$Flow, na.rm = TRUE), digits = 1), "ml/min"),
                           as.character(min(rawdata$timestamp)),
                           as.character(max(rawdata$timestamp)),
                           paste(round(nrow(rawdata)/60, digits = 1), "hours"),
                           paste(round(mean(rawdata$BC, na.rm = T), digits = 2), "µg/m^3"),
                           paste(round(median(rawdata$BC, na.rm = T),digits = 2), "µg/m^3"),
                           "")
                                 
        ) # end of data_summary
        
      colnames(data_summary) <- c("", "")
      
      
      
      # melt data for plotting
      meltdata <- melt(rawdata, measure.vars = c("BC", "ATN", "Flow", "PCB_temp", "Battery"))
      meltdata$variable <- factor(meltdata$variable, 
                                  levels = c("BC", "ATN", "Flow", "PCB_temp", "Battery"), 
                                  labels = c("BC (µg/m3)", "ATN", "Flow (ml/min)", "Temperature (C)", "Battery (%)"))
      
      return(list(rawdata = rawdata, meltdata = meltdata, data_summary = data_summary))
    }) # end of reactive Data
    
    
    
    
# raw data file for display
    output$contents <- renderTable({
            data_table <- Data()$rawdata
            data_table[,"timestamp"] <- as.character(data_table[,"timestamp"])
            return(data_table)
    })
    
# raw data file for download
    output$download_data <- downloadHandler(
      filename = function() { paste("MAE_", subject(), "_S", session(), "_", Data()$rawdata[1, "Date"], ".csv", sep = "") },
      content = function(file) { write.csv(Data()$rawdata, file) }
      )

# optional output of melt data
    output$contents2 <- renderTable({
           Data()$meltdata
    })
        
    
# data summary
    output$summary1 <- renderTable({
      Data()$data_summary
    }) 
    
        
# plots for display
    output$plot_all <- renderPlotly({
      
    df <- Data()$meltdata
    
    p <- ggplot(df) +
      geom_line(aes(x = timestamp, y = value, color = variable))+
      scale_color_manual("",breaks=c("BC (µg/m^3)","ATN", "Flow (ml/min)", "Temperature (C)", "Battery (%)"),
                            values=c("black","blue", "black", "black", "red"))+
      ggtitle(paste(subject(), "_S", df$session[1], "_", df$Date[1], "_", df$device_id[1], sep = ""))+
      facet_grid(variable ~ ., scales = "free_y")+
      # Suppress the legend since color isn't actually providing any information
    theme_bw() + 
      theme(legend.position = "none",
            #aspect.ratio=3/6,
            #axis.text.y   = element_text(size=18),
            axis.text.x   = element_text(angle = 60, hjust = 1),
            axis.title.y  = element_blank(),
            axis.title.x  = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_line(size=1, color="gray97", linetype="solid"),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            panel.border = element_rect(color = "black", fill=NA, size=1),
            plot.title = element_text(hjust = 0.5)
            #legend.key = element_rect(fill = "white"),
            #legend.title = element_blank(),
            #legend.text = element_text(size=15),
            #strip.text = element_text(size=18)
            )
      
    ggplotly(p) %>%
  layout(autosize = F, height = 850, width = 600,  margin=list(r=100, l=100, t=100, b=100))
    
       })
    
    
    
    
    # plots in pdf for download 
    output$download_plots <- downloadHandler(
      filename = function() { paste("MAE_", subject(), "_S", session(), "_", Data()$rawdata[1, "Date"], ".pdf", sep = "") },
      content = function(file) { 
        df <- Data()$meltdata
        pdf(file, width =8.5, height = 11)
        p <- ggplot(df) +
      geom_line(aes(x = timestamp, y = value, color = variable))+
      scale_color_manual("",breaks=c("BC (µg/m^3)","ATN", "Flow (ml/min)", "Temperature (C)", "Battery (%)"), 
                            values=c("black","blue", "black", "black", "red"))+
      ggtitle(paste(subject(), "_S", df$session[1], "_", df$Date[1], "_", df$device_id[1], sep = ""))+
      facet_grid(variable ~ ., scales = "free_y")+
      # Suppress the legend since color isn't actually providing any information
      theme_bw() + 
      theme(legend.position = "none",
            aspect.ratio=3/6,
            axis.text.y   = element_text(size=18),
            axis.text.x   = element_text(size=15,angle = 60, hjust = 1),
            axis.title.y  = element_blank(),
            axis.title.x  = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_line(size=1, color="gray97", linetype="solid"),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            panel.border = element_rect(color = "black", fill=NA, size=1),
            plot.title = element_text(size=18,hjust = 0.5),
            legend.key = element_rect(fill = "white"),
            legend.title = element_blank(),
            legend.text = element_text(size=15),
            strip.text = element_text(size=18)
            )
      print(p)
      dev.off()
      }
    ) # end of downloader handler
    
    
    
    
    
    # optional for individual parameters
    output$plot_BC <- renderPlot({
      
      df <- Data()$rawdata
      
     p  <- ggplot(data=Data()$rawdata)+
        geom_line(aes(x=timestamp, y=BC))+
        #scale_color_manual("",breaks=c("IR.ATN1"),values=c("blue"))+
        xlab("")+
        ylab("Black Carbon (µg/m^3)")+
        #ggtitle(paste(subject, session))+
        scale_x_datetime(breaks=date_breaks("6 hour"),labels = date_format("%m-%d %H:%M",tz="America/New_York"))+
        #guides(colour = guide_legend(override.aes = list(size=3)))+
        theme_individual
        #ifelse(nrow(Data()$data_min)/60>24,"6 hour","1 hour")
      print(p)
    })
    
    
    
    
  })
  
  
 

## run Shiny App
  shinyApp(ui, server)
