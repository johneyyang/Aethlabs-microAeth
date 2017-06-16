library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(grid)
library(gridExtra)
library(reshape2)

# https://stackoverflow.com/questions/11794436/stacking-multiple-plots-vertically-with-the-same-x-axis-but-different-y-axes-in

options(shiny.maxRequestSize=30*1024^2)


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

#if (interactive()) {
  
  ui <- fluidPage(
    # Application title
    titlePanel("MicroAeth AE51 Data Processing"),
    
    h4("v1 @2017-06"),
    # two columns with sidebar for parameter selection and main panel for showing results and plots
    sidebarLayout(
      
      # side panel 
      sidebarPanel(
        textInput(inputId = "subject", label = "What is the Subject ID?", value = "BIKE0000"),
        
        selectInput(inputId = "session", label = "Which Monitoring Session is this?", choices = list(1,2,3,4,5,6), selected = 1),
        
        fileInput(inputId = "input_datafile_1", label = "Choose CSV File from local drive to upload", accept=c("csv"))
        
        #actionButton("process_data", "Process Data")
        
        #helpText("Please note - there may be a delay of up to 1 minute after clicking 'Process Data' as the data is processed."),
        
        #downloadButton("downloadData", "Download the processed data")
        
      ),
      
      # main panel
      mainPanel(
        plotOutput("plot_BC"),
        plotOutput("plot_ATN"),
        plotOutput("plot_all"),
        tableOutput("contents2"),
        tableOutput("contents")
      )
      
    )
  )
  
  #header_AE51 <- c("Date",	"Time",	"Ref", "Sen",	"ATN",	"Flow",	"PCB_temp",	"Status",	"Battery", "BC")
  #test <- read.csv("C:/Users/Qiang/Dropbox (LDEO)/YangQ/BIKE/2016summercolocation/MicroAeth duplicates/BIKE1018_MAE85_S4-1_160810.csv", skip=17, header=F, col.names = header_AE51)
  #test$timestamp <- as.POSIXct(paste(test$Date, test$Time),format="%Y/%m/%d %H:%M:%S")
  
  
  server <- function(input, output) ({
    
    header_AE51 <- c("Date",	"Time",	"Ref", "Sen",	"ATN",	"Flow",	"PCB_temp",	"Status",	"Battery", "BC")
    
    subject <- reactive({
      input$subject
      
    })
    
    session <- reactive({
      input$session
    })
    
    Data <- reactive({
      infile <- input$input_datafile_1
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      
      rawdata <- data.frame(read.csv(infile$datapath, skip=17, header=F, col.names = header_AE51, stringsAsFactors = T))
      rawdata$datetime <- as.POSIXct(paste(rawdata$Date, rawdata$Time))
      #rawdata$datetime <- as.numeric(as.POSIXct(paste(rawdata$Date, rawdata$Time),format="%Y/%m/%d%H:%M:%S"))
      #rawdata$timestamp <- as.POSIXct(rawdata$datetime)
      dt.df <- melt(rawdata, measure.vars = c("BC", "ATN"))
      return(list(rawdata = rawdata, meltdata = dt.df))
    })
    
    
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      Data()$rawdata
    })
    
    
    
    output$plot_BC <- renderPlot({
     p  <- ggplot(data=Data()$rawdata)+
        geom_line(aes_string(x=names(Data()$rawdata)[11], y=names(Data()$rawdata)[10]))+
        #scale_color_manual("",breaks=c("IR.ATN1"),values=c("blue"))+
        xlab("")+
        ylab("Black Carbon (ug/m^3)")+
        #ggtitle(paste(subject, session))+
        scale_x_datetime(breaks=date_breaks("6 hour"),labels = date_format("%m-%d %H:%M",tz="America/New_York"))+
        #guides(colour = guide_legend(override.aes = list(size=3)))+
        theme_individual
        #ifelse(nrow(Data()$data_min)/60>24,"6 hour","1 hour")
      print(p)
    })
    
    
    output$plot_ATN <- renderPlot({
      p  <- ggplot(data=Data()$rawdata)+
        geom_line(aes_string(x=names(Data()$rawdata)[11], y=names(Data()$rawdata)[5]))+
        #scale_color_manual("",breaks=c("IR.ATN1"),values=c("blue"))+
        xlab("")+
        ylab("ATN")+
        #ggtitle(paste(subject, session))+
        scale_x_datetime(breaks=date_breaks("6 hour"),labels = date_format("%m-%d %H:%M",tz="America/New_York"))+
      #guides(colour = guide_legend(override.aes = list(size=3)))+
      theme_individual
      #ifelse(nrow(Data()$data_min)/60>24,"6 hour","1 hour")
      print(p)
    })
    
    
    output$contents2 <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      dt.df <- melt(Data()$rawdata, measure.vars = c("BC", "ATN"))
    })
    
    
    output$plot_all <- renderPlot({
      
    dt.df <- Data()$rawdata
    
    p <- ggplot(dt.df) +
      geom_line(aes(x = datetime, y = value, color = variable)) +
      facet_grid(variable ~ ., scales = "free_y")
      # Suppress the legend since color isn't actually providing any information
      #opts(legend.position = "none")
    print(p)
    })
    
  })
   
  
  shinyApp(ui, server)
