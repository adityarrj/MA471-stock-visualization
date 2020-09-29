
library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)



ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny - An Interactive Visualization Example: Stock Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                  selected = "Blue",multiple = F),
      
      radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="BLACK","White"="#ffffff")),

      selectInput(inputId="channel11",label="Choose Stock",choices = c("AXISBANK"="AXISBANK",
                                                                          "ASIANPAINT"="ASIANPAINT",
                                                                          "ONGC"="ONGC",
                                                                          "RELIANCE"="RELIANCE"),
                  selected = "AXISBANK",multiple = F),
            
      selectInput(inputId="channel1",label="Choose Data Type",choices = c("VWAP"="VWAP",
                                                                        "Close"="Close"),
                  selected = "VWAP",multiple = F),
      
      sliderInput(inputId = "bins1xz",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      sliderInput(inputId = "range1",
                  label = "Data Range",
                  min = 1,
                  max = 1000,
                  value = c(1,1000))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Outputs: ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "distPlot1"),
      plotOutput(outputId = "distPlot2")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output){
  # renderPlot (similarly reactive)) indicates that:
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs change
  # 2. Its output type is a plot
  
  
  # Data Preparation Steps
  datasetInput <- reactive({
    file_to_read <- paste(input$channel11,".csv",sep="")
      
    data <- read.csv(file_to_read)
    
    data$Date1 <- strptime(as.character(data$Date),format="%Y-%m-%d")
    data$Date1 <- as.POSIXct(data$Date1)
    data$Day <- as.numeric(as.character(strftime(data$Date,format="%d")))
    data$Month <- as.numeric(as.character(strftime(data$Date,format="%m")))
    data$Year <- as.numeric(as.character(strftime(data$Date,format="%Y")))
    x <- data$Day[1]
    data$Day <- data$Day + 30*(data$Month-1) + 365*(data$Year - 2000) - x + 1
    return(data)
  })

  output$distPlot <- renderPlot({
    data<-datasetInput()
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    p2 <- data %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot()
    if(input$channel1 == "VWAP"){
      p2 <- p2 + geom_histogram(aes(x=VWAP),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "Close"){
      p2 <- p2 + geom_histogram(aes(x=Close),bins = input$bins1xz,col=input$border1,fill=sColor)
    }
    p2 <- p2 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x=input$channel1,y="Count",title=paste("Stock price data (Day Level) Histogram",input$channel1,sep = " "))
    
    p2
  })
  
  output$distPlot1 <- renderPlot({
    data<-datasetInput()
    p1 <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2]) %>% ggplot(aes(x=Day))
    if(input$channel1 == "VWAP"){
      p1 <- p1 + geom_line(aes(y=VWAP,col="VWAP"),size=0.5)
    }else if(input$channel1 == "Close"){
      p1 <- p1 + geom_line(aes(y=Close,col="Close"),size=0.5)
    }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Time",y=input$channel1,title=paste("Stock price data (Day Level) in 2000-2020: ",input$channel1,sep = " "),colour="Data Type")

    p1
    
  })
  
  output$distPlot2 <- renderPlot({
    data<-datasetInput()
    d <- data  %>%  filter(Day >= input$range1[1] & Day <= input$range1[2])

    d <- ddply(d, .variables = c("Day"),function(x){

      VWAPavg <- mean(x$VWAP,na.rm = T)
      Closeavg <- mean(x$Close,na.rm = T)
      data.frame(VWAPavg,Closeavg)
    })

    p1 <- d %>% ggplot(aes(x=Day))
    if(input$channel1 == "VWAP"){
      p1 <- p1 + geom_line(aes(y=VWAPavg,col="VWAP"),size=1)
      p1 <- p1 + geom_point(aes(y=VWAPavg))
    }else if(input$channel1 == "Close"){
      p1 <- p1 + geom_line(aes(y=Closeavg,col="Close"),size=1)
      p1 <- p1 + geom_point(aes(y=Closeavg))
    }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Time",y=input$channel1,title=paste("Stock price data (Day Level) - Average Variation - 2000-2020: ",input$channel1,sep = " "),colour="Data Type")

    p1

  })
}

shinyApp(ui = ui, server = server)
