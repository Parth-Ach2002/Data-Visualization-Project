#Read data
fifa18=read.csv('fifa18.csv',header=TRUE, sep=",")

#Load libraries
library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)

#Data Cleaning
fifa18$Preferred.Positions <- 
  gsub(" ", "", substr(fifa18$Preferred.Positions, 1, 3))

x <- as.factor(fifa18$Preferred.Positions)
levels(x) <- list(GK  = c("GK"), 
                  DEF = c("LWB", "LB", "CB", "RB", "RWB"), 
                  MID = c("LW","LM","CDM","CM","CAM","RM","RW"), 
                  FWD = c("CF", "ST"))
fifa18 <- mutate(fifa18, Position = x)

toNumberCurrency <- function(vector) {
  vector <- as.character(vector)
  vector <- gsub("(€|,)","", vector)
  result <- as.numeric(vector)
  k_positions <- grep("K", vector)
  result[k_positions] <- as.numeric(gsub("K","",        
                                         vector[k_positions])) * 1000
  m_positions <- grep("M", vector)
  result[m_positions] <- as.numeric(gsub("M","", 
                                         vector[m_positions])) * 1000000
  return(result)
}
fifa18$Wage <- toNumberCurrency(fifa18$Wage) 
fifa18$Value <- toNumberCurrency(fifa18$Value)

fifa18<-data.frame(fifa18)
fifa18["age_group"] = cut(fifa18$Age, c(16,20,25,30, 35, 40, 45, 50), c("16-20", "21-25", "26-30", "31-35","36-40","41-45","46-50"), include.lowest=TRUE)
fifa18

#UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                headerPanel('Fifa 18 Chart Analysis'),
                sidebarPanel(
                  selectInput("Club",
                              label="Club:",
                              choices=unique(fifa18$Club),
                              selected="Real Madrid")
                  ),
                fluidRow(column(10, plotOutput("graph1"))),
                fluidRow(column(10, plotOutput("graph2"))),
                fluidRow(column(10, plotOutput("graph3"))),
                fluidRow(column(10, plotOutput("graph4"))),
                fluidRow(column(10, plotOutput("graph5")))
)

#Server
server<-function(input, output, session){
  
  selected <- reactive(fifa18%>%filter(fifa18$Club==input$Club))

  output$graph1<- renderPlot({
    ggplot(data=selected(), aes(x=Value, y=Overall, color=age_group)) +
      geom_smooth(se=FALSE) + facet_grid(.~age_group) +
      labs(title="Distribution between Value and Overall")
  })
  
  output$graph2<- renderPlot({
  ggplot(data=selected(), aes(x=Wage, y=Overall, color=age_group)) +
    geom_smooth(se=FALSE) + facet_grid(.~age_group) +
      labs(title="Distribution between Wage and Overall")
  })
    
  output$graph3<- renderPlot({
    ggplot(data=selected(), aes(x=Age, y=Potential, color=Position)) +
    geom_line(se=FALSE) + facet_grid(.~Position) +
      labs(title="Distribution between Age and Potential")
  })
  
  output$graph4<- renderPlot({
  ggplot(data=selected(), aes(x=Value, y=Potential, color=age_group)) +
    geom_smooth(se=FALSE) + facet_grid(.~age_group) +
      labs(title="Distribution between Value and Potential")
  })
  
  output$graph5<- renderPlot({
  ggplot(data=selected(), aes(x=Potential, y=Overall, color=age_group)) +
    geom_smooth(se=FALSE) + facet_grid(.~age_group) +
      labs(title="Distribution between Potential and Overall")
  })
  }

shinyApp(ui,server)
