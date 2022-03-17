library(readxl)
library(dplyr)
library(highcharter)
library(shiny)
library(shinythemes)

dat.viz <- read_excel("DataVizApp.xlsx")

prod.dat <- dat.viz %>%
  filter(Attribute == "Production")

cons.dat <- dat.viz %>%
  filter(Attribute == "Consumption")

exp.dat <- dat.viz %>%
  filter(Attribute == "Exports")

imp.dat <- dat.viz %>%
  filter(Attribute == "Imports")


#############################################################
#################### DataViz Shiny App ######################
#############################################################

ui <- fluidPage(
  titlePanel("Trends: Production, Consumption, and Trade"),
  theme = shinytheme("flatly"), #sandstone, flatly, cerulean
  sidebarPanel(
    selectInput("country", "Select a Country:", choices = unique(dat.viz$Country), selected = "Argentina"),
    checkboxGroupInput("commodity", "Choose a Commodity:", selected = "Corn",
                       choiceNames = list(
                         tags$span("Corn", style = "color: blue;"),
                         tags$span("Soybean", style = "color: red;"), 
                         tags$span("Wheat", style = "color: orange;") 
                         #tags$span("D", style = "font-weight: bold;")
                       ),
                       choiceValues = c("Corn", "Soybean", "Wheat"))
    #selectInput("year", "Choose a Year:", choices = unique(dat.viz$Year))
  ),
  
  mainPanel(
    textOutput("selected_country"),
    textOutput("selected_commodity"),
    tabsetPanel(
      tabPanel('Production', fluidRow(
        column(width = 10, highchartOutput('Production')))),
      tabPanel('Consumption', fluidRow(
        column(width = 10, highchartOutput('Consumption')))),
      tabPanel('Exports', fluidRow(
        column(width = 10, highchartOutput('Exports')))),
      tabPanel('Imports', fluidRow(
        column(width = 10, highchartOutput('Imports'))))
  )
  ),
  
  basicPage(
    verbatimTextOutput("default1")
  
  )
  #fluidRow(
  #  column(width = 6, highchartOutput("Production")),
  #  column(width = 6, highchartOutput("Consumption")),
  #  column(width = 6, highchartOutput("Exports")),
  #  column(width = 6, highchartOutput("Imports")),
  #)
  
)

#server <- function(input, output){}

server <- function(input, output) {
  output$selected_country <- renderText({
    paste("You have selected", input$country, "and")
  })
  output$selected_commodity <- renderText({
    commodity <- paste(input$commodity, collapse = ", ")
    paste("You have chosen", commodity)
  })
  
  output$Production <- renderHighchart({
    
    prod.dat %>% filter(Commodity == input$commodity) %>% filter(Country == input$country) %>%
      hchart(
        'line', hcaes(x = Year, y = Value, group = Commodity)
      ) %>%
      hc_colors(c("blue", "red", "orange")) %>%
      hc_yAxis(
        title = list(text = "Production in 1000 Metric Tons",
                     style = list(fontSize = "20px")),
        labels = list(style = list(fontSize = "20px"))) %>%
      hc_xAxis(
        title = list(text = "Year",
                     style = list(fontSize = "20px"
                     )),
        labels = list(style = list(fontSize = "20px")))
    
  })
  
  output$Consumption <- renderHighchart({
    
    
    cons.dat %>% filter(Commodity == input$commodity) %>% filter(Country == input$country) %>%
      hchart(
        'column', hcaes(x = Year, y = Value, group = Commodity)
      ) %>%
      hc_colors(c("blue", "red", "orange")) %>%
      hc_yAxis(
        title = list(text = "Domestic Consumption in 1000 Metric Tons",
                     style = list(fontSize = "20px")),
        labels = list(style = list(fontSize = "20px"))) %>%
      hc_xAxis(
        title = list(text = "Year",
                     style = list(fontSize = "20px"
                     )),
        labels = list(style = list(fontSize = "20px")))
  })
  
  output$Exports <- renderHighchart({
    
    
    exp.dat %>% filter(Commodity == input$commodity) %>% filter(Country == input$country) %>%
      hchart(
        'bar', hcaes(x = Year, y = Value, group = Commodity)
      ) %>%
      hc_colors(c("blue", "red", "orange")) %>%
      hc_yAxis(
        title = list(text = "Exports in 1000 Metric Tons",
                     style = list(fontSize = "20px")),
        labels = list(style = list(fontSize = "20px"))) %>%
      hc_xAxis(
        title = list(text = "Year",
                     style = list(fontSize = "20px"
                     )),
        labels = list(style = list(fontSize = "20px")))
  })
  
  output$Imports <- renderHighchart({
    
    
    imp.dat %>% filter(Commodity == input$commodity) %>% filter(Country == input$country) %>% 
      hchart(
        'column', hcaes(x = Year, y = Value, group = Commodity)
      ) %>%
      hc_colors(c("blue", "red", "orange")) %>%
      hc_yAxis(
        title = list(text = "Imports in 1000 Metric Tons",
                     style = list(fontSize = "20px")),
        labels = list(style = list(fontSize = "20px"))) %>%
      hc_xAxis(
        title = list(text = "Year",
                     style = list(fontSize = "20px"
                     )),
        labels = list(style = list(fontSize = "20px")))
  })
  
  #url <- a("PSD Online site", href = "https://apps.fas.usda.gov/psdonline/app/index.html#/app/home")
  output$default1 <- renderText({
    "Source: Data were collected from Production, Supply, and Distribution (PSD) Online site of the U.S. Department of Agriculture"
  })
  
  
}

shinyApp(ui, server)
