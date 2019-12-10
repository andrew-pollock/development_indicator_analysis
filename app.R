
if(!require("shiny")) install.packages('shiny')
if(!require("shinydashboard")) install.packages('shinydashboard')

library(shiny)
library(shinydashboard)
library(ggplot2)

dashboard_data <- read.csv("data/dashboard_data.csv")

ui <- dashboardPage(
  
  dashboardHeader(title = "Country Economic Health",
                  titleWidth = 280),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Country Deepdive", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Country Comparison", tabName = "widgets", icon = icon("poll"))
  )),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 400,width = 600)),
                
                box(
                  sidebarPanel( 
                    sliderInput("num", "Years to Include:",min = 1980, max = 2014,step=1,value=c(1,2), width = 600), width = 12)
                  
                ),
                box(
                  selectInput("filter_country", "Country", dashboard_data[,"country_name"], multiple = FALSE#,
                              #selectize = TRUE
                  ),
                  selectInput("filter_indicator", "Indicator", dashboard_data[,"indicator_name"], multiple = FALSE,
                              selectize = TRUE)
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
  
  
)


server <- function(input, output) { 

  dat <- reactive({
    test <- dashboard_data[dashboard_data$year %in% seq(from=min(input$num),to=max(input$num),by=1) & 
                             dashboard_data$country_name == input$filter_country &
                             dashboard_data$indicator_name == input$filter_indicator,]
    test
  })
  
  
  output$plot1 <- renderPlot({
    
    ggplot(dat(),aes(x=as.numeric(year),y=value)) + 
      geom_line() + 
      ggtitle(input$filter_country) +
      ylab(input$filter_indicator) +
      xlab("Year")
    
  },height = 400,width = 600)
  
  
  
}

shinyApp(ui, server)