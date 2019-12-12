
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
                box(plotOutput("plot1", height = 600, width = "100%")),
                
                box(
                  sidebarPanel( 
                    sliderInput("num", "Years to Include:",min = 1970, max = 2019,step=1,value=c(1970,2014), width = 600), width = 12)
                  
                ),
                box(
                  selectInput("filter_country", "Country", 
                              choices=levels(dashboard_data$country_name),
                              selected=levels(dashboard_data$country_name)[1]),
                  selectInput("filter_indicator", "Indicator", 
                              choices=levels(dashboard_data$indicator_name),
                              selected=levels(dashboard_data$indicator_name)[1], multiple = TRUE)),
                box(checkboxInput("include_world", "Include World?", value = FALSE),
                    checkboxInput("include_se", "Include Standard Error?", value = FALSE)
                ))
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
                             (dashboard_data$country_name == input$filter_country | (dashboard_data$country_name == "World" & input$include_world == TRUE)) &
                             dashboard_data$indicator_name == input$filter_indicator,]
  })
  
  
  output$plot1 <- renderPlot({
    
    ggplot(dat(),aes(x=as.numeric(year),y=value, color=indicator_name, linetype=country_name)) + 
      geom_line() + 
      ggtitle(paste0(input$filter_country, " Economic Health")) +
      ylab(input$filter_indicator) +
      xlab("Year") + 
      xlim(min(input$num), max(input$num)) +
      # theme_bw() +
      theme_classic() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(legend.key.size = unit(1, 'cm')) +
      stat_smooth(method="lm", se = input$include_se, fullrange=TRUE) + #, color = "black")
      scale_linetype_discrete(name = "Country", guide = if(!input$include_world) 'none' else "legend") +
      scale_color_discrete(name = "Economic Indicators", guide = if(length(input$filter_indicator) == 1) 'none' else "legend")
    
  },width = "auto")
  
  
  
}

shinyApp(ui, server)
