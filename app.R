
if(!require("shiny")) install.packages('shiny')
if(!require("shinydashboard")) install.packages('shinydashboard')

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

dashboard_data <- read.csv("data/dashboard_data.csv")
indicator_data <- read.csv("data/indicator_data.csv", stringsAsFactors = FALSE)

import_export_data <- select(dashboard_data, -country_code)
import_export_data <- left_join(import_export_data, indicator_data, by = "indicator_name")


# import_export_data <- import_export_data %>% group_by(country_name, region, metric, year) %>% filter(!is.na(metric)) %>% 
#   summarise(Merch_Imports = sum(case_when(scale == "merch_imports" ~ value, TRUE ~ 0)),
#             Merch_Exports = sum(case_when(scale == "merch_exports" ~ value, TRUE ~ 0)))

import_export_data <- import_export_data %>% 
  mutate(scale = case_when(scale == "merch_imports" ~ "Merchandise Imports", TRUE ~ "Merchandise Exports"))

merch_usd_data <- import_export_data %>% filter(metric == "Merchandise") 

import_export_data$country_name <- as.factor(import_export_data$country_name)
import_export_data$indicator_name <- as.factor(import_export_data$indicator_name)
import_export_data$region <- as.factor(import_export_data$region)
import_export_data$metric <- as.factor(import_export_data$metric)


ui <- dashboardPage(
  
  dashboardHeader(title = "Country Economic Health",
                  titleWidth = 280),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Country Dashboard", tabName = "country_dashboard", icon = icon("dashboard")),
    menuItem("Country Deepdive", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Country Comparison", tabName = "widgets", icon = icon("poll")),
    menuItem("Import Export Bar", tabName = "Imports", icon = icon("poll"))
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
              fluidRow(
                box(plotOutput("plot2", height = 600, width = "100%")),
                box(plotOutput("plot3", height = 600, width = "100%")),
                box(
                  sidebarPanel( 
                    sliderInput("num2", "Years to Include:",min = 1970, max = 2019,step=1,value=c(1970,2014), width = 600), width = 12)
                  
                ),
                box(
                  selectInput("filter_country2", "Country", 
                              choices=levels(import_export_data$country_name),
                              selected=levels(import_export_data$country_name)[1]),
                  selectInput("filter_indicator2", "Indicator", 
                              choices=levels(import_export_data$metric),
                              selected=levels(import_export_data$metric)[1], multiple = FALSE))
                
              )
      ),
      # Third tab content
      tabItem(tabName = "Imports",
              fluidRow(
                box(plotOutput("plot4", height = 600, width = "100%"), width = 8),
                box(sidebarPanel( 
                    sliderInput("filter_year3", "Year",min = 1970, max = 2014,step=1,value=2014, width = 600), width = "100%"), width = 4
                  
                )
                
              )
      ),
      tabItem(tabName = "country_dashboard",
              fluidRow(
                box(plotOutput("plot8", height = 450, width = "100%")), ## Top left
                box(plotOutput("plot9", height = 450, width = "100%")), ## Top Right  (this will be the map)
                
                box( ## Bottom Left
                  sidebarPanel( 
                    sliderInput("num3", "Years to Include:",min = 1970, max = 2019,step=1,value=c(1970,2014), width = 600), width = 12),
                  selectInput("filter_country3", "Country", 
                              choices=levels(import_export_data$country_name),
                              selected=levels(import_export_data$country_name)[1]),
                  selectInput("filter_indicator3", "Indicator", 
                              choices=levels(import_export_data$indicator_name),
                              selected=levels(import_export_data$indicator_name)[1], multiple = FALSE)),
                box(plotOutput("plot7", height = 450, width = "100%")) ## Bottom right
              )
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
  
  dat2 <- reactive({
    test2 <- import_export_data[import_export_data$year %in% seq(from=min(input$num2),to=max(input$num2),by=1) & 
                                  import_export_data$country_name == input$filter_country2 &
                                  import_export_data$metric == input$filter_indicator2,]
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(dat2(),aes(x=as.numeric(year),y=Merch_Imports)) + 
      geom_line() + 
      ylab(paste0(input$filter_indicator2, " as % of Merchandise Imports")) +
      xlab("Year") +
      xlim(min(input$num2), max(input$num2)) +
      theme_classic() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    
  },width = "auto")
  
  output$plot3 <- renderPlot({
    
    ggplot(dat2(),aes(x=as.numeric(year),y=Merch_Exports)) + 
      geom_line() + 
      ylab(paste0(input$filter_indicator2, " as % of Merchandise Exports")) +
      xlab("Year") +
      xlim(min(input$num2), max(input$num2)) +
      theme_classic() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    
  },width = "auto")
  
  
  dat3 <- reactive({
    test3 <- merch_usd_data[merch_usd_data$year == input$filter_year3,]
  })
  
  output$plot4 <- renderPlot({
    
    dat3() %>% filter(country_name != "World") %>% mutate(value = value/1000000000) %>% 
      ggplot(aes(x=country_name, y=value, fill = key)) +
      geom_bar(stat ="identity", position=position_dodge()) +
      ggtitle(paste0("Merchandise Imports & Exports in ", input$filter_year3)) +
      theme_classic() +
      xlab("Country") +
      facet_wrap(~region, scales = "free_x") +
      ylab("Current US Dollars (in Billions)") +
      scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "B")) +
      theme(legend.justification = "centre", legend.position = "top", plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    
  },width = "auto")
  
  
  dat8 <- reactive({
    metric <- import_export_data[import_export_data$indicator_name == input$filter_indicator3, 6][1]
    
    test <- import_export_data[import_export_data$year %in% seq(from=min(input$num3),to=max(input$num3),by=1) &
                                 import_export_data$country_name == input$filter_country3 &
                                 import_export_data$metric == metric &
                                 !is.na(import_export_data$metric),]
  })
  
  
  output$plot8 <- renderPlot({
    
    ggplot(dat8(),aes(x=as.numeric(year),y=value)) +
      geom_line() +
      ylab(" as % of Merchandise Imports/Exports") +
      xlab("Year") +
      xlim(min(input$num3), max(input$num3)) +
      theme_classic() +
      facet_wrap(~scale, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    
  },width = "auto")
  
  
  dat9 <- reactive({
    test3 <- merch_usd_data[merch_usd_data$year == max(input$num3),]
  })
  
  output$plot7 <- renderPlot({
    
    dat3() %>% filter(country_name != "World") %>% mutate(value = value/1000000000) %>% 
      ggplot(aes(x=country_name, y=value, fill = scale)) +
      geom_bar(stat ="identity", position=position_dodge()) +
      ggtitle(paste0("Total Merchandise Imports & Exports in ", max(input$num3))) +
      theme_classic() +
      xlab("Country") +
      facet_wrap(~region, scales = "free_x") +
      ylab("Current US Dollars (in Billions)") +
      scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "B")) +
      theme(legend.justification = "centre", legend.position = "top", plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    
  },width = "auto")
  
  
  
}

shinyApp(ui, server)
