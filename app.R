
if(!require("shiny")) install.packages('shiny')
if(!require("shinydashboard")) install.packages('shinydashboard')
if(!require("rworldmap")) install.packages('rworldmap')

library(shiny)
library(shinydashboard)
library(rworldmap)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)

dashboard_data <- read.csv("data/dashboard_data.csv")
world_data <- read.csv("data/world_data.csv")
indicator_data <- read.csv("data/indicator_data.csv", stringsAsFactors = FALSE)


import_export_data <- select(dashboard_data, -country_code)
import_export_data <- left_join(import_export_data, indicator_data, by = "indicator_name") %>% 
  mutate(scale = case_when(scale == "merch_imports" ~ "Merchandise Imports", TRUE ~ "Merchandise Exports"))


merch_usd_data <- import_export_data %>% filter(metric == "Merchandise") 


import_export_data$country_name <- as.factor(import_export_data$country_name)
import_export_data$indicator_name <- as.factor(import_export_data$indicator_name)
import_export_data$region <- as.factor(import_export_data$region)
import_export_data$metric <- as.factor(import_export_data$metric)

colourPalette <- brewer.pal(11,'RdYlBu')

ui <- dashboardPage(
  
  dashboardHeader(title = "Country Economic Health",
                  titleWidth = 280),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Country Dashboard", tabName = "main_dashboard", icon = icon("dashboard")),
    menuItem("Country Deepdive", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Country Comparison", tabName = "widgets", icon = icon("poll")),
    menuItem("Import Export Bar", tabName = "Imports", icon = icon("poll"))
  )),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "main_dashboard",
              fluidRow(
                box(plotOutput("import_export_plot", height = 450, width = "100%")), ## Top left
                box(plotOutput("map_plot", height = 450, width = "100%")), ## Top Right  (this will be the map)
                
                box( ## Bottom Left
                  sidebarPanel( 
                    sliderInput("num3", "Years to Include:",min = 1970, max = 2014,step=1,value=c(1970,2014), width = 600), width = 12),
                  selectInput("filter_country3", "Country", 
                              choices=levels(import_export_data$country_name),
                              selected=levels(import_export_data$country_name)[1]),
                  selectInput("filter_indicator3", "Indicator", 
                              choices=levels(import_export_data$indicator_name),
                              selected=levels(import_export_data$indicator_name)[1], multiple = FALSE)),
                box(plotOutput("import_export_bar", height = 450, width = "100%")) ## Bottom right
              )
      ),
      # Second Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("multi_ind_plot", height = 600, width = "100%")),
                
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
      # Third tab content
      tabItem(tabName = "widgets",
              fluidRow(
                box(plotOutput("import_export_plot2", height = 600, width = "100%"), width = 12),
                box(
                  sidebarPanel( 
                    sliderInput("num2", "Years to Include:",min = 1970, max = 2019,step=1,value=c(1970,2014), width = 600), width = 12)
                  
                ),
                box(
                  selectInput("filter_country2", "Country", 
                              choices=levels(import_export_data$country_name),
                              selected=levels(import_export_data$country_name)[1]),
                  selectInput("filter_indicator2", "Indicator", 
                              choices=levels(import_export_data$indicator_name),
                              selected=levels(import_export_data$indicator_name)[1], multiple = FALSE))
                
              )
      ),
      # Fourth tab content
      tabItem(tabName = "Imports",
              fluidRow(
                box(plotOutput("bar_plot", height = 600, width = "100%"), width = 8),
                box(sidebarPanel( 
                    sliderInput("filter_year3", "Year",min = 1970, max = 2014,step=1,value=2014, width = 600), width = "100%"), width = 4
                  
                )
                
              )
      ))
  )
  
  
)


server <- function(input, output) { 

  # Main Dashboard plots
  import_export_plot_data <- reactive({
    metric <- import_export_data[import_export_data$indicator_name == input$filter_indicator3, 6][1]
    
    output_data <- import_export_data[import_export_data$year %in% seq(from=min(input$num3),to=max(input$num3),by=1) &
                                 import_export_data$country_name == input$filter_country3 &
                                 import_export_data$metric == metric &
                                 !is.na(import_export_data$metric),]
  })
  
  output$import_export_plot <- renderPlot({
    
    ggplot(import_export_plot_data(),aes(x=as.numeric(year),y=value, color=scale)) +
      geom_line(size=1.5) +
      ylab(" as % of Merchandise Imports/Exports") +
      xlab("Year") +
      xlim(min(input$num3), max(input$num3)) +
      theme_classic() +
      facet_wrap(~scale, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_color_manual(values=c("#1b9e77", "#d95f02")) +
      theme(legend.position = "none") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text.x = element_text(size = 11, face="bold"), legend.text=element_text(size=12)
      )
    
  },width = "auto")
  
  # Map plot goes here
  map_data <- reactive({
    output_data <- world_data[world_data$year == max(input$num3) & 
                              world_data$indicator_name == input$filter_indicator3,]
  })
  
  output$map_plot <- renderPlot({
    par(mar=c(0, 0, 1, 0), pty = "m",xaxs = "r", xaxt = 's', xpd = NA, yaxs = "i", yaxt = 's')
    mapCountryData(joinCountryData2Map(map_data(),
                                       joinCode = "ISO3",
                                       nameJoinColumn = "country_code",
                                       verbose = FALSE),
                   nameColumnToPlot="value",
                   colourPalette=colourPalette,
                   mapTitle= map_data()$indicator_name[1]
                )
    
  },width = "auto")
  
  
  import_export_bar_data <- reactive({
    output_data <- merch_usd_data[merch_usd_data$year == max(input$num3),]
  })
  
  output$import_export_bar <- renderPlot({
    
    import_export_bar_data() %>% filter(country_name != "World") %>% mutate(value = value/1000000000) %>% 
      ggplot(aes(x=country_name, y=value, fill = scale)) +
      geom_bar(stat ="identity", position=position_dodge()) +
      ggtitle(paste0("Total Merchandise Imports & Exports in ", max(input$num3))) +
      theme_classic() +
      xlab("Country") +
      facet_wrap(~region, scales = "free_x") +
      ylab("Current US Dollars (in Billions)") +
      scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "B")) +
      theme(legend.justification = "centre", legend.position = "top", plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
      scale_fill_manual(values=c("#1b9e77", "#d95f02")) +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text.x = element_text(size = 11, face="bold"), legend.text=element_text(size=12)
      )
    
  },width = "auto")
  
  
  #### Second Tab Plots ####
  
  multi_ind_plot_data <- reactive({
    output_data <- dashboard_data[dashboard_data$year %in% seq(from=min(input$num),to=max(input$num),by=1) & 
                             (dashboard_data$country_name == input$filter_country | (dashboard_data$country_name == "World" & input$include_world == TRUE)) &
                             dashboard_data$indicator_name %in% as.vector(input$filter_indicator),]
  })
  
  output$multi_ind_plot <- renderPlot({
    
    ggplot(multi_ind_plot_data(),aes(x=as.numeric(year),y=value, color=indicator_name, linetype=country_name)) + 
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
  
  
  #### Third Tab Plot ####
  
  import_export_plot_data2 <- reactive({
    metric2 <- import_export_data[import_export_data$indicator_name == input$filter_indicator2, 6][1]
    
    output_data <- import_export_data[import_export_data$year %in% seq(from=min(input$num2),to=max(input$num2),by=1) &
                                 import_export_data$country_name == input$filter_country2 &
                                 import_export_data$metric == metric2 &
                                 !is.na(import_export_data$metric),]
  })
  
  output$import_export_plot2 <- renderPlot({
    
    ggplot(import_export_plot_data2(),aes(x=as.numeric(year),y=value)) +
      geom_line() +
      ylab(" as % of Merchandise Imports/Exports") +
      xlab("Year") +
      xlim(min(input$num2), max(input$num2)) +
      theme_classic() +
      facet_wrap(~scale, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    
  },width = "auto")
  
  
  #### Fourth Tab Plot ####
  
  
  bar_data <- reactive({
    output_data <- merch_usd_data[merch_usd_data$year == input$filter_year3,]
  })
  
  output$bar_plot <- renderPlot({
    
    bar_data() %>% filter(country_name != "World") %>% mutate(value = value/1000000000) %>% 
      ggplot(aes(x=country_name, y=value, fill = scale)) +
      geom_bar(stat ="identity", position=position_dodge()) +
      ggtitle(paste0("Merchandise Imports & Exports in ", input$filter_year3)) +
      theme_classic() +
      xlab("Country") +
      facet_wrap(~region, scales = "free_x") +
      ylab("Current US Dollars (in Billions)") +
      scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "B")) +
      theme(legend.justification = "centre", legend.position = "top", plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    
  },width = "auto")


}

shinyApp(ui, server)
  