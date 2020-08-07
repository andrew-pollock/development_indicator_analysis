
if(!require("shiny")) install.packages('shiny')
if(!require("shinydashboard")) install.packages('shinydashboard')
if(!require("rworldmap")) install.packages('rworldmap')
if(!require("RColorBrewer")) install.packages('RColorBrewer')
if(!require("dplyr")) install.packages('dplyr')
if(!require("tidyr")) install.packages('tidyr')
if(!require("ggplot2")) install.packages('ggplot2')


## Load the data
dashboard_data <- read.csv("data/dashboard_data.csv", stringsAsFactors = TRUE)
world_data     <- read.csv("data/world_data.csv", stringsAsFactors = FALSE)
indicator_data <- read.csv("data/indicator_data.csv", stringsAsFactors = FALSE)
gdp_data       <- read.csv("data/world_gdp_per_capita.csv", stringsAsFactors = FALSE)


# Filter GDP Data to just countries in dashboard_data
gdp_data <- filter(gdp_data, country_code %in% levels(dashboard_data$country_code))


# Create a new table with additional data on whether each indicator is Imports or Exports
import_export_data <- select(dashboard_data, -country_code) %>% inner_join(indicator_data, by = "indicator_name")


## Create the dataset for the Bottom-Right Merch Import/Export Bar Chart
merch_usd_data <- import_export_data %>% filter(metric == "Merchandise") %>% mutate(region = as.character(region)) %>%
                                          mutate(region = case_when(region == "Europe & Central Asia" ~ "Europe",
                                                                    region == "East Asia & Pacific" ~ "East Asia",
                                                                    TRUE ~ region))


# Create country and indicator filters for the input boxes
country_filter   <- data.frame(country_name = factor(c("Canada", "France", "Germany", "Japan", "South Korea", "United States")))
indicator_filter <- indicator_data %>% filter(!indicator_name %in% c("Merchandise exports (current US$)", 
                                                                     "Merchandise imports (current US$)")) %>% 
                        select(indicator_name) %>% mutate(indicator_name = factor(indicator_name))


# Creates the options for country to compare against (has "None" as an option)
comparison_country_filter <- data.frame(country_name = factor(x = c("None", levels(country_filter$country_name)), 
                                                        levels = c("None", "Canada", "France", "Germany", "Japan", "South Korea", "United States")))

# Create a custom colour palette for the map
colourPalette <- c("#40004b","#762a83","#9970ab","#c2a5cf","#e7d4e8","#f7f7f7",
                   "#d9f0d3","#a6dba0","#5aae61","#1b7837","#00441b")

ui <- dashboardPage(
  
  dashboardHeader(title = textOutput("country_title"),
                  titleWidth = "100%"),
  
  dashboardSidebar(disable = TRUE),

  dashboardBody(
      tabItem(tabName = "main_dashboard",
              fluidRow( # Top row, just contains 2 graphs
                box(plotOutput("import_export_plot", height = 450, width = "100%")), ## Top left
                box(plotOutput("map_plot", height = 450, width = "100%")), ## Top Right  (this will be the map)
              ),
              # Bottom Row, filters in the first column, Value boxes in the second and a graph on the right side
              fluidRow(column(width = 4,
                              box(title = "Inputs", status = "primary", solidHeader = TRUE, ## Bottom Left
                                sidebarPanel( 
                                  sliderInput("num3", "Years to Include:",min = 1970, max = 2019,step=1,value=c(1970,2014), width = 600),
                                  
                                  
                                  fluidRow(column(width = 8, selectInput("filter_country", "Country", 
                                                                         choices=levels(country_filter$country_name),
                                                                         selected=levels(country_filter$country_name)[1])),
                                           column(width = 4, checkboxInput("include_world", "Include World?", value = FALSE))),
                                  
                                  fluidRow(column(width = 8, selectInput("filter_comparison_country", "Country to Compare Against", 
                                                                         choices=levels(comparison_country_filter$country_name),
                                                                         selected=levels(comparison_country_filter$country_name)[1])),
                                           column(width = 4, checkboxInput("include_trend", "Include Trendline?", value = FALSE))),
                                 selectInput("filter_indicator", "Indicator", 
                                             choices=levels(indicator_filter$indicator_name),
                                             selected=levels(indicator_filter$indicator_name)[1], multiple = FALSE) 
                                , width = "100%", height = 350), width = 12, height = 440)
                              
                              
                              ),  
                       # Bottom row, Second column, contains value boxes
                       column(width = 2,
                              titlePanel(h1(strong(textOutput("country_title2"), align = "center"))),
                                             valueBoxOutput("progressBox", width = "100%"), 
                                             valueBoxOutput("progressBox2", width = "100%"),
                                             valueBoxOutput("progressBox3", width = "100%")
                              ),
                       # Bottom row, right hand side, contains Imports vs Exports bar chart
                       column(width = 6, box(plotOutput("import_export_bar", height = 420, width = "100%"), width = "100%")) ## Bottom right
              )
      ))
)



server <- function(input, output) { 

  import_export_plot_data <- reactive({
    metric <- import_export_data[import_export_data$indicator_name == input$filter_indicator, 6][1]
    
    output_data <- import_export_data[import_export_data$year %in% seq(from=min(input$num3),to=max(input$num3),by=1) &
                                 (import_export_data$country_name == input$filter_country  | 
                                  (import_export_data$country_name == "World" & input$include_world == TRUE) |
                                  import_export_data$country_name == input$filter_comparison_country) &
                                 import_export_data$metric == metric &
                                 !is.na(import_export_data$metric),]
  })
  
  output$country_title <- renderText({ paste0(as.character(import_export_plot_data()$country_name[1]), 
                                              " Merchandise Imports & Exports from ",
                                              as.character(min(input$num3)), 
                                              " to ",
                                              as.character(pmin(max(input$num3), 2014))
                                              ) })
  
  output$country_title2 <- renderText({ paste0(as.character(import_export_plot_data()$country_name[1]), 
                                              " ", as.character(pmin(max(input$num3), 2014))
  ) })
  
  
  output$import_export_plot <- renderPlot({
    
    ggplot(import_export_plot_data(),aes(x=as.numeric(year),y=value, #color=scale, 
                                         color=country_name)) +
      geom_line(size=1.5) +
      labs(x = "Year", y = paste0(as.character(import_export_plot_data()$metric), " Imports & Exports")) +
      xlim(min(input$num3), max(input$num3)) +
      theme_classic() +
      facet_wrap(~scale, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      scale_color_manual(values=c("#1f78b4", "#b2df8a", "#a6cee3"), 
                         guide = if(!input$include_world & input$filter_comparison_country == "None") 'none' else "legend" ) +
      theme(legend.position = "none") +
      scale_linetype_discrete(name = "Country", guide = if(!input$include_world) 'none' else "legend") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.text.x = element_text(size = 11, face="bold"), legend.text=element_text(size=12),
        legend.justification = "centre", legend.position = "bottom", legend.key.width=unit(2.5, "line"), legend.title=element_blank()
      ) +
      {if(input$include_trend)stat_smooth(method="glm", formula = y ~ splines::bs(x, 3), fullrange=TRUE, se = FALSE, 
                                          linetype = "dashed", aes(group = country_name), show.legend = FALSE)}

    
  },width = "auto")
  
  kpi_data <- reactive({
    output_data <- dashboard_data[dashboard_data$year == pmin(max(input$num3), 2014) & 
                                    (dashboard_data$country_name == input$filter_country) &
                                    dashboard_data$indicator_name == input$filter_indicator,]
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(kpi_data()$value*100, "%"), 
      gsub("\\(.+", "", kpi_data()$indicator_name),
      icon = icon("percent"),
      color = "purple"
    )
  })
  
  kpi_rank_data <- reactive({
    year_indicator_data <- dashboard_data[dashboard_data$year == pmin(max(input$num3), 2014) &
                                          dashboard_data$indicator_name == input$filter_indicator &
                                          dashboard_data$country_name != "World",]
    year_indicator_data$rank <- scales::ordinal(rank(-year_indicator_data$value))
    year_indicator_data <- year_indicator_data[year_indicator_data$country_name == input$filter_country,]

  })
  
  output$progressBox2 <- renderValueBox({
    valueBox(
      kpi_rank_data()$rank, paste0("Highest ", gsub("\\(.+", "", kpi_data()$indicator_name)), icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  gdp_rank_data <- reactive({
    gdp_rank <- gdp_data[gdp_data$year == pmin(max(input$num3), 2014) &
                                      gdp_data$country_name != "World",]
    gdp_rank$rank <- scales::ordinal(rank(-gdp_rank$value))
    gdp_rank <- gdp_rank[gdp_rank$country_name == input$filter_country,]

  })
  
  output$progressBox3 <- renderValueBox({
    valueBox(
      gdp_rank_data()$rank, "Highest GDP per capita", icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  # Map plot goes here
  map_data <- reactive({
    output_data <- world_data[world_data$year == pmin(max(input$num3), 2014) & 
                              world_data$indicator_name == input$filter_indicator,]
  })
  
  output$map_plot <- renderPlot({
    par(mar=c(0, 0, 1, 0), pty = "m",xaxs = "r", xaxt = 's', xpd = NA, yaxs = "i", yaxt = 's')
    mapCountryData(joinCountryData2Map(map_data(),
                                       joinCode = "ISO3",
                                       nameJoinColumn = "country_code",
                                       verbose = FALSE),
                   nameColumnToPlot="value",
                   colourPalette=colourPalette, numCats = 11,
                   mapTitle= paste0(map_data()$year[1], " ", map_data()$indicator_name[1])
                )
    
  },width = "auto")
  
  
  import_export_bar_data <- reactive({
    output_data <- merch_usd_data[merch_usd_data$year == pmin(max(input$num3), 2014),]
  })
  
  output$import_export_bar <- renderPlot({
    import_export_bar_data() %>% filter(country_name != "World") %>% mutate(value = value/1000000000) %>% 
      ggplot(aes(x=country_name, y=value, fill = scale)) +
      geom_bar(stat ="identity", position=position_dodge(), color = "black") +
      theme_classic() +
      labs(title = paste0("Total Merchandise Imports & Exports in ", pmin(max(input$num3), 2014)),
           x = "Country", y = "Current US Dollars (in Billions)") +
      facet_grid(region~., scales = "free_y") +
      scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "B")) +
      scale_fill_manual(values=c("#1b9e77", "#d95f02")) +
      theme(
        legend.justification = "centre", legend.position = "top", legend.title = element_blank(),
        plot.title   = element_text(size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=13, face="bold"),
        axis.title.y = element_text(size=13, face="bold"),
        axis.text.x  = element_text(size=12),
        axis.text.y  = element_text(size=12),
        strip.text.y = element_text(size=12, face="bold"), legend.text=element_text(size=12)) + 
      coord_flip()
  },width = "auto")
  
}


shinyApp(ui, server)
