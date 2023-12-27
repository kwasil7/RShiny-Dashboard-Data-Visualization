#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
## ui.R ##
library(shinythemes)
library(ggplot2)
library(gganimate)
library(readr)
library(shiny)
library(shinyjs)
library(lubridate)
library(dplyr)
library(leaflet)
library(tidyverse)
library(tidygeocoder)
library(usmap)
library(maps)
library(mapdata)
library(fuzzyjoin)
library(scales)
library(sf)
library(tmaptools)
library(RColorBrewer)
library(reshape2)
library(readxl)
library(shinycssloaders)
# source("helpers.R")

Sys.setlocale("LC_ALL", "English")

linebreaks <- function(n){HTML(strrep(br(), n))}

# Color-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load data frame
electric_data <- read_csv("data/Electric_Vehicle_Population_Size_History_By_County.csv", 
                          col_names = c("Date", "County", "State", "Vehicle_Primary_Use", "BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles", "Percent_EV"))

# Remove the first row
electric_data <- electric_data[-1, ]

# Convert Date from character to Date object using lubridate's mdy(), for the entire dataframe
electric_data$Date <- mdy(electric_data$Date)

# Convert EV_Total to numeric for the entire dataframe
electric_data$EV_Total <- as.numeric(electric_data$EV_Total)

electric_data$BEVs <- as.numeric(electric_data$BEVs)

electric_data$PHEVs <- as.numeric(electric_data$PHEVs)

electric_data$Percent_EV <- as.numeric(electric_data$Percent_EV)

electric_data$NonEV_Total <- as.numeric(electric_data$NonEV_Total)

electric_data$Total_Vehicles <- as.numeric(electric_data$Total_Vehicles)

# Conversion to a factor
electric_data$Vehicle_Primary_Use <- as.factor(electric_data$Vehicle_Primary_Use)

# Filter for King County data, excluding the first row
king_county_data <- subset(electric_data, County == "King")

# Filter for Pierce County data
pierce_county_data <- subset(electric_data, County == "Pierce")

# the map defined below
country_data <- read.csv("processed_country_data.csv")

######## POPULATION FILTERING
county_population <- read_excel("data/Washington_County_Population_2023.xlsx")

# Filter electric_data for Washington State
electric_data <- electric_data %>%
  filter(State == "WA")

# Standardize county names in electric_data
electric_data$County <- gsub(" County", "", electric_data$County)

# Standardize county names in county_population
county_population$County <- gsub(" County", "", county_population$County)

# Join the dataframes
electric_data <- electric_data %>%
  left_join(county_population, by = "County")

# Handle NA values in Population
electric_data <- electric_data %>%
  mutate(Population = ifelse(is.na(Population), 0, Population))

######## POPULATION FILTERING

usa <- map_data('usa')

# Getting map data for all US counties
counties <- map_data("county")

counties_data <- electric_data %>%
  mutate(county = tolower(County),
         state = tolower(State))

electric_data_state_level <- electric_data %>%
  group_by(State) %>%
  summarise(EV_Total = sum(EV_Total, na.rm = TRUE))

electric_data_state_level <- electric_data_state_level %>%
  rename(state = State)

# Define a named vector of colors for counties
county_colors <- setNames(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                            "#8c564b", "#e377c2", "#b05f5f", "#e67315", "#17becf", "#ffc0cb",
                            "#eb4034", "#c9eb34", "#8c6701", "#01128c", "#1f4d2e", "#21b53e", "#827e14", "#950350", "#732210", "#0ecf15",
                            "#edd02b", "#962bed", "#ed2bb3", "#ed522b", "#178a6f", "#573209", "#42382c", "#6e3cc9", "#8a1127", "#109c1e"),
                          c("King", "Pierce", "Clark", "Whatcom", 
                            "Snohomish", "Spokane", "Thurston", "Yakima", 
                            "Island", "Benton", "Kitsap", 
                            "Jefferson", "Pend Oreille", "Columbia", "Asotin", "Walla Walla", "San Juan", "Skagit", "Kittitas", "Ferry", "Garfield",
                            "Lewis", "Grant", "Cowlitz", "Franklin", "Clallam", "Stevens", "Grays Harbor", "Okanogan", "Chelan", "Douglas"))


# Define UI
ui <- navbarPage("Data Visualization Group 15 Project", theme = shinytheme("superhero"),
                 
  tabPanel("Home",
          fluidPage(
            tags$h1("Welcome to the EV Data Dashboard"),
            tags$a("Click here for the dataset source", href = "https://data.wa.gov/Transportation/Electric-Vehicle-Population-Size-History-By-County/3d5d-sdqb"),
            downloadButton("downloadButton", "Download Report"),
            linebreaks(2),
            tags$h4("In January 2023, the number of electric vehicles registered in Washington reached more than 100.000", style = "font-weight: bold;"),
            tags$p("However, when it comes to the percent of electric vehicles (for example, in King County) versus their non-electric counterparts 
                   the plot may look less impressive."),
          ),
          mainPanel(
            plotOutput(outputId = "jan_2023"),
            tableOutput(outputId = "jan_2023_table"),
            plotOutput(outputId = "king_percent"),
            plotOutput(outputId = "king_plot"),
            tags$p("King County is the most populated county in Washington"),
            plotOutput(outputId = "yakima_plot"),
            tags$p("Yakima County is the most populated rural county in the state."),
            plotOutput(outputId = "garfield_plot"),
            tags$p("Garfield County is the least populated county in Washington. It is also a rural county."),
            plotOutput(outputId = "island_plot"),
            tags$p("Island County is composed entirely of islands and classied as rural."),
            "What about a total number of registered vehicles across time?",
            plotOutput(outputId = "total_plot"),
            "Electric cars and electric trucks over time in King County",
            div(
              tags$img(src = "barplot_animate.gif", alt = "Animated Bar Plot", width = "100%", height = "auto"),
              style = "text-align: left; width: 50%;"  # Adjust width as needed
            )
          )
  ),
  
  tabPanel("Analysis",
          sidebarLayout(
            sidebarPanel(
              helpText("Explore the Electric Vehicle Population Size History By County Dataset"),
              selectInput(inputId = "y", label = "Y-axis:", choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles"), selected = "EV_Total"),
              selectInput(inputId = "x", label = "X-axis:", choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles", "Date"), selected = "Date"),
              selectInput(inputId = "z", label = "Color by:", choices = c("Vehicle_Primary_Use", "BEVs", "PHEVs"), selected = "Vehicle_Primary_Use"),
              sliderInput(inputId = "alpha", label = "Alpha:", min = 0, max = 1, value = 0.5),
              selectInput(inputId = "selected_type", label = "Select vehicle type:", choices = c("Passenger", "Truck"), selected = "Passenger"),
              sliderInput('years', 'Years', min = 2017, max = 2024, value = c(2017, 2024)),
              selectInput(inputId = "date_interval",
                          label = "Date Interval:",
                          choices = c("Yearly", "Quarterly", "Monthly"),
                          selected = "Monthly")
            ),
            mainPanel(
              plotOutput(outputId = "scatterplot"),
              plotOutput(outputId = "densityplot", height = 200),
              plotOutput(outputId = "lineplot"),
              plotOutput(outputId = "lineplot_cars"),
              plotOutput(outputId = "hist_bevs_king"),
              plotOutput(outputId = "hist_phevs_king"),
              plotOutput(outputId = "pierce_plot"),
              plotOutput(outputId = "yakima_lollipop_plot")
            )
          )
  ),
  
  tabPanel("Maps",
           fluidRow(
             column(width = 12,
                imageOutput("rural_wa", height = "100%"),
                tags$br(),
                "Sources: ",
                tags$a("Washington State Department of Health", href='https://doh.wa.gov/sites/default/files/legacy/Documents/Pubs/609003.pdf'),
                " and ",
                tags$a("Wikipedia", href='https://en.wikipedia.org/wiki/List_of_counties_in_Washington'),
                tags$br(),
                "There are 9 urban counties and 30 rural counties in Washington state. King County is the most populated in the state.
                Yakima is the most populated rural county with 257.001 people.",
                tags$br(), tags$br(),
                titlePanel("Washington State, US"),
                plotOutput("map_wa"),
                plotOutput("map_wa_counties"),
                titlePanel("Washington State Vehicle Registrations Across the US"),
                plotOutput("map_states", height = "600px"),
                plotOutput("map_states_no_wa", height = "600px")
             )
           )
  ),
  tabPanel("Interactive Graphs",
           sidebarLayout(
             sidebarPanel(
               sliderInput("yearSlider", "Select Year:",
                           min = 2017, max = 2024, value = c(2017, 2024)),
               selectInput("dataCategory", "Select Vehicle Category:",
                           choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles"),
                           selected = "EV_Total"),
               checkboxInput("togglePercentage", "Show registrations as Percentage of Population", value = FALSE),
               selectInput(inputId = "numCounties",
                           label = "Amount of Counties:",
                           choices = c("Top 5 registrations" = 5, "Top 10 registrations" = 10, "Top 25 registrations" = 25, "All registrations" = Inf),
                           selected = 10)
             ),
             mainPanel(
               plotOutput("countyBarGraph"),
               plotOutput("stackedAreaPlot"),
               plotOutput("countyBoxplot"),
               plotOutput("timeSeriesPlot"),
               fluidRow(
                 column(6, plotOutput("evTimeSeriesPlot")),
                 column(6, plotOutput("nonEvTimeSeriesPlot"))
               ),
               plotOutput("countyHeatmap")
             )
           )),
  
  tabPanel("Registration Trends Heatmap",
           sidebarLayout(
             sidebarPanel(
               selectInput("selectedYear", "Select Year:",
                           choices = sort(unique(format(electric_data$Date, "%Y"))),
                           selected = max(format(electric_data$Date, "%Y")))
               ,
               selectInput("selectedVehicleType", "Select Vehicle Type:",
                           choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles"),
                           selected = "EV_Total"))
               ,
             mainPanel(
               plotOutput("monthlyRegistrationsHeatmap"),
               plotOutput("allTimeVehicleTypeHeatmap")
             )
           )
  ),
  tabPanel("WA Registration Analysis",
           fluidPage(
             titlePanel("Analysis of WA Registered Vehicles Outside the State"),
             textOutput("totalVehiclesOutsideWA"),
             plotOutput("usHeatmap"),
             actionButton("toggleButton1", "Show/Hide Specifics"),  # Button to toggle the plot
             div(id = "plotDiv",  # Div container for the plot
                 plotOutput("waAnalysisPlot", height = "2000px")
            )
           )
  )
  
  # Add more tabPanel for other pages if needed
)

# Define server 

server <- function(input, output, session) {
  
  output$downloadButton <- downloadHandler(
    filename = function() {
      "report.pdf" # The name of the file that will be downloaded
    },
    content = function(file) {
      # The direct link to the Google Doc with permissions set so anyone with the link can view
      doc_link <- "https://docs.google.com/document/d/1SMbbhp9OPY2cW_8Jpb9VrC_2hU1QeSoIgbnKxrJ3CF0/export?format=pdf"
      
      # Use httr to handle the request and download the file
      httr::GET(url = doc_link, httr::write_disk(file, overwrite = TRUE))
    }
  )
  
  # Create a subset of data filtering for chosen title types
  king_county_data_subset <- reactive({
    req(input$selected_type)
    filter(king_county_data, Vehicle_Primary_Use == input$selected_type)
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data = king_county_data, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha) +
      labs(title = "The Scatterplot")
  })
  
  output$densityplot <- renderPlot({
    ggplot(data = king_county_data, aes_string(x = input$x)) +
      geom_density() +
      labs(title = "The Density Plot")
  })
  
  output$lineplot <- renderPlot({
    # Filter data for electric trucks
    truck_data <- electric_data %>%
      filter(Vehicle_Primary_Use == "Truck")
    
    # Summarize data to get total count by Date for trucks
    summarized_truck_data <- truck_data %>%
      group_by(Date) %>%
      summarize(Total_EV_Trucks = sum(EV_Total, na.rm = TRUE))
    
    # Create line plot for electric trucks
    ggplot(data = summarized_truck_data, aes(x = Date, y = Total_EV_Trucks)) +
      geom_line() + 
      theme_minimal() +
      labs(title = "Trend in Electric Truck Registrations Over Time",
           x = "Date",
           y = "Total Electric Truck Registrations") +
      theme(text = element_text(size = 14)) + 
      annotate("label", x = as.Date("2022-01-31"), y = max(summarized_truck_data$Total_EV_Trucks, na.rm = TRUE), 
                                                      label = "Gas vehicle ban 2035 announced on Jan 2022", 
                                                      color = "#D55E00", 
                                                      size = 5,
                                                      angle = 45, 
                                                      fontface = "bold")
  })
  
  output$lineplot_cars <- renderPlot({
    # Filter data for electric trucks
    cars_data <- electric_data %>%
      filter(Vehicle_Primary_Use == "Passenger")
    
    # Summarize data to get total count by Date for cars
    summarized_car_data <- cars_data %>%
      group_by(Date) %>%
      summarize(Total_EV_Cars = sum(EV_Total, na.rm = TRUE))
    
    # Create line plot for electric trucks
    ggplot(data = summarized_car_data, aes(x = Date, y = Total_EV_Cars)) +
      geom_line() + 
      theme_minimal() +
      labs(title = "Trend in Electric Cars Registrations Over Time",
           x = "Date",
           y = "Total Electric Cars Registrations") +
      theme(text = element_text(size = 14))
  })
  
  output$hist_bevs_king <- renderPlot({
    hist(king_county_data$BEVs, 
         main = "Histogram of BEVs in King County",  # Add your title here
         xlab = "BEVs",  # Label for the x-axis
         ylab = "Frequency")  # Label for the y-axis
  })
  
  output$hist_phevs_king <- renderPlot({
    hist(king_county_data$PHEVs,
         main = "Histogram of PHEVs in King County",  # Add your title here
         xlab = "PHEVs",  # Label for the x-axis
         ylab = "Frequency")  # Label for the y-axis)
  })
  
  output$pierce_plot <- renderPlot({
    # Determine date breaks and labels based on selected interval
    date_interval <- input$date_interval
    date_breaks <- switch(date_interval,
                          "Yearly" = "1 year",
                          "Quarterly" = "3 months",
                          "Monthly" = "1 month")
    
    date_labels <- switch(date_interval,
                          "Yearly" = "%Y",
                          "Quarterly" = "%b %Y",
                          "Monthly" = "%b %Y")
    
    # Create a new column for grouping based on the selected interval
    pierce_county_data$DateGroup <- cut(as.Date(pierce_county_data$Date), breaks = date_breaks)
    
    # Convert DateGroup to class "Date"
    pierce_county_data$DateGroup <- as.Date(pierce_county_data$DateGroup)
    
    # Get unique levels for discrete x-axis
    unique_levels <- unique(pierce_county_data$DateGroup)
    
    # Plot data using bars
    ggplot(pierce_county_data, aes(x = DateGroup, y = EV_Total)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      scale_x_date(breaks = unique_levels, labels = format(unique_levels, date_labels), date_labels = date_labels) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(size = 8)) +
      labs(title = "Number of Electric Vehicles Over Time in Pierce County",
           x = "Date",
           y = "Number of Electric Vehicles") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$king_plot <- renderPlot({
    # Plot data using bars
    ggplot(king_county_data, aes(x = king_county_data$Date, y = king_county_data$EV_Total)) +
      geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # Format date breaks and labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis text
            axis.text.y = element_text(size = 8)) +  # Adjust y-axis text size
      labs(title = "Number of Electric Vehicles Over Time in King County",
           x = "Date",
           y = "Number of Electric Vehicles") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$yakima_plot <- renderPlot({
    
    yakima_county_data <- subset(electric_data, County == "Yakima")
    
    # Plot data using bars
    ggplot(yakima_county_data, aes(x = yakima_county_data$Date, y = yakima_county_data$EV_Total)) +
      geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # Format date breaks and labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis text
            axis.text.y = element_text(size = 8)) +  # Adjust y-axis text size
      labs(title = "Number of Electric Vehicles Over Time in Yakima County",
           x = "Date",
           y = "Number of Electric Vehicles") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$garfield_plot <- renderPlot({
    
    garfield_county_data <- subset(electric_data, County == "Garfield")
    
    # Plot data using bars
    ggplot(garfield_county_data, aes(x = garfield_county_data$Date, y = garfield_county_data$EV_Total)) +
      geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # Format date breaks and labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis text
            axis.text.y = element_text(size = 8)) +  # Adjust y-axis text size
      labs(title = "Number of Electric Vehicles Over Time in Garfield County",
           x = "Date",
           y = "Number of Electric Vehicles") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$island_plot <- renderPlot({
    
    island_county_data <- subset(electric_data, County == "Island")
    
    # Plot data using bars
    ggplot(island_county_data, aes(x = island_county_data$Date, y = island_county_data$EV_Total)) +
      geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # Format date breaks and labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis text
            axis.text.y = element_text(size = 8)) +  # Adjust y-axis text size
      labs(title = "Number of Electric Vehicles Over Time in Island County",
           x = "Date",
           y = "Number of Electric Vehicles") +
      theme(plot.title = element_text(face = "bold"))
  })
  
   output$map <- renderPlot({
     plot_usmap(regions = "states") + 
       labs(title = "U.S. States",
            subtitle = "This is a blank map of the United States.") + 
       theme(panel.background=element_blank())
   })
  
   output$map_wa <- renderPlot({
     plot_usmap(include = c("WA")) + 
       labs(title = "The State of Washington") + 
       theme(panel.background = element_rect(color = "blue"))
   })
   
   output$map_wa_counties <- renderPlot({
     
     state <- map_data("state")
     
     washington <- subset(state, region=="washington")
     counties <- map_data("county")
     washington_county <- subset(counties, region=="washington")
     
     ggplot(data=washington, mapping=aes(x=long, y=lat, group=group)) + 
       coord_fixed(1.3) + 
       geom_polygon(color="black", fill="gray") + 
       geom_polygon(data=washington_county, fill=NA, color="white") + 
       geom_polygon(color="black", fill=NA) + 
       ggtitle('Washington Map with Counties') + 
       theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
             axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
   })
   
   output$map_states <- renderPlot({
     
     custom_labels <- c("1000", "3M", "5M")
     
     plot_usmap(data = electric_data_state_level, values = "EV_Total", regions = "states") +
       scale_fill_continuous(
         name = "Total EVs", 
         limits = c(0, 6000000),
         low = "#999999", 
         high = "#56B4E9",
         breaks = c(1000, 3000000, 5000000),
         labels = custom_labels
       ) +
       guides(fill = guide_colourbar(barwidth = 40, barheight = 1)) +
       theme(legend.position = "bottom", text = element_text(size = 12)) +
       labs(title = "Distribution of Electric Vehicles registered in Washington in the US")
   })
   
   output$map_states_no_wa <- renderPlot({
     
     custom_labels <- c("100", "1000", "3000", "7000")
     
     # Filter out the rows corresponding to Washington (WA)
     filtered_data <- electric_data_state_level %>%
       filter(state != "WA")
     
     plot_usmap(data = filtered_data, values = "EV_Total", regions = "states") +
       scale_fill_continuous(
         name = "Total EVs", 
         limits = c(0, 7000),
         low = "#999999", 
         high = "#56B4E9",
         breaks = c(100, 1000, 3000, 7000),
         labels = custom_labels
       ) +
       guides(fill = guide_colourbar(barwidth = 20, barheight = 1)) +
       theme(legend.position = "bottom", text = element_text(size = 14)) +
       labs(title = "Distribution of Electric Vehicles registered in the Washington state in the US (excluding Washington)")
   })
   
   output$jan_2023 <- renderPlot({
     # Filter for January 31st of each year
     year_data <- electric_data %>%
       filter(format(Date, "%m-%d") == "01-31" & 
                year(Date) %in% c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) %>%
       group_by(Year = year(Date)) %>%
       summarize(Total_EV = sum(EV_Total, na.rm = TRUE))
     
     # Create the bar plot
     ggplot(year_data, aes(x = as.factor(Year), y = Total_EV)) + 
       geom_bar(stat = "identity", fill = "#56B4E9") +
       labs(title = "Electric Vehicles Total Each Year (2017-2023)", 
            x = "Year", 
            y = "Total Registered Electric Vehicles") +
       theme_minimal() +
       theme(plot.title = element_text(face = "bold"))
   })
   
   output$jan_2023_table <- renderTable({
     
     filter_date <- as.Date("2023-01-31")
     
       electric_data %>%
         select(-State) %>%
         filter(Date == filter_date, EV_Total > 1000) %>%
         mutate(Date = as.character(Date)) %>%
         head(10)
     },
     caption = "Data for January 31, 2023 (first 10 rows)"
   )
   
   output$king_percent <- renderPlot({
     # Filter for December 31st of each year in King County
     king_percent_data <- electric_data %>%
       filter(County == "King" & 
                format(Date, "%m-%d") == "12-31" & 
                year(Date) %in% c(2017, 2018, 2019, 2020, 2021, 2022)) %>%
       mutate(Year = year(Date)) %>%
       select(Year, Percent_EV)  # Select only the Year and Percent_EV
     
     # Create the bar plot
     ggplot(king_percent_data, aes(x = as.factor(Year), y = Percent_EV)) + 
       geom_col(fill = "#56B4E9") +
       labs(title = "Percentage of Electric Vehicles at Year-End in King County (2017-2022)", 
            x = "Year", 
            y = "Percentage of Electric Vehicles") +
       scale_y_continuous(labels = scales::percent_format(scale = 1),  limits = c(0, 100)) +
       theme_minimal() +
       theme(plot.title = element_text(face = "bold"))
   })
   
   
   output$rural_wa <- renderImage({
     list(
       src = "images/rural_wa.png",  # Adjust the path and file name as needed
       contentType = "image/png",
       width = 800
     )
   }, deleteFile = FALSE)
   
   output$yakima_lollipop_plot <- renderPlot({
     # Filter for Yakima County data and aggregate by year
     yakima_yearly_data <- electric_data %>%
       filter(County == "Yakima") %>%
       group_by(Year = year(Date)) %>%
       summarize(Total_PHEVs = sum(PHEVs, na.rm = TRUE))
     
     # Create the lollipop plot for PHEVs over the years
     ggplot(yakima_yearly_data, aes(x = as.factor(Year), y = Total_PHEVs)) +
       geom_segment(aes(xend = as.factor(Year), yend = 0), color = "grey") +
       geom_point(color = "#56B4E9", size = 3) +
       labs(title = "Yearly Count of PHEVs in Yakima County",
            x = "Year",
            y = "Number of PHEVs") +
       theme_minimal() +
       theme(axis.title.x=element_text(size=13), 
             axis.title.y=element_text(size=13))
   })
   
   output$total_plot <- renderPlot({
     # Filter data for the end of each year
     end_of_year_data <- electric_data %>%
       mutate(Year = year(Date)) %>%
       group_by(Year) %>%
       filter(Date == max(Date)) %>%
       summarise(Total_Vehicles = sum(Total_Vehicles, na.rm = TRUE)) %>%
       ungroup()
     
     ggplot(end_of_year_data, aes(x = Year, y = Total_Vehicles)) +
       geom_line(color = "#0072B2") +
       xlab("Year") +
       ylab("Total Vehicles Registered by Year-End") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
   
   
   output$countyHeatmap <- renderPlot({
     req(input$yearSlider, input$dataCategory, input$numCounties)
     
     num_counties <- as.numeric(input$numCounties)
     
     # Join the electric_data with county_population
     wa_county_data <- electric_data %>%
       filter(year(Date) == input$yearSlider, State == "WA") %>%
       group_by(County) %>%
       summarise(EV_Total_County = sum(!!sym(input$dataCategory), na.rm = TRUE)) %>%
       left_join(county_population, by = "County") %>%
       mutate(Population = as.numeric(Population))
     
     # Calculate registration percentage
     wa_county_data <- wa_county_data %>%
       mutate(Registration_Percentage = (EV_Total_County / Population) * 100)
     
     # Select top counties based on toggle state and user selection
     top_counties <- if(input$togglePercentage) {
       wa_county_data %>%
         arrange(desc(Registration_Percentage)) %>%
         slice_head(n = num_counties)
     } else {
       wa_county_data %>%
         arrange(desc(EV_Total_County)) %>%
         slice_head(n = num_counties)
     }
     
     # Filter for top counties
     wa_county_data <- wa_county_data %>%
       filter(County %in% top_counties$County)
     
     # Plot of heatmap for counties in Washington
     ggplot(wa_county_data, aes(x = County, y = "", fill = if(input$togglePercentage) Registration_Percentage else EV_Total_County)) +
       geom_tile() +
       scale_fill_gradient(low = "blue", high = "red") +
       labs(title = paste("Heatmap of", input$dataCategory, "Registrations by County in Washington State from", input$yearSlider[1], "to", input$yearSlider[2]),
            x = "County",
            y = "",
            fill = if(input$togglePercentage) "Registration Percentage" else "Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
   output$stackedAreaPlot <- renderPlot({
     req(input$yearSlider, input$date_interval, input$dataCategory, input$numCounties)
     
     num_counties <- as.numeric(input$numCounties)
     
     # Filter data for the selected year range and specific counties
     filtered_data <- electric_data %>%
       filter(year(Date) >= input$yearSlider[1], year(Date) <= input$yearSlider[2], State == "WA") %>%
       left_join(county_population, by = "County")
     
     # Use the correct Population column
     filtered_data$Population <- as.numeric(filtered_data$Population.x)
     
     
     # Check if Population column exists and convert it to numeric if needed
     if("Population" %in% names(filtered_data) && is.character(filtered_data$Population)) {
       filtered_data$Population <- as.numeric(gsub(",", "", filtered_data$Population))
     }
     
     # Ensure Population is numeric (catch-all in case the column was already numeric)
     filtered_data$Population <- as.numeric(filtered_data$Population)
     
     # Determine date breaks based on selected interval
     date_breaks <- switch(input$date_interval,
                           "Yearly" = "year",
                           "Quarterly" = "quarter",
                           "Monthly" = "month")
     
     # Group and summarize data based on the selected data category
     summarized_data <- filtered_data %>%
       mutate(DateGroup = floor_date(Date, unit = date_breaks)) %>%
       group_by(DateGroup, County) %>%
       summarize(Selected_Total = sum(!!sym(input$dataCategory), na.rm = TRUE), Population = first(Population)) %>%
       ungroup()
     
     # Calculate percentage of population if toggle is on
     if (input$togglePercentage) {
       summarized_data <- summarized_data %>%
         mutate(Selected_Total = (Selected_Total / Population) * 100)
     }
     
     # Select the top counties based on user selection
     top_counties <- summarized_data %>%
       group_by(County) %>%
       summarize(Total = sum(Selected_Total)) %>%
       arrange(desc(Total)) %>%
       slice_head(n = num_counties) %>%
       pull(County)
     
     # Filter for top counties
     summarized_data <- summarized_data %>%
       filter(County %in% top_counties)
     
     # Create stacked area plot
     ggplot(summarized_data, aes(x = DateGroup, y = Selected_Total, fill = County)) +
       geom_area(alpha = 0.8) +
       scale_fill_manual(values = county_colors) +
       labs(title = paste("Top", input$numCounties, "Counties for", input$dataCategory, "in Washington State from", input$yearSlider[1], "to", input$yearSlider[2]),
            subtitle = if(input$togglePercentage) "Percentage of Population" else "Raw Registrations",
            x = "Date",
            y = if(input$togglePercentage) "Percentage of Population" else paste(input$dataCategory, "Registrations")) +
       theme_minimal() +
       theme(legend.title = element_blank())
   })
   
   output$countyBarGraph <- renderPlot({
     req(input$yearSlider, input$dataCategory)
     
     num_counties <- as.numeric(input$numCounties)
     
     # Prepare data
     plot_data <- electric_data %>%
       filter(State == "WA", year(Date) == input$yearSlider) %>%
       group_by(County) %>%
       summarise(Selected_Total = sum(!!sym(input$dataCategory), na.rm = TRUE)) %>%
       left_join(county_population, by = "County")
     
     # Ensure Population is numeric
     plot_data$Population <- as.numeric(plot_data$Population)
     
     # Handle any NA or conversion errors in Population
     plot_data <- plot_data %>%
       mutate(Population = ifelse(is.na(Population) | !is.finite(Population), 1, Population))
     
     # Calculate percentage of population with registrations
     plot_data <- plot_data %>%
       mutate(Percentage_With_Registrations = (Selected_Total / Population) * 100)
     
     # Select top counties based on user selection
     top_counties <- plot_data %>%
       arrange(if(input$togglePercentage) desc(Percentage_With_Registrations) else desc(Selected_Total)) %>%
       slice_head(n = num_counties)
     
     # Create bar graph
     ggplot(top_counties, aes(x = reorder(County, if(input$togglePercentage) Percentage_With_Registrations else Selected_Total), 
                              y = if(input$togglePercentage) Percentage_With_Registrations else Selected_Total, 
                              fill = County)) +
       geom_bar(stat = "identity") +
       scale_fill_manual(values = county_colors) +
       labs(title = paste("Top ", input$numCounties ," Counties for", input$dataCategory, "Registrations in Washington State from", input$yearSlider[1], "to", input$yearSlider[2]),
            x = "County",
            y = "Total Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
   
   output$countyBoxplot <- renderPlot({
     req(input$yearSlider, input$dataCategory, input$numCounties)
     
     # Ensure numCounties is numeric
     num_counties <- as.numeric(input$numCounties)
     
     # Ensure the county names in both datasets match
     county_population_adjusted <- county_population %>%
       mutate(County = gsub(" County", "", County))
     
     # Filter electric_data for the selected year range in Washington state
     filtered_data <- electric_data %>%
       filter(year(Date) >= input$yearSlider[1], year(Date) <= input$yearSlider[2], State == "WA")
     
     # Join with county population data
     filtered_data <- filtered_data %>%
       left_join(county_population_adjusted, by = "County")
     
     # Ensure the county names in both datasets match
     county_population_adjusted <- county_population %>%
       mutate(County = gsub(" County", "", County))
     
     # Join with county population data
     filtered_data <- filtered_data %>%
       left_join(county_population_adjusted, by = "County")
     
     # Check if Population column is available
     if ("Population" %in% names(filtered_data)) {
       # Calculate the total selected category and population percentage (if required)
       filtered_data <- filtered_data %>%
         group_by(County) %>%
         summarise(Selected_Total = sum(!!sym(input$dataCategory), na.rm = TRUE),
                   Population = first(Population)) %>%
         ungroup()
       
       if (input$togglePercentage) {
         filtered_data <- filtered_data %>%
           mutate(Percentage_With_Registrations = (Selected_Total / Population) * 100)
       }
       
       # Select the top counties based on user selection
       top_counties <- filtered_data %>%
         arrange(desc(if (input$togglePercentage) Percentage_With_Registrations else Selected_Total)) %>%
         head(n = num_counties)
       
       # Create the boxplot
       ggplot(top_counties, aes(x = County, y = if(input$togglePercentage) Percentage_With_Registrations else Selected_Total, fill = County)) +
         geom_boxplot() +
         scale_fill_brewer(palette = "Set3") +
         labs(title = paste("Top ", num_counties ," Counties for", input$dataCategory, "in Washington State"),
              x = "County",
              y = if(input$togglePercentage) "Percentage of Population" else paste(input$dataCategory, "Total")) +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
     } else {
       print("Population data not found in the joined dataset.")
     }
   })
   
   output$timeSeriesPlot <- renderPlot({
     req(input$yearSlider, input$dataCategory)
     
     # Filter data for the selected year range and specific counties in Washington state
     filtered_data <- electric_data %>%
       filter(year(Date) >= input$yearSlider[1], 
              year(Date) <= input$yearSlider[2], 
              State == "WA") %>%
       group_by(Date) %>%
       summarise(Selected_Total = sum(!!sym(input$dataCategory), na.rm = TRUE)) %>%
       ungroup()
     
     # Create the time series plot
     ggplot(filtered_data, aes(x = Date, y = Selected_Total)) +
       geom_line(color = "blue") +
       labs(title = paste("Time Series of", input$dataCategory, "in Washington State"),
            x = "Date",
            y = paste(input$dataCategory, "Total")) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
   
   # EV Time Series Plot
   output$evTimeSeriesPlot <- renderPlot({
     req(input$yearSlider)
     
     ev_data <- electric_data %>%
       filter(year(Date) >= input$yearSlider[1], year(Date) <= input$yearSlider[2], State == "WA") %>%
       group_by(Date) %>%
       summarise(EV_Total = sum(EV_Total, na.rm = TRUE)) %>%
       ungroup()
     
     ggplot(ev_data, aes(x = Date, y = EV_Total)) +
       geom_line(color = "blue") +
       labs(title = "EV Registrations Over Time",
            x = "Date",
            y = "EV Total Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
   # Non-EV Time Series Plot
   output$nonEvTimeSeriesPlot <- renderPlot({
     req(input$yearSlider)
     
     non_ev_data <- electric_data %>%
       filter(year(Date) >= input$yearSlider[1], year(Date) <= input$yearSlider[2], State == "WA") %>%
       group_by(Date) %>%
       summarise(NonEV_Total = sum(NonEV_Total, na.rm = TRUE)) %>%
       ungroup()
     
     ggplot(non_ev_data, aes(x = Date, y = NonEV_Total)) +
       geom_line(color = "red") +
       labs(title = "Non-EV Registrations Over Time",
            x = "Date",
            y = "Non-EV Total Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
   
   output$monthlyRegistrationsHeatmap <- renderPlot({
     req(input$selectedYear, input$selectedVehicleType)
     
     # Filter data based on selected year and vehicle type
     vehicle_column <- ifelse(input$selectedVehicleType == "Total_Vehicles",
                              "Total_Vehicles",
                              input$selectedVehicleType)
     
     filtered_data <- electric_data %>%
       filter(format(Date, "%Y") == input$selectedYear) %>%
       mutate(Month = format(Date, "%m")) %>%
       group_by(Month) %>%
       summarise(Total_Registrations = sum(!!sym(vehicle_column), na.rm = TRUE), .groups = "drop") %>%
       ungroup()
     
     # Convert to wide format for the heatmap
     wide_data <- filtered_data %>%
       mutate(Month = factor(Month, levels = sprintf("%02d", 1:12), labels = month.abb))
     
     # Create the heatmap
     ggplot(wide_data, aes(x = Month, y = "", fill = Total_Registrations)) +
       geom_tile(color = "white") +
       geom_text(aes(label = Total_Registrations), vjust = 1.5, color = "black", size = 3) +
       scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
       labs(title = paste("Monthly", input$selectedVehicleType, "Registrations in", input$selectedYear),
            x = "Month",
            y = "",
            fill = "Total Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
   })
   
   output$allTimeVehicleTypeHeatmap <- renderPlot({
     req(input$selectedVehicleType)
     
     # Select the appropriate column based on the vehicle type
     vehicle_column <- ifelse(input$selectedVehicleType == "Total_Vehicles",
                              "Total_Vehicles",
                              input$selectedVehicleType)
     
     # Calculate monthly totals for the selected vehicle type
     vehicle_type_totals <- electric_data %>%
       mutate(Year = year(Date), Month = format(Date, "%m")) %>%
       group_by(Year, Month) %>%
       summarise(Total = sum(!!sym(vehicle_column), na.rm = TRUE), .groups = "drop") %>%
       ungroup()
     
     # Convert to wide format for the heatmap
     wide_data <- reshape2::dcast(vehicle_type_totals, Year ~ Month, value.var = "Total")
     
     # Ensure 'Month' is a factor with levels representing months
     wide_data <- wide_data %>%
       gather(Month, Total, `01`:`12`, factor_key = TRUE) %>%
       mutate(Month = factor(Month, levels = sprintf("%02d", 1:12), labels = month.abb))
     
     # Create the heatmap using ggplot2
     ggplot(wide_data, aes(x = Month, y = factor(Year), fill = Total)) +
       geom_tile(color = "white") +
       geom_text(aes(label = Total), vjust = 1.5, color = "black", size = 3) +
       scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
       labs(title = paste("Monthly", input$selectedVehicleType, "Registrations by Year"), x = "Month", y = "Year", fill = "Total Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
   })
   
   
   # Server logic to show/hide the plot
   observeEvent(input$toggleButton1, {
     toggle("plotDiv")  # Use shinyjs to toggle the div visibility
   })
   
   
   output$waAnalysisPlot <- renderPlot({
     # Load the processed_country_data.csv file
     processed_country_data <- read.csv("processed_country_data.csv")
     
     # Define Washington state geographical bounds
     wa_lat_bounds <- c(45.5, 49)
     wa_long_bounds <- c(-124.5, -117)
     
     # Function to check if a location is outside Washington state bounds
     is_outside_washington_bounds <- function(lat, long) {
       return(lat < wa_lat_bounds[1] || lat > wa_lat_bounds[2] || long < wa_long_bounds[1] || long > wa_long_bounds[2])
     }
     
     # Apply the function and subset the data
     processed_country_data$Outside_WA_Bounds <- mapply(is_outside_washington_bounds, processed_country_data$lat, processed_country_data$long)
     wa_vehicles_outside_bounds <- subset(processed_country_data, `State...3` != "WA" & Outside_WA_Bounds)
     
     # Aggregating to find the top '50' locations
     top_locations <- wa_vehicles_outside_bounds %>%
       group_by(full_address) %>%
       summarize(Count = n()) %>%
       arrange(desc(Count)) %>%
       head(450)
     
     # Plotting with an adjusted y-axis range
     if (nrow(top_locations) > 0) {
       max_count <- max(top_locations$Count, na.rm = TRUE)
       ggplot(top_locations, aes(x = reorder(full_address, Count), y = Count)) +
         geom_bar(stat = "identity") +
         coord_flip() + # Flipping coordinates for better readability
         scale_y_continuous(limits = c(0, max_count * 1)) + # Extend y-axis to 120% of max count
         labs(title = "Top 15 Locations of Vehicles Registered in WA but Located Outside",
              x = "Location",
              y = "Count of Vehicles")
     } else {
       print("No data available for vehicles registered in WA but located outside state bounds")
     }
   })
   
   
   output$usHeatmap <- renderPlot({
     # Load the processed_country_data.csv file
     processed_country_data <- read.csv("processed_country_data.csv")
     
     # Define Washington state geographical bounds
     wa_lat_bounds <- c(45.5, 49)
     wa_long_bounds <- c(-124.5, -117)
     
     # Function to check if a location is outside Washington state bounds
     is_outside_washington_bounds <- function(lat, long) {
       return(lat < wa_lat_bounds[1] || lat > wa_lat_bounds[2] || long < wa_long_bounds[1] || long > wa_long_bounds[2])
     }
     
     # Apply the function and subset the data
     processed_country_data$Outside_WA_Bounds <- mapply(is_outside_washington_bounds, processed_country_data$lat, processed_country_data$long)
     wa_vehicles_outside_bounds <- subset(processed_country_data, `State...3` != "WA" & Outside_WA_Bounds)
     
     # Function to convert state abbreviations to full names
     abbrev_to_fullname <- function(abbrev) {
       state.name[match(abbrev, state.abb)]
     }
     
     # Aggregating data by state
     state_counts <- wa_vehicles_outside_bounds %>%
       group_by(State...3) %>%
       summarize(Count = n()) %>%
       na.omit() # Remove NA values
     
     # Convert state abbreviations to full names
     state_counts$region <- tolower(sapply(state_counts$State...3, abbrev_to_fullname))
     
     # Getting US map data and fortifying it
     states_map <- map_data("state") %>%
       fortify()
     
     # Creating a lookup table for state abbreviations to full names
     state_names <- data.frame(abb = tolower(state.abb), full = tolower(state.name))
     
     # Merging the state counts with the lookup table to get full state names
     state_counts <- state_counts %>%
       mutate(State...3 = tolower(State...3)) %>%
       left_join(state_names, by = c("State...3" = "abb"))
     
     # Merging map data with state counts
     map_data_merged <- left_join(states_map, state_counts, by = c("region" = "full"))
     
     # Ensure NA values are set to 0 for a consistent color scale
     map_data_merged$Count[is.na(map_data_merged$Count)] <- 0
     
     # Creating the heatmap
     gg <- ggplot(data = map_data_merged, aes(x = long, y = lat, group = group, fill = Count)) +
       geom_polygon(color = "white") +
       scale_fill_gradient(low = "blue", high = "red", na.value = "grey50") +
       coord_fixed(1.3) +
       theme_void() +
       labs(title = "Heatmap of Vehicle Registrations Outside Washington by State (Excluding WA)", fill = "Count")
     
     print(gg)
     
   })
   

   output$totalVehiclesOutsideWA <- renderText({
     # Load the processed_country_data.csv file
     processed_country_data <- read.csv("processed_country_data.csv")
     
     # Define Washington state geographical bounds
     wa_lat_bounds <- c(45.5, 49)
     wa_long_bounds <- c(-124.5, -117)
     
     # Function to check if a location is outside Washington state bounds
     is_outside_washington_bounds <- function(lat, long) {
       return(lat < wa_lat_bounds[1] || lat > wa_lat_bounds[2] || long < wa_long_bounds[1] || long > wa_long_bounds[2])
     }
     
     # Apply the function and subset the data
     processed_country_data$Outside_WA_Bounds <- mapply(is_outside_washington_bounds, processed_country_data$lat, processed_country_data$long)
     wa_vehicles_outside_bounds <- subset(processed_country_data, `State...3` != "WA" & Outside_WA_Bounds)
     
     # Function to convert state abbreviations to full names
     abbrev_to_fullname <- function(abbrev) {
       state.name[match(abbrev, state.abb)]
     }
     
     # Aggregating data by state
     state_counts <- wa_vehicles_outside_bounds %>%
       group_by(State...3) %>%
       summarize(Count = n()) %>%
       na.omit() # Remove NA values
     
     # Convert state abbreviations to full names
     state_counts$region <- tolower(sapply(state_counts$State...3, abbrev_to_fullname))
     
     # Getting US map data and fortifying it
     states_map <- map_data("state") %>%
       fortify()
     
     # Creating a lookup table for state abbreviations to full names
     state_names <- data.frame(abb = tolower(state.abb), full = tolower(state.name))
     
     # Merging the state counts with the lookup table to get full state names
     state_counts <- state_counts %>%
       mutate(State...3 = tolower(State...3)) %>%
       left_join(state_names, by = c("State...3" = "abb"))
     
     # Merging map data with state counts
     map_data_merged <- left_join(states_map, state_counts, by = c("region" = "full"))
     
     
     # Using similar logic as in the usHeatmap plot to filter out WA vehicles
     processed_country_data$Outside_WA_Bounds <- mapply(is_outside_washington_bounds, processed_country_data$lat, processed_country_data$long)
     wa_vehicles_outside_bounds <- subset(processed_country_data, `State...3` != "WA" & Outside_WA_Bounds)
     
     # Now compute the total
     total_vehicles <- nrow(wa_vehicles_outside_bounds)
     paste("Total number of vehicles registered outside of Washington: ", total_vehicles)
   })
   
   # output$animated_plot <- renderPlot({
   #   
   #   req(king_county_data)
   #   
   #   anim_file <- tempfile(fileext = ".gif")
   #   animated_plot <- ggplot(king_county_data, aes(x = Vehicle_Primary_Use, y = EV_Total)) +  
   #     geom_bar(stat = "identity") +
   #     labs(title = "Total Number of Electric Vehicles by Vehicle Primary Use \non {closest_state} in King County", 
   #          x = "Vehicle Primary Use", y = "Total Number of EVs") +
   #     theme_minimal() +
   #     theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) + 
   #     transition_states(Date, transition_length = 2, state_length = 1) +
   #     shadow_mark() +
   #     enter_grow() +
   #     enter_fade()
   #   
   #   anim_file <- tempfile(fileext = ".gif")
   #   anim_save(anim_file, animation = animated_plot)
   #   
   #   # Return an image tag with the path to the gif
   #   tags$img(src = anim_file, alt = "Animated plot", style = "max-width: 100%;")
   # })
   
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)

# summary(electric_data)
