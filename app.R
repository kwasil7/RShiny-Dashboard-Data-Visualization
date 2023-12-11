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
library(readr)
library(shiny)
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
# source("helpers.R")

Sys.setlocale("LC_ALL", "English")

linebreaks <- function(n){HTML(strrep(br(), n))}

# Color-blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load data frame
electric_data <- read_csv("data/Electric_Vehicle_Population_Size_History_By_County.csv", 
                          col_names = c("Date", "County", "State", "Vehicle_Primary_Use", "BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles", "Percent_EV"))

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
king_county_data <- subset(electric_data[-1, ], County == "King")

# Filter for Pierce County data, excluding the first row
pierce_county_data <- subset(electric_data[-1, ], County == "Pierce")

# the map defined below
country_data <- read.csv("processed_country_data.csv")

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

# Define UI
ui <- fluidPage(
  
  tags$h1("Data Visualization Group 15 Project"),
  tags$a("We use this dataset.", href = "https://data.wa.gov/Transportation/Electric-Vehicle-Population-Size-History-By-County/3d5d-sdqb"),
  
  downloadButton("downloadButton", "Download Report"),
  
  theme = shinytheme("superhero"),
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      helpText("Group 15 Dataset"),
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles"),
        selected = "EV_Total"
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles", "Date"),
        selected = "BEVs"
      ),
      # Select variables for colors
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c("Vehicle_Primary_Use", "BEVs", "PHEVs"),
        selected = "Vehicle_Primary_Use"
      ),
      # Alpha for the points
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      # Select which types of vehicle use to plot
      selectInput(
        inputId = "selected_type",
        label = "Select vehicle type:",
        choices = c("Passenger", "Truck"),
        selected = "Passenger"
      ),
      sliderInput('years', 'Years', min = 2017, max = 2024, value = c(2017, 2019))
    ),
    
    # Output:
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "densityplot", height = 200),
      plotOutput(outputId = "barplot"),
      plotOutput(outputId = "lineplot"),
      plotOutput(outputId = "lineplot_cars"),
      plotOutput(outputId = "hist_bevs_king"),
      "The histogram below depicts the number of Plug-In Hybrid Electric Vehicles in the King County.",
      plotOutput(outputId = "hist_phevs_king"),
      plotOutput(outputId = "experimental_plot"),
      linebreaks(2),
      titlePanel("The map of the US"),
      plotOutput("map"),
      titlePanel("Washington State, US"),
      plotOutput("map_wa"),
      plotOutput("map_wa_counties"),
      titlePanel("Washington State Vehicle Registrations Across the US"),
      plotOutput("map_states", height = "600px"),
      linebreaks(2),
      plotOutput("map_states_no_wa", height = "600px")
    )
  )
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
  
  output$barplot <- renderPlot({
    ggplot(data = king_county_data, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
      labs(title = "The Barplot")
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
  
  output$experimental_plot <- renderPlot({
    # Plot data using bars
    ggplot(pierce_county_data, aes(x = pierce_county_data$Date, y = pierce_county_data$EV_Total)) +
      geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # Format date breaks and labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis text
            axis.text.y = element_text(size = 8)) +  # Adjust y-axis text size
      labs(title = "Number of Electric Vehicles Over Time in Pierce County",
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
   
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)

# summary(electric_data)
