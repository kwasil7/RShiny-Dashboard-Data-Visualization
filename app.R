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
library(dplyr)
library(lubridate)
library(maps)
library(mapproj)
# source("helpers.R")

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

# percent_map(counties$Percent.Electric.Vehicles, "darkgreen", "% EVs")

# Define UI

ui <- fluidPage(
  
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
      )
    ),
    
    # Output:
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "densityplot", height = 200),
      plotOutput(outputId = "barplot"),
      plotOutput(outputId = "lineplot")
    )
  )
)

# Define server 

server <- function(input, output, session) {
  
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
           y = "Total Electric Truck Registrations")
  })
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)

# summary(electric_data)
