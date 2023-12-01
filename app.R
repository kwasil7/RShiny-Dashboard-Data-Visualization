#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages

library(ggplot2)
library(readr)
library(shiny)
library(lubridate)
library(maps)
library(mapproj)
source("helpers.R")
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

electric_data$EV_Total <- as.numeric(electric_data$EV_Total)

# Filter for King County data, excluding the first row
king_county_data <- subset(electric_data[-1, ], County == "King")

# percent_map(counties$Percent.Electric.Vehicles, "darkgreen", "% EVs")

# Define UI 

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      helpText("Group 15 Dataset"),
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles"),
        selected = "Total_Vehicles"
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("BEVs", "PHEVs", "EV_Total", "NonEV_Total", "Total_Vehicles", "Date"),
        selected = "EV_Total"
      ),
      # Select variables for colors
      selectInput(
        inputId = "x",
        label = "Color by:",
        choices = c("Vehicle_Primary_Use", "BEVs"),
        selected = "Vehicle_Primary_Use"
      )
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "densityplot", height = 200)
    )
  )
)

# Define server 

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = king_county_data, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point()
  })
  
  output$densityplot <- renderPlot({
    ggplot(data = king_county_data, aes_string(x = input$x)) +
      geom_density()
  })
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)
