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
                            "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
                          c("King", "Pierce", "Clark", "Whatcom", 
                            "Snohomish", "Spokane", "Thurston", "Yakima", 
                            "Island", "Benton"))


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
            plotOutput(outputId = "total_plot")
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
                           selected = "EV_Total")
               #, checkboxInput("togglePercentage", "Show as Percentage of Population", value = FALSE)
             ),
             mainPanel(
               plotOutput("countyHeatmap"),
               plotOutput("stackedAreaPlot"),
               plotOutput("countyBarGraph")
             )
           ))
  
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
     req(input$yearSlider, input$dataCategory)
     
     # Filter data for the selected year and for Washington state
     wa_county_data <- electric_data %>%
       filter(year(Date) == input$yearSlider, State == "WA") %>%
       group_by(County) %>%
       summarise(EV_Total_County = sum(!!sym(input$dataCategory), na.rm = TRUE)) %>%
       ungroup()
     
     # Plot of heatmap for counties in Washington
     ggplot(wa_county_data, aes(x = County, y = "", fill = EV_Total_County)) +
       geom_tile() +
       scale_fill_gradient(low = "blue", high = "red") +
       labs(title = paste("Heatmap of", input$dataCategory, "Registrations by County in Washington State from", input$yearSlider[1], "to", input$yearSlider[2]),
            x = "County",
            y = "",
            fill = "Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
   
   output$stackedAreaPlot <- renderPlot({
     
     req(input$yearSlider, input$date_interval, input$dataCategory)
     
     # Filter data for the selected year range and specific counties
     filtered_data <- electric_data %>%
       filter(year(Date) >= input$yearSlider[1], 
              year(Date) <= input$yearSlider[2], 
              County %in% c("King", "Pierce", "Clark", "Whatcom", "Snohomish", "Thurston", "Yakima"), 
              State == "WA")
     
     # Determine date breaks based on selected interval
     date_breaks <- switch(input$date_interval,
                           "Yearly" = "year",
                           "Quarterly" = "quarter",
                           "Monthly" = "month")
     
     # Group and summarize data based on the selected data category
     summarized_data <- filtered_data %>%
       mutate(DateGroup = floor_date(Date, unit = date_breaks)) %>%
       group_by(DateGroup, County) %>%
       summarize(Selected_Total = sum(!!sym(input$dataCategory), na.rm = TRUE), .groups = "drop")
     
     # Create stacked area plot
     ggplot(summarized_data, aes(x = DateGroup, y = Selected_Total, fill = County)) +
       geom_area(alpha = 0.8) +
       scale_fill_manual(values = county_colors) +
       labs(title = paste(input$dataCategory, "in Selected Counties"),
            subtitle = paste("From", input$yearSlider[1], "to", input$yearSlider[2]),
            x = "Date",
            y = paste(input$dataCategory, "Registrations")) +
       theme_minimal() +
       theme(legend.title = element_blank())
   })
   
   
   
   output$countyBarGraph <- renderPlot({
     req(input$yearSlider, input$dataCategory)
     
     # Filter and summarize data
     wa_county_data <- electric_data %>%
       filter(year(Date) == input$yearSlider, State == "WA") %>%
       group_by(County) %>%
       summarise(Selected_Total = sum(!!sym(input$dataCategory), na.rm = TRUE)) %>%
       ungroup() %>%
       arrange(desc(Selected_Total)) %>%
       top_n(10, Selected_Total)  # Select the top 10 counties
     
     # Set a color palette (excluding white)
     color_palette <- brewer.pal(min(10, n_distinct(wa_county_data$County)), "Set3")
     
     # Create the bar graph
     ggplot(wa_county_data, aes(x = reorder(County, Selected_Total), y = Selected_Total, fill = County)) +
       geom_bar(stat = "identity") +
       scale_fill_manual(values = county_colors) +
       labs(title = paste("Top 10 Counties for", input$dataCategory, "Registrations in Washington State from", input$yearSlider[1], "to", input$yearSlider[2]),
            x = "County",
            y = "Total Registrations") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)

# summary(electric_data)
