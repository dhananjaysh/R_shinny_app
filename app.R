#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
if (!require("rjson")) install.packages("rjson")
library(jsonlite)
library(dplyr)
if (!require("DT")) install.packages("DT")
library(DT)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("countrycode")) install.packages("countrycode")
library(countrycode)
if (!require("plotly")) install.packages("plotly")
library(plotly)
library(maps)
library(rsconnect)

data <-  jsonlite::fromJSON("data_cia2.json")
data <- as.data.frame(data)
data$status <- factor(data$status,levels = c("H", "UM", "LM", "L"))

colnames(data) <- c("Country", "X", "ISO3", "Continent", "Subcontinent", "Status", "Expenditure on education", "Youth unemployment rate", "Net migration rate", "Low youth unemployment rate", "High net migration rate", "Electricity (fossil fuel)", "Area", "Population growth rate", "Life expectancy", "Population")
variable_names <- c("Expenditure on education", "Youth unemployment rate", "Net migration rate", "Population growth rate", "Life expectancy", "Electricity (fossil fuel)")

world_map <- map_data("world")
world_map$ISO3 <- countrycode(sourcevar = world_map$region,
                              origin = "country.name",
                              destination = "iso3c", nomatch = NA)

map_data <- world_map %>% left_join(data, by = c("ISO3" = "ISO3"))


# UI funtion
ui <- fluidPage(
    # App title
    titlePanel("CIA World Factbook 2020"),
    textOutput("greeting"),
    tabsetPanel(
      tabPanel("Univariate analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("variable", "Select a variable: ", variable_names),
                   actionButton("clickViewRawData", "View raw data"),
                   dataTableOutput("rawDataOutput")),
                 mainPanel(
                    tabsetPanel(
                      tabPanel("Map",
                               textOutput("mapheader"),
                               plotlyOutput("mapPlot", width = "850px")),
                      tabPanel("Global analysis",
                               fluidRow(
                                 column(6,
                                    plotlyOutput("histogram", width = "400px")),
                                 column(6, 
                                        plotlyOutput("boxplot", width = "400px")))),
                      tabPanel("Analysis per continent",
                               fluidRow(
                                 column(6,
                                        plotlyOutput("histogram_continent", width = "400px")),
                                 column(6, 
                                        plotlyOutput("boxplot_continent", width = "400px")))))))),
      tabPanel("Multivariate analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("multi_variable_1", "Select variable 1: ", variable_names),
                   selectInput("multi_variable_2", "Select variable 2: ", variable_names, selected = variable_names[2]),
                   selectInput("multi_sized_by", "Scale points by: ", c("Area", "Population"))
                 ),
                 mainPanel(tabPanel("Scatterplot", fluidRow(column(12, plotlyOutput("multi_scatterplot", width = "800px")))))))))


# server funtion
server <- function(input, output) {
  # Reactive expressions for selected variables
  selected_var <- reactive({ input$variable })
  multi_var1 <- reactive({ input$multi_variable_1 })
  multi_var2 <- reactive({ input$multi_variable_2 })
  multi_size <- reactive({ input$multi_sized_by })
    output$greeting <- renderText("Welcome to my shiny app, which allows you to visualize variables from the CIA 2020 factbook on the world map, generate descriptive statistics and statistical graphics.")
    output$mapheader <- renderText("The map contains values of the selected variables. The countries with gray areas have a missing value for the visualized variable.")
    observeEvent(input$clickViewRawData, {
      req(input$variable)
      subset_df <- data[, c("Country", "Continent", selected_var())]
      output$rawDataOutput <- renderDT({subset_df}, options = list(lengthMenu = list(c(10, 15), c('10', '15'))))
    })
    
    output$mapPlot <- renderPlotly({
      req(input$variable)
      
      p <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = get(selected_var()),
                                text = paste("Country:", region, "<br>", selected_var(), ":", get(selected_var())))) +
        geom_polygon(colour = "white", size=0.2) +
        scale_fill_viridis_c(
          option   = "plasma",
          name     = input$variable)

      ggplotly(p, tooltip = "text")
    })
    
    output$boxplot <- renderPlotly({
      req(input$variable)
      
      p <- ggplot(data, aes(y = get(selected_var()))) +
        geom_boxplot() +
        labs(y = input$variable)

      ggplotly(p)
    })
    
    output$histogram <- renderPlotly({
      req(input$variable)
      
      p <- ggplot(data, aes(x = get(selected_var()), y = stat(density))) +
        geom_histogram(bins = 30, fill="lightgrey") +
        geom_density(alpha = 0.5, color = "black", lwd=0.1, fill = "lightblue") + 
        labs(x = selected_var())
      
      ggplotly(p)
    })
    
    output$boxplot_continent <- renderPlotly({
      req(input$variable)
      
      p <- ggplot(data, aes(x = Continent, y = get(selected_var()))) +
        geom_boxplot() +
        labs(y = selected_var())
      
      ggplotly(p)
    })
    
    output$histogram_continent <- renderPlotly({
      req(input$variable)
      
      p <- ggplot(data, aes(x = get(selected_var()), y = stat(density), fill=Continent)) +
        geom_density(alpha = 0.5, color = "black", lwd=0.1) + 
        labs(x = selected_var())
      
      ggplotly(p)
    })
    
    output$multi_scatterplot <- renderPlotly({
      req(input$multi_variable_1)
      req(input$multi_variable_2)
      req(input$multi_sized_by)
      
      p <- ggplot(data, aes(x = get(multi_var1()), y = get(multi_var2()),
                  text = paste("Country:", Country,
                               "<br>",
                               multi_var1(), ":", get(multi_var1()),
                               "<br>",
                               multi_var2(), ":", get(multi_var2()),
                               "<br>",
                               multi_size(), ":", get(multi_size())
                               )
                  )) +
        geom_point(aes(size = get(multi_size()), colour = factor(Continent))) +
        geom_smooth(aes(group = Continent, colour = factor(Continent)), method = "loess", se = FALSE, size = 0.5) +
        labs(x = multi_var1(), y = multi_var2()) +
        ggtitle("Scatterplot") +
        guides(size=guide_legend(title=tolower(multi_size())), col=guide_legend(title="continent"))
      
      ggplotly(p, tooltip = "text")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
