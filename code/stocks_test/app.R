library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)

finished_data <- read.csv("../../data/finished_data.csv")

metrics <- c("Open", "High", "Low", "Close", "Adj.Close")
stock_names <- finished_data %>% filter(Size == "big") %>% pull(stock_name) %>% unique()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stock Data Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("stock_names",
                      "Stock:",
                      choices = stock_names),
          selectInput("metrics",
                      "Metric:",
                      choices = metrics)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("linePlot")
        )
    )
)

# Define server logic required to draw graphs
server <- function(input, output) {

  output$linePlot <- renderPlotly({
    label <- str_c(input$metrics, "_cat")
    p <- finished_data %>%
      filter(stock_name == input$stock_names) %>% 
      group_by(!!sym(label)) %>% 
      count() %>% 
      mutate({{label}} := as.numeric(!!sym(label))/100) %>% 
      ggplot() +
      geom_line(aes(x = !!sym(label), y = n)) +
      labs(title = "", x = "Cent Price", y = str_c("Recorded ", input$metrics, "s")) +
      theme_minimal()
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
