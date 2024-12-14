
# Define UI
ui2 <- fluidPage(
  
  # Application title
  titlePanel("Daily Highs"),
  
  # Sidebar with volume range selection
  sidebarLayout(
    sidebarPanel(
      selectInput("volume_range",
                  "Select Volume Range:",
                  choices = unique(modified_data$volume_range),
                  selected = unique(modified_data$volume_range)[1]),
      selectInput("metrics",
                  "Metric:",
                  choices = metrics),
      dateRangeInput("date_range",
                     "Date Range:",
                     start = min(finished_data$Date),
                     end = max(finished_data$Date),
                     format = "yyyy-mm-dd",
                     min = min(finished_data$Date),
                     max = max(finished_data$Date),
                     separator = " to ")
    ),
    
    # Main panel to display the plot
    mainPanel(
      plotlyOutput("barPlot")
    )
  )
)

# Define server logic
server2 <- function(input, output) {
  
  output$barPlot <- renderPlotly({
    
    # Filter the data based on selected volume range
    filtered_data <- modified_data %>%
      filter(Date >= as.Date(input$date_range[1]) & Date <= as.Date(input$date_range[2]) &
               volume_range == input$volume_range)
    
    # Create the label dynamically based on the chosen metric
    label <- str_c(input$metrics, "_cat")
    
    # Generate the plot data
    plot_data <- filtered_data %>%
      group_by(!!sym(label)) %>%
      count() %>%
      mutate(cent_price = as.numeric(!!sym(label)) / 100) %>% # Convert to cent price
      
      # Create the ggplot
      plot <- ggplot(plot_data) +
      geom_bar(aes(x = cent_price, y = n), stat = "identity") +
      labs(
        title = paste("Median Trading Volume:", input$volume_range),
        subtitle = str_c("Recorded ", input$metrics, "s"),
        x = "Cent Price",
        y = "Number of Trades"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)  # Ensure y-axis is formatted with commas
    
    # Convert ggplot to plotly for interactive hover functionality
    ggplotly(plot) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12)
        )
      ) %>%
      add_trace(
        hoverinfo = "text",
        text = ~paste("Cent Price:", cent_price, "<br>Number of Trades:", n)
      )
  })
}

# Run the application
shinyApp(ui = ui2,server = server2)
