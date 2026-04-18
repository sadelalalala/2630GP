# Load packages
library(shiny)
library(ggplot2)
library(dplyr)

# Read data
brand_stats <- read.csv("D:/brand_stats.csv")

# Calculate total count by brand for pie chart
brand_total <- brand_stats %>%
  group_by(brand) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))

# UI
ui <- fluidPage(
  titlePanel("Best Buy Over-Ear Headphone Brand Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("brand_select", "Select Brand:",
                  choices = c("All", unique(brand_stats$brand)),
                  selected = "All"),
      br(),
      p("Data Source: Best Buy API"),
      p("Total Headphones: 212")
    ),
    
    mainPanel(
      plotOutput("pie_chart", height = "500px"),
      br(),
      verbatimTextOutput("brand_detail")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$pie_chart <- renderPlot({
    
    if(input$brand_select == "All") {
      # Pie chart for all brands
      plot_data <- brand_total
      plot_title <- "Headphone Distribution by Brand"
      
      ggplot(plot_data, aes(x = "", y = total_count, fill = brand)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = plot_title,
             fill = "Brand") +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        geom_text(aes(label = paste0(brand, "\n", total_count)),
                  position = position_stack(vjust = 0.5), size = 3)
      
    } else {
      # Bar chart for selected brand (showing wireless/wired/unknown)
      plot_data <- brand_stats[brand_stats$brand == input$brand_select, ]
      plot_title <- paste(input$brand_select, "- Connection Type Distribution")
      
      ggplot(plot_data, aes(x = brand, y = count, fill = connection)) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values = c("wireless" = "blue", "wired" = "green", "unknown" = "red")) +
        labs(title = plot_title,
             x = "Brand",
             y = "Number of Headphones",
             fill = "Connection Type") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_text(aes(label = count), vjust = -0.5, size = 5)
    }
  })
  
  output$brand_detail <- renderPrint({
    if(input$brand_select != "All") {
      selected_data <- brand_stats[brand_stats$brand == input$brand_select, ]
      cat(paste0("Brand: ", input$brand_select, "\n\n"))
      for(i in 1:nrow(selected_data)) {
        percentage <- selected_data$count[i] / sum(selected_data$count) * 100
        cat(paste0(selected_data$connection[i], ": ", selected_data$count[i], 
                   " units (", round(percentage, 1), "%)\n"))
      }
      cat(paste0("\nTotal: ", sum(selected_data$count), " units"))
    } else {
      cat("Select a brand from the dropdown menu to view wireless/wired/unknown breakdown")
    }
  })
}

# Run
shinyApp(ui = ui, server = server)