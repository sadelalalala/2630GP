library(shiny)
library(ggplot2)
library(dplyr)
df <- read.csv("/Users/adela/Downloads/final_cleaned.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Wireless vs Wired Headphones Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Statistics"),
      p(paste("Total comments:", nrow(df))),
      p(paste("Wireless mentions:", sum(df$mentions_wireless))),
      p(paste("Wired mentions:", sum(df$mentions_wired)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Pie Chart", plotOutput("piePlot")),
        tabPanel("Comparison Plot", plotOutput("comparePlot")),
        tabPanel("Data", tableOutput("table"))  # 保持你原来的样式
      )
    )
  )
)

server <- function(input, output) {

  output$piePlot <- renderPlot({
    df %>%
      count(sentiment) %>%
      ggplot(aes(x = "", y = n, fill = sentiment)) +
      geom_col(color = "white") +
      coord_polar("y") +
      theme_void() +
      labs(title = "Overall Sentiment Distribution", fill = "Sentiment")
  })

  output$comparePlot <- renderPlot({
    df %>%
      filter(mentions_wireless == 1 | mentions_wired == 1) %>%
      mutate(type = case_when(
        mentions_wireless == 1 ~ "Wireless",
        mentions_wired == 1 ~ "Wired"
      )) %>%
      count(type, sentiment) %>%
      ggplot(aes(x = type, y = n, fill = sentiment)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = c("positive"="green", "negative"="red", "neutral"="gray")) +
      labs(title = "Wireless vs Wired Sentiment Comparison", y = "Proportion")
  })

  output$table <- renderTable({
    df[, c("cleaned_text", "sentiment", "mentions_wireless", "mentions_wired")]
  })
}

shinyApp(ui = ui, server = server)
