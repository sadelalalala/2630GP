library(shiny)
library(ggplot2)
library(dplyr)

lr_coef <- read.csv("D:/Users/常芮宁/Downloads/lr_coefficients.csv")

lr_coef <- lr_coef %>%
  filter(!word %in% c("ve", "just", "example", "like", "would", "get")) %>%
  mutate(word = ifelse(word == "5mm", "3.5mm", word))

wireless_keywords <- lr_coef %>%
  filter(coefficient > 0) %>%
  arrange(desc(coefficient)) %>%
  head(10)

wired_keywords <- lr_coef %>%
  filter(coefficient < 0) %>%
  mutate(coefficient = abs(coefficient)) %>%
  arrange(desc(coefficient)) %>%
  head(10)

ui <- fluidPage(
  titlePanel("Logistic Regression: Wireless vs Wired Headphones"),
  sidebarLayout(
    sidebarPanel(
      h4("Logistic Regression Results"),
      p("Accuracy: 83.87%"),
      hr(),
      p("Wireless: Higher coefficient → more likely to be wireless"),
      p("Wired: Higher coefficient → more likely to be wired"),
      p("Note: Coefficients shown as absolute values for visualization")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("wireless_plot", height = "500px")),
        column(6, plotOutput("wired_plot", height = "500px"))
      )
    )
  )
)

server <- function(input, output) {
  output$wireless_plot <- renderPlot({
    wireless_keywords %>%
      arrange(coefficient) %>%
      mutate(word = factor(word, levels = word)) %>%
      ggplot(aes(x = coefficient, y = word)) +
      geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
      geom_text(aes(label = sprintf("%.2f", coefficient)), hjust = -0.2, size = 4) +
      xlim(0, max(wireless_keywords$coefficient) * 1.1) +
      labs(title = "Wireless Headphones", x = "Coefficient", y = "") +
      theme_minimal()
  })
  
  output$wired_plot <- renderPlot({
    wired_keywords %>%
      arrange(coefficient) %>%
      mutate(word = factor(word, levels = word)) %>%
      ggplot(aes(x = coefficient, y = word)) +
      geom_bar(stat = "identity", fill = "lightgreen", width = 0.7) +
      geom_text(aes(label = sprintf("%.2f", coefficient)), hjust = -0.2, size = 4) +
      xlim(0, max(wired_keywords$coefficient) * 1.1) +
      labs(title = "Wired Headphones", x = "Coefficient", y = "") +
      theme_minimal()
  })
}

shinyApp(ui, server)
