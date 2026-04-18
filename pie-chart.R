library(shiny)
library(ggplot2)
library(dplyr)

df <- read.csv("D:/headphone_comments_800_cleaned.csv")

df$text <- ifelse(is.na(df$comment_clean), df$comment, df$comment_clean)

classify_wireless_vs_wired <- function(text) {
  text_lower <- tolower(text)
  wireless_kw <- c('wireless', 'bluetooth', 'tws', 'airpods', 'noise cancelling')
  wired_kw <- c('wired', 'cable', '3.5mm', 'jack', 'impedance', 'dac', 'amp')
  
  is_wireless <- any(sapply(wireless_kw, function(kw) grepl(kw, text_lower)))
  is_wired <- any(sapply(wired_kw, function(kw) grepl(kw, text_lower)))
  
  if (is_wireless && !is_wired) {
    return('Wireless')
  } else if (is_wired && !is_wireless) {
    return('Wired')
  } else {
    return('Other')
  }
}

df$type <- sapply(df$text, classify_wireless_vs_wired)


discussion_data <- df %>%
  filter(type != 'Other') %>%
  group_by(type) %>%
  summarise(count = n())


ui <- fluidPage(
  titlePanel("Headphone Comments: Wireless vs Wired Discussion Volume"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Comment Count"),
      verbatimTextOutput("count_summary")
    ),
    mainPanel(
      plotOutput("pie_chart", height = "500px")
    )
  )
)


server <- function(input, output) {
  
  output$count_summary <- renderPrint({
    wireless_count <- discussion_data$count[discussion_data$type == "Wireless"]
    wired_count <- discussion_data$count[discussion_data$type == "Wired"]
    cat("Wireless:", wireless_count, "comments\n")
    cat("Wired:", wired_count, "comments\n")
    cat("Total:", sum(discussion_data$count), "comments\n")
    cat("(Other:", sum(df$type == "Other"), "comments excluded)")
  })
  
  output$pie_chart <- renderPlot({
    ggplot(discussion_data, aes(x = "", y = count, fill = type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(count, " (", round(count/sum(count)*100, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 6) +
      scale_fill_manual(values = c("Wireless" = "skyblue", "Wired" = "lightgreen")) +
      labs(title = "Wireless vs Wired Discussion Volume", fill = "Headphone Type") +
      theme_void() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)
