library(shiny)
library(ggplot2)
library(quantmod)
library(zoo)

# 1. UI
ui <- fluidPage(
  titlePanel("Stock Price with 20-period Moving Average"),
  sidebarLayout(
    sidebarPanel(
      textInput("stock_symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput("date_range", "Date Range:",
                     start = "2023-01-01", end = "2023-12-31")
    ),
    mainPanel(
      plotOutput("stock_chart")
    )
  )
)

# 2. Server
server <- function(input, output) {
  stock_data <- reactive({
    req(input$stock_symbol)
    data <- getSymbols(input$stock_symbol, src = "yahoo", 
                       from = input$date_range[1], 
                       to = input$date_range[2], 
                       auto.assign = FALSE)
    return(data)
  })
  
  output$stock_chart <- renderPlot({
    data <- stock_data()
    # استخراج عمود الإغلاق وضمان تحويله بشكل صحيح لتجنب خطأ object/output
    close_col <- Cl(data)
    df <- data.frame(Date = index(data), Price = as.numeric(coredata(close_col)))
    
    # حساب المتوسط المتحرك لـ 20 فترة
    df$SMA20 <- rollmean(df$Price, 20, fill = NA, align = "right")
    
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Price, color = "Price"), na.rm = TRUE) +
      geom_line(aes(y = SMA20, color = "SMA 20"), na.rm = TRUE) +
      labs(title = paste("Stock Price with 20-period Moving Average -", input$stock_symbol),
           y = "Price", x = "Date") +
      scale_color_manual(values = c("Price" = "blue", "SMA 20" = "red"))
  })
}

# 3. Run Application
shinyApp(ui = ui, server = server)