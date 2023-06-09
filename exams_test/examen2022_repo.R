library(ggplot2)

#1---------
PlotDens <- function(mean = 0, sd = 1, a = -1, b = 1) {
  theme_set(
    theme_minimal() +
      theme(legend.position = "top") +
      theme(legend.text = element_text(size = 12))
  )
  data <- data.frame(x = seq(-5, 5, length = 200))
  data$dx <- dnorm(data$x)
  
  p <- ggplot(data, aes(x = x, y = dx)) +
    geom_line(aes(color = "")) +
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), xlim = c(a, b), geom = "area", fill = "red", alpha = 0.3) +
    geom_segment(aes(x = a, y = 0, xend = a, yend = dnorm(a)), linetype = "dashed", show.legend = FALSE) +
    geom_segment(aes(x = b, y = 0, xend = b, yend = dnorm(b)), linetype = "dashed", show.legend = FALSE) +
    annotate("text", x = a, y = 0, label = "a", hjust = 1, vjust = -0.5) +
    annotate("text", x = b, y = 0, label = "b", hjust = 0, vjust = -0.5) +
    labs(y = "density", x = "") +
    scale_color_manual(values = "red", name =  paste("Normal (mean=", mean, "sd=", sd,")\nP(a < X < b)=",round(pnorm(b,mean = mean, sd= sd)- pnorm(a, mean, sd), 3)))
  
  p
}

PlotDens()

#2----------

runPlotDens <- function() {
  require(shiny)
  ui <- fluidPage(
    sliderInput(
      inputId = "range",
      label = "Interval (a,b):",
      min = -5, max = 5, value = 1, step = 0.5
    ),
    plotOutput("plot")
  )
  server <- function(input, output) {
    output$plot <- renderPlot({
      a <- input$range[1]
      b <- input$range[2]
      PlotDens(a = a, b = b)
    })
  }
  runApp(shinyApp(ui, server))
}

runPlotDens()

#3

runPlotDens <- function() {
  ui <- fluidPage(
    sliderInput(
      inputId = "range",
      label = "Interval (a, b):",
      min = -5, max = 5, value = c(-1, 1), step = 0.5
    ),
    sliderInput(
      inputId = "mean",
      label = "Mean:",
      min = -5, max = 5, value = 0, step = 0.5
    ),
    sliderInput(
      inputId = "sd",
      label = "Standard Deviation:",
      min = 0.1, max = 5, value = 1, step = 0.1
    ),
    plotOutput("plot")
  )
  server <- function(input, output) {
    output$plot <- renderPlot({
      a <- input$range[1]
      b <- input$range[2]
      mean_val <- input$mean
      sd_val <- input$sd
      PlotDens(mean = mean_val, sd = sd_val, a = a, b = b)
    })
  }
  runApp(shinyApp(ui, server))
}
