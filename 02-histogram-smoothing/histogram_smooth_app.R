library(tidyverse)
library(shiny)
library(magrittr)

ui <- basicPage(
  title = "Сглаживание гистограммы",
  div(
    style = "display: flex; gap: 15px; align-items: flex-start;",
    sliderInput(
      inputId = "smooth_window",
      label = "Ширина окна сглаживания",
      min = 0.1,
      max = 10,
      value = 2
    ),
    numericInput(
      inputId = "n_bins",
      label = "Число столбцов",
      min = 5,
      value = 20
    ),
    selectInput(
      inputId = "kernel",
      label = "Ядро",
      choices = list(
        "Гауссово" = "gaussian",
        "Епанечниково" = "epanechnikov",
        "Равномерное" = "rectangular",
        "Треугольное" = "triangular",
        "Биквадратное" = "biweight",
        "Косинусоидальное (S)" = "cosine",
        "Косинусоидальное" = "optcosine"
      ),
      selected = "gaussian"
    ),
    actionButton(
      inputId = "generate_data",
      label = "Сгенерировать данные",
      style = "margin-top: 25px"
    ),
  ),
  plotOutput(
    outputId = "plot",
    height = 600
  )
)

server <- function(input, output, session) {
  data <- reactive({
    input$generate_data
    
    params <- tibble(
      mean = runif(5, min = 5, max = 20),
      sd = runif(5, min = 0.1, max = 3)
    )
    
    data <-  params %>%
      rowwise() %>%
      mutate(x = list(rnorm(100, mean, sd))) %$%
      unlist(x)
  })
  
  histogram <- reactive({
    n_bins <- req(input$n_bins)
    req(data()) %>%
      hist(breaks = n_bins, plot = FALSE) %$%
      tibble(counts, density, mids)
  })
  
  smooth_line <- reactive({
    histogram <- req(histogram())
    
    d <- density(
      x = histogram$mids,
      kernel = input$kernel,
      bw = input$smooth_window,
      weights = histogram$density / sum(histogram$density)
    ) 
    tibble(x = d$x, y = d$y)
  })
  
  output$plot <- renderPlot({
    histogram <- req(histogram())
    smooth_line <- req(smooth_line())
    
    ggplot(histogram, aes(x = mids, y = density)) +
      geom_col() +
      geom_line(data = smooth_line, mapping = aes(x = x, y = y))
  })
}

app <- shinyApp(ui, server)

runApp(app)