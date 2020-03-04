library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Linear Correlation"),
  tabsetPanel(
    tabPanel("Linear", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("noise", "Amount of Noise", min = 0, max = 400, value = 0)
               ),
               mainPanel(
                 htmlOutput("correlation"), 
                 plotOutput("scatterplot")
               )
             )
    ),
    tabPanel("Quad", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("q_noise", "Amount of Noise", min = 0, max = 400, value = 0)
               ),
               mainPanel(
                 htmlOutput("q_correlation"), 
                 plotOutput("q_scatterplot")
               )
             )
    )
  )
)

server <- function(input, output) {
  output$correlation <- renderUI({
    set.seed(1)
    data <- data.frame(x = c(-10:10))
    data$y <- map_dbl(data$x, ~.x + rnorm(1, sd = input$noise))
    cor <- cor(data$x, data$y)
    HTML(paste("The correlation coefficient r is: ", cor))
  })
  output$scatterplot <- renderPlot({
    set.seed(1)
    data <- data.frame(x = c(-10:10))
    data$y <- map_dbl(data$x, ~.x + rnorm(1, sd = input$noise))
    ggplot(data = data, aes(x=x, y=y)) + geom_point() + geom_smooth(method = "lm", se = F)
  })
  
  output$q_correlation <- renderUI({
    set.seed(1)
    data <- data.frame(x = c(-10:10))
    data$quadratic <- map_dbl(data$x, ~.x^2 + rnorm(1, sd = input$q_noise))
    cor <- cor(data$x, data$quadratic)
    HTML(paste("The correlation coefficient r is: ", cor))
  })
  output$q_scatterplot <- renderPlot({
    set.seed(1)
    data <- data.frame(x = c(-10:10))
    data$quadratic <- map_dbl(data$x, ~.x^2 + rnorm(1, sd = input$q_noise))
    ggplot(data = data, aes(x=x, y=quadratic)) + geom_point() + geom_smooth(method = "lm", se = F)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
