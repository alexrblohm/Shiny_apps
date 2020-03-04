library(shiny)
library(BSDA)
library(tidyverse)

ui <- fluidPage(
   
   # Application title
   titlePanel("1 Sample Power Analysis"),
   tabsetPanel(
     tabPanel("Normal", fluid = TRUE,
       sidebarLayout(
          sidebarPanel(
            sliderInput("Std",
                        "Population Standard Deviation",
                        min = 0.1, max = 10, value = 2, step = 0.1), 
            sliderInput("TRUE_DIFF",
                         "True Difference",
                         min = 0, max = 9, value = 1, step = 0.5),
             sliderInput("Sample_Size",
                         "Sample Size",
                         min = 3, max = 50, value = 5, step = 1),
             sliderInput("Type_1_Error",
                         "Type I Error (Alpha)",
                         min = 0.01, max = 0.10, value = 0.05),
            checkboxInput("Type_I", "Type I Error", value = FALSE),
            checkboxInput("Type_II", "Type II Error", value = FALSE),
            checkboxInput("Power", "Power", value = FALSE)
          ),
          mainPanel(
             htmlOutput("T_Power"),
             plotOutput("Population", height = 200),
             plotOutput("t_plot", height = 200)
          )
       )
      ),
     tabPanel("Uniform", fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            sliderInput("Max",
                        "Maximum",
                        min = 4, max = 10, value = 5, step = 0.1),
            sliderInput("U.Sample_Size",
                        "Sample Size",
                        min = 3, max = 50, value = 5, step = 1),
            sliderInput("U.Type_1_Error",
                        "Type I Error (Alpha)",
                        min = 0.01, max = 0.10, value = 0.05),
            checkboxInput("U.Type_I", "Type I Error", value = FALSE),
            checkboxInput("U.Type_II", "Type II Error", value = FALSE),
            checkboxInput("U.Power", "Power", value = FALSE)
            
          ),
            mainPanel(
              htmlOutput("U.T_Power"),
              plotOutput("U.Population", height = 200),
              plotOutput("U.t_plot", height = 200)
            )
        )
     )
  )
)

# Define server 
server <- function(input, output) {
  output$T_Power <- renderUI({
      Theoretical_Power <- power.t.test(n = input$Sample_Size, 
                                        delta = input$TRUE_DIFF, 
                                        sd = input$Std, 
                                        sig.level = input$Type_1_Error,
                                        power = NULL,
                                        type = c("one.sample"),
                                        alternative = c("one.sided")
                            )
    
      N.TTest <- function(){
        Sample <- rnorm(input$Sample_Size, mean = 0 + input$TRUE_DIFF, sd = input$Std)
        t_test <- t.test(Sample, alternative = "greater", mu = 0, conf.level = 1-input$Type_1_Error)
        Reject <- t_test$p.value < input$Type_1_Error
        return(Reject)
      }
    
      N.Sign_test <- function(){
        Sample <- rnorm(input$Sample_Size, mean = 0 + input$TRUE_DIFF, sd = input$Std)
        Sign_T <- SIGN.test(Sample, md = 0, alternative = "greater", conf.level = 1-input$Type_1_Error)
        Reject_Sign <- Sign_T$p.value < input$Type_1_Error
        return(Reject_Sign)
      }
    
      N.Wilcox_Test <- function(){
        Sample <- rnorm(input$Sample_Size, mean = 0 + input$TRUE_DIFF, sd = input$Std)
        WCX_T <- wilcox.test(Sample, md = 0, alternative = "greater", conf.level = 1-input$Type_1_Error)
        Reject_Sign <- WCX_T$p.value < input$Type_1_Error
        return(Reject_Sign)
      }
    
      set.seed(1)    
      Power_t <- sum(replicate(N.TTest(), n = 1000))/1000
      set.seed(1)
      Power_sign <- sum(replicate(N.Sign_test(), n = 1000))/1000
      set.seed(1)
      Power_WCX <- sum(replicate(N.Wilcox_Test(), n = 1000))/1000
    
      HTML(paste("The Theoretical Power of a t test is: ", Theoretical_Power$power,
               "The simulated Power from a t-test is: ", Power_t,
               "The simulated Power from a sign test is: ", Power_sign,
               "The simulated Power from a Wilcoxon Signed-Rank test is: ", Power_WCX,
                sep = '<br/>'))
  })
  

  output$Population <- renderPlot({
      Pop <- function(x) {dnorm(x, mean = 0 + input$TRUE_DIFF, sd = input$Std)}
      ggplot(data.frame(x = c(-4, 12)), aes(x = x)) +
      stat_function(fun = Pop) +
      ggtitle("Population Distribution")
    })
  
  output$t_plot <- renderPlot({
    standard <- function(x) {dnorm(x, mean = 0, sd = input$Std / sqrt(input$Sample_Size))}
    shifted <- function(x) {dnorm(x, mean = input$TRUE_DIFF, sd = input$Std / sqrt(input$Sample_Size))}
    p <- ggplot(data.frame(x = c(-4, 12)), aes(x = x)) +
      stat_function(fun = shifted, col = "red") + 
      stat_function(fun = standard) +
      ggtitle("Sampling Distribution")
      if(input$Type_I == T) {
        p <- p + stat_function(fun = dnorm, args = list(mean = 0, sd = input$Std / sqrt(input$Sample_Size)), 
                    xlim = c(qnorm(1 - input$Type_1_Error, mean = 0, sd = input$Std / sqrt(input$Sample_Size)), 12), 
                    geom = "area",
                    alpha = .5)
      }
      if(input$Power == T) {
          p <- p + stat_function(fun = shifted, 
                                 xlim = c(qnorm(1 - input$Type_1_Error, mean = 0, sd = input$Std / sqrt(input$Sample_Size)), 12), 
                                 geom = "area",
                                 col = "red",
                                 fill = "red",
                                 alpha = .5)
      }
    if(input$Type_II == T) {
      p <- p + stat_function(fun = shifted, 
                             xlim = c(-4, qnorm(1 - input$Type_1_Error, mean = 0, sd = input$Std / sqrt(input$Sample_Size))), 
                             geom = "area",
                             col = "green",
                             fill = "green",
                             alpha = .5)
    }
    p
  })
  #####################################################################################
  #Uniform
  output$U.T_Power <- renderUI({
    Theoretical_Power <- power.t.test(n = input$U.Sample_Size, 
                                      delta = (-4 + input$Max)/2, 
                                      sd = (input$Max - (-4))/sqrt(12), 
                                      sig.level = input$U.Type_1_Error,
                                      power = NULL,
                                      type = c("one.sample"),
                                      alternative = c("one.sided")
    )
    
    N.TTest <- function(){
      Sample <- runif(input$U.Sample_Size, min = -4, max = input$Max)
      t_test <- t.test(Sample, alternative = "greater", mu = 0, conf.level = 1-input$U.Type_1_Error)
      Reject <- t_test$p.value < input$U.Type_1_Error
      return(Reject)
    }
    
    N.Sign_test <- function(){
      Sample <- runif(input$U.Sample_Size, min = -4, max = input$Max)
      Sign_T <- SIGN.test(Sample, md = 0, alternative = "greater", conf.level = 1-input$U.Type_1_Error)
      Reject_Sign <- Sign_T$p.value < input$U.Type_1_Error
      return(Reject_Sign)
    }
    
    N.Wilcox_Test <- function(){
      Sample <- runif(input$U.Sample_Size, min = -4, max = input$Max)
      WCX_T <- wilcox.test(Sample, md = 0, alternative = "greater", conf.level = 1-input$U.Type_1_Error)
      Reject_Sign <- WCX_T$p.value < input$U.Type_1_Error
      return(Reject_Sign)
    }
    
    set.seed(1)    
    Power_t <- sum(replicate(N.TTest(), n = 1000))/1000
    set.seed(1)
    Power_sign <- sum(replicate(N.Sign_test(), n = 1000))/1000
    set.seed(1)
    Power_WCX <- sum(replicate(N.Wilcox_Test(), n = 1000))/1000
     
    HTML(paste("The Theoretical Power of a t test is: ", Theoretical_Power$power,
    "The simulated Power from a t-test is: ", Power_t,
    "The simulated Power from a sign test is: ", Power_sign,
    "The simulated Power from a Wilcoxon Signed-Rank test is: ", Power_WCX,
                  sep = '<br/>'))
  })

  output$U.Population <- renderPlot({
    Pop <- function(x) {dunif(x, min = -4, max = input$Max)}
    ggplot(data.frame(x = c(-5, 12)), aes(x = x)) +
      stat_function(fun = Pop) +
      ggtitle("Population Distribution")
  })
  
  output$U.t_plot <- renderPlot({
    standard <- function(x) {dnorm(x, mean = 0, sd = ((input$Max - (-4))/sqrt(12)) / sqrt(input$U.Sample_Size))}
    shifted <- function(x) {dnorm(x, mean = (-4 + input$Max)/2, sd = ((input$Max - (-4))/sqrt(12)) / sqrt(input$U.Sample_Size))}
    p <- ggplot(data.frame(x = c(-4, 12)), aes(x = x)) +
      stat_function(fun = shifted, col = "red") +
      stat_function(fun = standard) + 
      ggtitle("Sampling Distribution")
    if(input$U.Type_I == T) {
      p <- p + stat_function(fun = dnorm, args = list(mean = 0, sd = ((input$Max - (-4))/sqrt(12)) / sqrt(input$U.Sample_Size)),
                             xlim = c(qnorm(1 - input$U.Type_1_Error, mean = 0, sd = ((input$Max - (-4))/sqrt(12)) / sqrt(input$U.Sample_Size)), 12),
                             geom = "area",
                             alpha = .5)
    }
    if(input$U.Power == T) {
      p <- p + stat_function(fun = shifted,
                             xlim = c(qnorm(1 - input$U.Type_1_Error, mean = 0, sd = ((input$Max - (-4))/sqrt(12)) / sqrt(input$U.Sample_Size)), 12),
                             geom = "area",
                             col = "red",
                             fill = "red",
                             alpha = .5)
    }
    if(input$U.Type_II == T) {
      p <- p + stat_function(fun = shifted,
                             xlim = c(-4, qnorm(1 - input$U.Type_1_Error, mean = 0, sd = ((input$Max - (-4))/sqrt(12)) / sqrt(input$U.Sample_Size))),
                             geom = "area",
                             col = "green",
                             fill = "green",
                             alpha = .5)
    }
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

