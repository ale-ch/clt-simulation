library(shiny)
library(ggplot2)
library(purrr)

clt <- function(sample_size, n_samples, theta) {
    
    # generate N random bernoulli samples of size n, then take the mean of each sample
    means <- map_dbl(1:n_samples, ~ mean(rbinom(sample_size, 1, theta)))
    
    # collect results in a dataframe to use with ggplot
    df <- data.frame(sample_means = means)
    df
    
}

ui <- fluidPage(
    
    # title
    titlePanel("Simulation of central limit theorem - Bernoulli distribution"),
    fluidRow(
        column(4,
            # slider selection for sample size
            sliderInput(inputId = "samp_size", 
                        label = "Sample Size n", value = 200, min = 100, 
                        max = 1000)
        ),
        column(4, 
            # slider selection for number of samples
            sliderInput(inputId = "n_samples", label = "Samples N", 
                        value = 500, min = 10, max = 1000)
        ),
        column(4, 
               # slider selection for number of samples
               sliderInput(inputId = "theta", label = "Theta", 
                           value = 0.5, min = 0, max = 1)
        )
    ),
    # plot
    fluidRow(
        column(8, plotOutput(outputId = "plot")),
        column(4, checkboxInput("poly", "Frequency polygon", value = FALSE),
               checkboxInput("refline", "Reference line (mean)", value = FALSE),
               verbatimTextOutput(outputId = "summary")
               )
    )
)


server <- function(input, output, session) {
    # update simulation every 1000ms
    timer <- reactiveTimer(1000)
    
    means <- reactive({
        timer()
        clt(input$samp_size, input$n_samples, input$theta)})
    
    output$summary <- renderPrint({
        timer()
        summary(means())
    })
    
    output$plot <- renderPlot({
        timer()
        
        # x intercept for vertical reference line
        x_int <- mean(means()$sample_means)
        
        p <- ggplot(means(), aes(sample_means)) +
            geom_histogram(bins = 25) +
            xlab("Sample averages") +
            ylab("Count") +
            scale_y_continuous() +
            scale_x_continuous(limits = c(0, 1))
            
            
        
        # nested if statements to modify the plot with the frequency polygon and the reference line 
        if(input$poly == TRUE & input$refline == TRUE) {
            p + 
                geom_vline(xintercept = x_int, linetype = "dotted",
                           color = "yellow") +
                geom_freqpoly(color = "red")
        } else {
            if(input$poly == TRUE & input$refline == FALSE) {
                p + geom_freqpoly(color = "red")
            } else {
                if(input$poly == FALSE & input$refline == TRUE) {
                    p + geom_vline(xintercept = x_int, linetype = "dotted",
                                   color = "yellow")
                } else p
            }
        }
        
    })
}


shinyApp(ui = ui, server = server)
