library(shiny)
library(ggplot2)

clt <- function(sample_size, n_samples) {
    
    # N samples of size n 
    samples <- list()
    
    # mean of each sample
    means <- vector("double")
    
    # repeat for desired sample size
    for(i in 1:n_samples) {
        samples[[i]] <- rbinom(sample_size, 1, 0.5)
        means[i] <- mean(samples[[i]])
    }
    
    # collect the results in a dataframe for ggplot
    df <- data.frame(
        sample_means = means
    )
    
    df
    
}

ui <- fluidPage(
    
    # Title
    titlePanel("Simulation of central limit theorem - Bernoulli distribution"),
    fluidRow(
        column(5,
            # slider selection for sample size
            sliderInput(inputId = "samp_size", 
                        label = "Sample Size n", value = 200, min = 100, 
                        max = 1000)
        ),
        column(5, 
            # slider selection for number of samples
            sliderInput(inputId = "n_samples", label = "Samples N", 
                        value = 500, min = 10, max = 1000)
        ),
        column(2,
            # checkbox selection for additional references in the plot
            checkboxInput("poly", "Frequency polygon", value = FALSE),
            checkboxInput("refline", "Reference line (mean)", value = FALSE)
        )
    ),

    fluidRow(
        # plot
        column(8, plotOutput(outputId = "plot")),
        # summary table
        column(2, verbatimTextOutput(outputId = "summary"))
    )
)


server <- function(input, output, session) {
    # the simulation is updated every second 
    timer <- reactiveTimer(1000)
    
    means <- reactive({
        timer()
        clt(input$samp_size, input$n_samples)})
    
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
            scale_x_continuous(limits = c(0.28, 0.72))
            
            
        
        # nested if statements to modify the plot with the frequency polygon  
        # and the reference line 
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
