library(shiny)

shinyUI(fluidPage(
    titlePanel("Distribution of Sample Means with (a) various sample size & (b) number of repetition"), 
    br(),
    sidebarLayout(
        mainPanel(
            plotOutput("plot")
        ),
        sidebarPanel(
            radioButtons("dist", "Population distribution:",
                         c("Normal" = "norm",
                           "Uniform" = "unif",
                           "Log-normal" = "lnorm",
                           "Exponential" = "exp")),
            br(),     
            sliderInput("n", 
                        "Sample size:", 
                        value = 50,
                        min = 1, 
                        max = 1000),
            br(),br(),
            sliderInput("reps", 
                        "Number of repetition:", 
                        value = 200,
                        min = 1, 
                        max = 1000),
            br(),
            checkboxInput("checkbox", label = "Add sample one at a time", value = FALSE),
            
            actionButton("resample", label = "Draw a new sample"),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
        )
    )
))

