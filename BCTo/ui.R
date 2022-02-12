library(shiny)
shinyUI(fluidPage(
    
    titlePanel("Distribuição BCTo"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("m",
                        label = HTML("&mu;:"),
                        min = 0,
                        max = 5000,
                        value = 3500,
                        step=10),
            sliderInput("s",
                        label = HTML("&sigma;:"),
                        min = 0.00000001,
                        max = 20,
                        value = 3,
                        step=0.01),
            sliderInput("n",
                        label = HTML("&nu;:"),
                        min = -20,
                        max = 20,
                        value = -1,
                        step=0.01),
            sliderInput("t",
                        label = HTML("&tau;:"),
                        min = 0.00000001,
                        max = 10,
                        value = 2,
                        step=0.01)
            
        ),
        
        mainPanel(
            plotOutput("ProbPlot")
        )
    )
))