library(shiny)
library(ggplot2)
library(scales)
library(gamlss)
library(ggrepel)

shinyServer(function(input, output) {
    
    output$ProbPlot <- renderPlot({
        
        m = input$m
        s = input$s
        n = input$n
        t = input$t
        x <- 0:10000
        
        
        muCalculation <- function(x, lambda) {dBCTo(x, mu=m, sigma=s, nu=n, tau=t )}
        probability_at_lambda <- sapply(input$x, muCalculation, seq(0,10000, 0.01))
        
        
        lab1=paste("Mediana (\u00B5)=", formatC(m, digits=2, format='f'))
        lab2=paste("Variabilidade (\u03C3):", formatC(s, digits=2, format='f'))
        lab3=paste("Assimetria (\u03BD):", formatC(n, digits=2, format='f'))
        lab4=paste("Curtose (\u03C4):", formatC(t, digits=2, format='f'))

        ggplot(data.frame(x = c(0, 10000)), aes(x)) +
            stat_function(fun = function(x)dBCTo(x, mu = m, sigma = s, nu = n, tau = t),
                          color="red") +
            stat_function(fun = function(x)dBCTo(x, mu = m, sigma = s, nu = n, tau = t), 
                          xlim=c(0,10000),
                          geom = "area",
                          fill = 'gray', 
                          alpha = 0.6)+
            ylab("Densidade")+
            xlab("Valor mediano") +
            labs(title=("Função densidade de probabilidade dos valores (BCT)"),
                 subtitle =("(sob os parâmetros fixados)"))+
            annotate("text", x= 10, y=0.001, label="Parâmetros estimados:", size=3, hjust=0)+
            annotate("text", x= 10, y=0.00095, label=lab1, parse=FALSE, size=3, hjust=0)+
            annotate("text", x= 10, y=0.0009, label=lab2, parse=FALSE, size=3, hjust=0)+
            annotate("text", x= 10, y=0.00085, label=lab3, parse=FALSE, size=3, hjust=0)+
            annotate("text", x= 10, y=0.0008, label=lab4, parse=FALSE, size=3, hjust=0)+
            theme_classic()
        

        
        
        })
    
})









