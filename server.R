# server.R

library(networkD3)
library(shiny)
careers <- read.csv("~/Desktop/CareerChangersMatrix.csv", header = TRUE)


shinyServer(
        function(input, output) {
                output$occ <- renderPlot({
                       switch(input$var,
                                       "Sales Managers" 
                                       )
                        
                        ##Network Plot
                        g <- graph.data.frame(careers, directed = TRUE)
                        
                        ##D3 integration
                        r <- get.data.frame(g, what="edges")
                        simpleNetwork(r)
                        
                })
        }
)