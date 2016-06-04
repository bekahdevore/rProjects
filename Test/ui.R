# ui.R

shinyUI(fluidPage(
        titlePanel("occcupationMatrix"),
        
        sidebarLayout(
                sidebarPanel(
                        helpText("Choose an occupation to see occupations that are related 
                                 based on skill-set. "),
                        
                        selectInput("var", 
                                    label = "Choose a variable to display",
                                    choices = get(careers$Title),
                                    selected = "Sales Managers")
                        
                        ),
                
                mainPanel(plotOutput("occ"))
        )
))