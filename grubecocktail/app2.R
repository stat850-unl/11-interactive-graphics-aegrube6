#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinySearchbar)
library(ggplot2)



#get data 
#press enter on individual lines to download data as an object and get the data in the environment 
cocktails<-read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
bstncocktails<-read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
#downloads data as a dataframe
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cocktail Ingredients "),

    # Sidebar panel for inputs 
    sidebarLayout(
        sidebarPanel(
            #search bar on the sidebar panel 
            #source https://rdrr.io/cran/shinyWidgets/man/searchInput.html 
            tags$h1("Ingredient filter "),
            br(),
            searchInput(
                inputId = "search", 
                label = "Enter desired ingredient",
                placeholder = "Ingredient Name",
                btnSearch = icon("search"),
                btnReset = icon("remove"),
            )
            
        ),

        # main display
        mainPanel(
          #data interaction 
          # boston<- filter(bstncocktails,category=="Cocktail Classics",.preserve=TRUE),
          # all<-filter(cocktails, category=="Cocktail")
            splitLayout(
            plotOutput("distplot"),
            plotOutput("distplot2")
            
        )
        )
    )
)

# Define server logic required to print data
server <- function(input, output) {
    dfb<-data.frame(bstncocktails)
    dfc<-data.frame(cocktails)
    bcount<-table(dfb$name)
    ccount<-table(dfc$drink)

    #function to control the search bar 
    output$distplot <- renderPlot({
        
        x<-bcount
        hist(x)
        
    })
    output$distplot2 <- renderPlot({
        
        y=ccount
        hist(y)
    })
  
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
