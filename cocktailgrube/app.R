ui <- fluidPage(
    searchbar("sb", "text"),
    textOutput("text")
)

server <- function(input, output) {
    output$text <- renderText("Hello world!")
}

shinyApp(ui, server)