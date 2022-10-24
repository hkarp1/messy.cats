# name shiny
library(shiny)
library(messy.cats)
library(dplyr)
library(magrittr)
library(DT)
library(readr)

ui <- fluidPage(
  sidebarLayout(
    fileInput(inputId = "messy", label = "messy"),
    fileInput(inputId = "clean", label = "clean")
    ),
  mainPanel(

    selectInput(inputId = "m.col", label = "input a messy file",
                choices = 'No choices here yet'),
    selectInput(inputId = "c.col", label = "input a clean file",
                choices = 'No choices here yet'),
    dataTableOutput(outputId = "matched")
  )
)

server <- function(input, output, session) {
  observeEvent(input$messy, {
    messy.file = input$messy
    messy = read_csv(messy.file$datapath)

    updateSelectInput(session, "m.col", label = "Select",
                      choices = colnames(messy))
  })
  observeEvent(input$clean, {
    clean.file = input$clean
    clean = read_csv(clean.file$datapath)

    updateSelectInput(session, "c.col", label = "Select",
                      choices = colnames(clean))
  })
  observeEvent(input$m.col, {
    messy.file = input$messy
    messy = read_csv(messy.file$datapath)

    updateSelectInput(session, "m.col", label = "Select",
                      choices = colnames(messy))
  })
  output$matched <-  renderDataTable({
    m.col = input$m.col
    c.col = input$c.col
    cat_match(messy$m.col,clean$c.col)
  })
}

# NOT SURE IF YOU NEED THIS
# observeEvent(input$m.col, {
#   m.col = input$m.col
# })
# observeEvent(input$c.col, {
#   c.col = input$c.col
# })

shinyApp(ui = ui, server = server)
