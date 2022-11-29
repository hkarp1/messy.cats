# # name shiny
# library(shiny)
# library(dplyr)
# library(magrittr)
# library(DT)
# library(readr)
#
# ui <- fluidPage(
#   sidebarLayout(
#     fileInput(inputId = "messy", label = "messy"),
#     fileInput(inputId = "clean", label = "clean")),
#   mainPanel(
#     dataTableOutput(outputId = "matched")
#     )
#   )
#
# server <- function(input, output, session) {
#   output$matched <-  renderDataTable({
#     messy.file = input$messy
#     clean.file = input$clean
#
#     messy = read_csv(messy.file$datapath)
#     clean = read_csv(clean.file$datapath)
#
#     name_match(messy,clean)
#     })
# }
#
#
#
#
# shinyApp(ui = ui, server = server)
