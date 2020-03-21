library(shiny)
library(tidyverse)
library(rio)
library(rlang)
source("utils/utility-functions.R")

# Data --------------------------------------------------------------------

data.raw <- rio::import("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-20.xlsx")
data <- prepareData(data.raw)

# Application -------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Koronawirus - dane z ECDC"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "var",
                label = "Wybierz zmienną:",
                choices = colnames(data)[-1],
                selected = "CasesTotal"
            )
        ),
        mainPanel(
           plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    output$plot <- renderPlot({
        ggplot(data, aes_string(x = "Date", y = input$var)) +
            geom_point()
    })
}

shinyApp(ui = ui, server = server)