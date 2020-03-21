library(shiny)
library(tidyverse)
library(rio)
library(scales)
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
        y <- input$var
        breaks.y <- returnBreaks(max(data[y]))
        
        ggplot(data, aes_string(x = "Date", y = input$var)) +
            geom_point() +
            scale_x_date(breaks = date_breaks("days")) +
            theme(axis.text.x = element_text(angle=270)) +
            scale_y_continuous(breaks = breaks.y, limits = c(0, max(breaks.y)))+
            theme(panel.grid.minor = element_blank()) +
            xlab("")
    })
}

shinyApp(ui = ui, server = server)