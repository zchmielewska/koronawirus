library(shiny)
library(tidyverse)
library(rio)
library(scales)
source("utils/utility-functions.R")

# Data --------------------------------------------------------------------

data.raw <- rio::import("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-21.xlsx")
data     <- prepareData(data.raw)

vars <- tribble(
    ~ColumnName,   ~FullName,
    "CasesTotal",  "Całkowita liczba zakażeń",
    "CasesDelta",  "Przyrost liczby zakażeń",
    "DeathsTotal", "Całkowita liczba przypadków śmiertelnych", 
    "DeathsDelta", "Przyrost liczby przypadków śmiertelnych")

# Application -------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Koronawirus dla Myszy"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "var",
                label = "Wybierz zmienną:",
                choices = vars$FullName,
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
        y <- vars %>% 
            filter(FullName == input$var) %>% 
            select(ColumnName) %>% pull()
        breaks.y <- returnBreaks(max(data[y]))
        
        ggplot(data, aes_string(x = "EpidemiaDay", y = y, color = "Country")) +
            geom_point() +
            geom_line() +
            scale_x_continuous(breaks = seq(1, max(data$EpidemiaDay), by = 1)) +
            # theme(axis.text.x = element_text(angle=270)) +
            # scale_y_continuous(breaks = breaks.y, limits = c(0, max(breaks.y))) +
            theme(panel.grid.minor = element_blank()) +
            ggtitle(input$var) +
            xlab("Dzień epidemii") +
            ylab("")
    })
}

shinyApp(ui = ui, server = server)