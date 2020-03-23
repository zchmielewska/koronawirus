library(shiny)
library(tidyverse)
library(rio)
library(scales)
source("utils/utility-functions.R")

# Data --------------------------------------------------------------------

ecdc     <- loadECDC()
data.raw <- ecdc$data.raw
data     <- prepareData(data.raw)

vars <- tribble(
    ~ColumnName,   ~FullName,
    "CasesTotal",  "Całkowita liczba zakażeń",
    "CasesDelta",  "Przyrost liczby zakażeń",
    "DeathsTotal", "Całkowita liczba przypadków śmiertelnych", 
    "DeathsDelta", "Przyrost liczby przypadków śmiertelnych")

x.axes <- tribble(
    ~ColumnName,   ~FullName,
    "EpidemiaDay", "Dzień epidemii",
    "Date",        "Dzień kalendarzowy"
)

# Application -------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Koronawirus dla Myszy"),
    
    theme = shinythemes::shinytheme("lumen"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "country",
                label = "Kraj",
                choices = unique(data$Country),
                selected = c("Poland", "Italy"),
                multiple = TRUE
            ),
            
            selectInput(
                inputId = "var",
                label = "Zmienna",
                choices = vars$FullName,
                selected = "Całkowita liczba zakażeń"
            ),
            
            selectInput(
                inputId = "x.axis",
                label = "Przebieg czasu",
                choices = x.axes$FullName,
                selected = "Dzień epidemii"
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Wykres",
                    # textOutput("test"), 
                    plotly::plotlyOutput("plot"),
                    p(paste("Dane ECDC opublikowane w dniu:", ecdc$date))    
                ),
                tabPanel("Dane",
                    DT::DTOutput("table")
                ),
                tabPanel("Info",
                    br(),
                    p("Dane zasilające wykresy pochodzą ze strony ECDC (Europejskie Centrum ds. Zapobiegania i Kontroli Chorób)
                      i są dostępne po adresem: https://www.ecdc.europa.eu/en")         
                )
            )
        )
    )
)

server <- function(input, output, session) {
    output$test <- renderText({input$country})
    
    getChosenData <- function() {
        country <- input$country
        if(is.null(country)) country <- "Poland"
        chosen.data <- filter(data, Country %in% country)
        return(chosen.data)
    }
    
    output$plot <- plotly::renderPlotly({
        plot.data <- getChosenData()
        
        # x.axis <- "Dzień kalendarzowy"
        x.axis <- input$x.axis
        x <- x.axes %>% 
            filter(FullName == x.axis) %>% 
            select(ColumnName) %>% pull()
        
        # var <- "Całkowita liczba zakażeń"
        var <- input$var
        y <- vars %>% 
            filter(FullName == var) %>% 
            select(ColumnName) %>% pull()
        
        p1 <- ggplot(plot.data, aes_string(x = x, y = y, color = "Country")) +
            geom_point() +
            geom_line() +
            theme(panel.grid.minor = element_blank()) +
            ggtitle(var) +
            xlab(x.axis) +
            ylab("")
        
        if(x == "EpidemiaDay") {
            p2 <- p1  +
                scale_x_continuous(breaks = seq(1, max(pull(data[x])), by = 1))
        } else if (x == "Date") {
            p2 <- p1 +
                scale_x_date(breaks = seq(min(pull(data[x])), max(pull(data[x])), by = 1)) +
                theme(axis.text.x = element_text(angle=270))
        }
        
        p2
    })
    
    output$table <- DT::renderDT({
        DT::datatable(getChosenData())
    })
}

shinyApp(ui = ui, server = server)