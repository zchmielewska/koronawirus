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
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "country",
                label = "Wybierz kraj:",
                choices = unique(data$Country),
                selected = c("Poland", "Italy"),
                multiple = TRUE
            ),
            
            selectInput(
                inputId = "var",
                label = "Zmienna:",
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
            # textOutput("test"), 
            plotly::plotlyOutput("plot"),
            p(paste("Dane ECDC opublikowane w dniu:", ecdc$date))
        )
    )
)

server <- function(input, output, session) {
    output$test <- renderText({input$country})
    
    output$plot <- plotly::renderPlotly({
        
        country <- input$country
        if(is.null(country)) country <- "Poland"
        plot.data <- filter(data, Country == country)
        
        # x.axis <- "Dzień epidemii"
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
}

shinyApp(ui = ui, server = server)