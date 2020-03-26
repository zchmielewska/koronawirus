
# Setup -------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(rio)
library(scales)
library(shinydashboard)
source("utils/utility-functions.R")

loadSettings <- function() {
    settings <- list()
    
    settings$vars <- tribble(
        ~ColumnName,   ~FullName,
        "CasesTotal",  "Całkowita liczba zakażeń",
        "CasesDelta",  "Przyrost liczby zakażeń",
        "DeathsTotal", "Całkowita liczba zgonów", 
        "DeathsDelta", "Przyrost liczby zgonów"
    )
    
    settings$x.axes <- tribble(
        ~ColumnName,   ~FullName,
        "EpidemiaDay", "Dzień epidemii",
        "Date",        "Dzień kalendarzowy"
    )
    
    return(settings)
}

ecdc        <- loadECDC()
poland.data <- getPolandData(ecdc$data.raw)
settings    <- loadSettings()

# Application -------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Polska", tabName = "Poland", icon = icon("flag"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "Poland",
            fluidRow(
                valueBoxOutput("epidemiaDayBox"),
                valueBoxOutput("casesBox"),
                valueBoxOutput("deathsBox")
            ),
            fluidRow(
                box(width = 9, solidHeader = TRUE,
                    plotly::plotlyOutput("plot"),
                    p(paste("Dane z dnia:", ecdc$date))
                    ),
                box(title = "Ustawienia", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput(
                        inputId = "var",
                        label = "Zmienna",
                        choices = settings$vars$FullName,
                        selected = "Całkowita liczba zakażeń"
                    ),
                    selectInput(
                        inputId = "x.axis",
                        label = "Przebieg czasu",
                        choices = settings$x.axes$FullName,
                        selected = "Dzień epidemii"
                    )
                )
            )                    
        )
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "Koronawirus"),
    sidebar,
    body   
)

server <- function(input, output, session) {
    
    output$epidemiaDayBox <- renderValueBox({
        valueBox(
            paste0(ecdc$date - as.Date("2020-03-03")), "Dzień epidemii", icon = icon("first-aid"),
            color = "purple"
        )
    })
    
    output$casesBox <- renderValueBox({
        poland.today <- filter(poland.data, Date == ecdc$date)
        total        <- poland.today %>% select(CasesTotal) %>% pull()
        delta        <- poland.today %>% select(CasesDelta) %>% pull()
        valueBox(
            paste0(total, " (+", delta, ")"), "Zakażenia", icon = icon("diagnoses"),
            color = "orange"
        )
    })
    
    output$deathsBox <- renderValueBox({
        poland.today <- filter(poland.data, Date == ecdc$date)
        total        <- poland.today %>% select(DeathsTotal) %>% pull()
        delta        <- poland.today %>% select(DeathsDelta) %>% pull()
        valueBox(
            paste0(total, " (+", delta, ")"), "Zgony", icon = icon("times"),
            color = "navy"
        )
    })
    
    output$plot <- plotly::renderPlotly({
        
        x.axis <- input$x.axis # x.axis <- "Dzień epidemii"
        x      <- settings$x.axes %>% filter(FullName == x.axis) %>% select(ColumnName) %>% pull()
        var    <- input$var # var <- "Całkowita liczba zakażeń"
        y      <- settings$vars %>% filter(FullName == var) %>% select(ColumnName) %>% pull()
        
        p1 <- ggplot(poland.data, aes_string(x = x, y = y)) +
            geom_point() +
            geom_line() +
            theme(panel.grid.minor = element_blank()) +
            ggtitle(var) +
            xlab(x.axis) +
            ylab("")
        
        if(x == "EpidemiaDay") {
            p2 <- p1  +
                scale_x_continuous(breaks = seq(1, max(poland.data$EpidemiaDay), by = 1))
        } else if (x == "Date") {
            p2 <- p1 +
                scale_x_date(breaks = seq(min(poland.data$Date), max(poland.data$Date), by = 1)) +
                theme(axis.text.x = element_text(angle=270))
        }
        
        p2
    })
}

shinyApp(ui = ui, server = server)