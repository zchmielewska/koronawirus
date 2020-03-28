
# Setup -------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(rio)
library(scales)
library(shinydashboard)
source("utils/utility-functions.R")

loadSettings <- function() {
    settings <- list()
    
    settings$y.vars <- tribble(
        ~ColumnName,   ~FullName,
        "CasesTotal",  "Całkowita liczba zakażeń",
        "CasesDelta",  "Przyrost liczby zakażeń",
        "DeathsTotal", "Całkowita liczba zgonów", 
        "DeathsDelta", "Przyrost liczby zgonów"
    )
    
    settings$x.vars <- tribble(
        ~ColumnName,   ~FullName,
        "EpidemiaDay", "Dzień epidemii",
        "Date",        "Dzień kalendarzowy"
    )
    
    return(settings)
}

settings    <- loadSettings()
ecdc        <- loadECDC()
poland.data <- getPolandData(ecdc$data)
world.data  <- getWorldData(ecdc$data)


# Application -------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Polska", tabName = "poland", icon = icon("flag")),
        menuItem("Świat",  tabName = "world",  icon = icon("globe"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "poland",
            fluidRow(
                valueBoxOutput("epidemiaDayBox"),
                valueBoxOutput("casesBox"),
                valueBoxOutput("deathsBox")
            ),
            fluidRow(
                box(width = 9, solidHeader = TRUE,
                    plotly::plotlyOutput("polandPlot"),
                    p(paste("Dane z dnia:", ecdc$date))
                    ),
                box(title = "Ustawienia", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput(
                        inputId = "polandYVar",
                        label = "Zmienna", choices = settings$y.vars$FullName,
                        selected = "Całkowita liczba zakażeń"
                    ),
                    selectInput(
                        inputId = "polandXVar",
                        label = "Przebieg czasu", choices = settings$x.vars$FullName,
                        selected = "Dzień epidemii"
                    )
                )
            )                    
        ),
        tabItem(
            tabName = "world",
            fluidRow(
                box(width = 9,
                    plotly::plotlyOutput("worldPlot")
                ),
                box(width = 3,
                    selectInput(
                     inputId = "worldCountry",
                     label = "Wybierz kraj", choices = unique(world.data$Country),
                     selected = c("China", "United_States_of_America"), multiple = TRUE
                    ),
                    selectInput(
                        inputId = "worldYVar",
                        label = "Zmienna", choices = settings$y.vars$FullName,
                        selected = "Całkowita liczba zakażeń"
                    )
                )
            )
        )
    )
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
    
    output$polandPlot <- plotly::renderPlotly({
        x.var <- input$polandXVar # x.var <- "Dzień epidemii"
        x     <- settings$x.vars %>% filter(FullName == x.var) %>% select(ColumnName) %>% pull()
        y.var <- input$polandYVar # y.var <- "Całkowita liczba zakażeń"
        y     <- settings$y.vars %>% filter(FullName == y.var) %>% select(ColumnName) %>% pull()
        
        p1 <- ggplot(poland.data, aes_string(x = x, y = y)) +
            geom_point() +
            geom_line() +
            theme(panel.grid.minor = element_blank()) +
            ggtitle(y.var) +
            xlab(x.var) +
            ylab("")
        
        if(x == "EpidemiaDay") {
            p2 <- p1 + 
                scale_x_continuous(breaks = seq(1, max(poland.data$EpidemiaDay), by = 1))
        } else if (x == "Date") {
            p2 <- p1 +
                scale_x_date(breaks = seq(min(poland.data$Date), max(poland.data$Date), by = 1)) +
                theme(axis.text.x = element_text(angle=270))
        }
        
        p2
    })
    
    output$worldPlot <- plotly::renderPlotly({
        y.var <- input$worldYVar # y.var <- "Całkowita liczba zakażeń"
        y     <- settings$y.vars %>% filter(FullName == y.var) %>% select(ColumnName) %>% pull()
        
        # world.data.plot <- filter(world.data, Country %in% "Poland")
        world.data.plot <- filter(world.data, Country %in% input$worldCountry)
        
        ggplot(world.data.plot, aes_string(x = "Date", y = y, color = "Country")) +
            geom_point() +
            geom_line() +
            ggtitle(y.var) +
            xlab("") +
            ylab("")
    })
    
}

ui <- dashboardPage(
    dashboardHeader(title = "Koronawirus"),
    sidebar,
    body   
)

shinyApp(ui = ui, server = server)