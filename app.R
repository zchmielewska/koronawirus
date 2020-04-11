
# Setup -------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(DT)
library(purrr)
library(tidyr)
source("utils/utility-functions.R")

# Functions with polish letters (crash otherwise)
loadSettings <- function() {
    settings <- list()
    
    settings$y.vars <- tribble(
        ~ColumnName,   ~FullName,
        "CasesTotal",  "Zakażenia",
        "CasesDelta",  "Przyrost zakażeń",
        "DeathsTotal", "Zgony", 
        "DeathsDelta", "Przyrost zgonów"
    )
    
    settings$x.vars <- tribble(
        ~ColumnName,   ~FullName,
        "EpidemiaDay", "Dzień epidemii",
        "Date",        "Dzień kalendarzowy"
    )
    return(settings)
}
settings <- loadSettings()
getWorldToday <- function(ecdc.data, ecdc.date) {
    result <- ecdc.data %>% 
        filter(Date == ecdc.date) %>% 
        arrange(desc(CasesTotal)) %>% 
        mutate(CasesInPop = CasesTotal/popData2018,
               Mortality  = DeathsTotal/CasesTotal) %>% 
        select(Country, CasesTotal, DeathsTotal, popData2018, CasesInPop, Mortality) %>% 
        rename(`Kraj / terytorium` = Country,
               Zakażenia = CasesTotal,
               Zgony = DeathsTotal,
               Populacja = popData2018,
               `Zakażenia w populacji` = CasesInPop,
               `Śmiertelność` = Mortality)
    return(result)
}

# UI ----------------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Polska", tabName = "poland", icon = icon("flag")),
        menuItem("Świat",  tabName = "world",  icon = icon("globe")),
        menuItem("Dziś",   tabName = "today",  icon = icon("calendar")),
        menuItem("Info",   tabName = "info",   icon = icon("info"))
    )
)

body <- dashboardBody(
    tabItems(
        # Poland UI ---------------------------------------------------------------
        tabItem(
            tabName = "poland",
            fluidRow(
                valueBoxOutput("epidemiaDayBox"),
                valueBoxOutput("casesBox"),
                valueBoxOutput("deathsBox")
            ),
            fluidRow(
                box(width = 9, solidHeader = TRUE,
                    plotly::plotlyOutput("polandPlot")
                    ),
                box(title = "Ustawienia", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput(
                        inputId = "polandYVar",
                        label = "Zmienna", choices = settings$y.vars$FullName,
                        selected = "Całkowita liczba zakażeń"
                    ),
                    checkboxInput("polandLogScale", "Skala logarytmiczna", value = FALSE),
                    hr(),
                    textOutput("datestamp")
                )
            )                    
        ),

        # World UI ----------------------------------------------------------------
        tabItem(
            tabName = "world",
            fluidRow(
                box(width = 9,
                    plotly::plotlyOutput("worldPlot")
                ),
                box(width = 3,
                    selectInput(
                     inputId = "worldCountry",
                     label = "Wybierz kraj", choices = c("China", "Italy"),
                     selected = c("China", "Italy"), multiple = TRUE
                    ),
                    selectInput(
                        inputId = "worldYVar",
                        label = "Zmienna", choices = settings$y.vars$FullName,
                        selected = "Całkowita liczba zakażeń"
                    ),
                    selectInput(
                        inputId = "worldXVar",
                        label = "Przebieg czasu", choices = settings$x.vars$FullName,
                        selected = "Dzień kalendarzowy"
                    ),
                    checkboxInput("worldLogScale", "Skala logarytmiczna", value = FALSE)
                )
            )
        ),
        tabItem(
            tabName = "today",
            fluidRow(
                box(width = 9,
                     DTOutput("todayTable")
                )
            )
        ),
        tabItem(
            tabName = "info",
            fluidRow(
                box(title = "Źródło", width = 6, 
                    status = "primary", solidHeader = TRUE,
                    p("Dane zasilające wykresy i tabelę pochodzą ze strony
                      ECDC (European Centre for Disease Prevention and Control).",
                      style="text-align: justify;"),
                    p("Są one publicznie dostępne na stronie:",
                      tags$a(href = "https://www.ecdc.europa.eu/en", "www.ecdc.europa.eu")),
                    p("Warto pamiętać, że liczba zakażeń to jedynie liczba potwierdzonych przypadków.
                      Dane ECDC nie zawierają informacji na temat liczby przeprowadzonych testów 
                      w danym kraju. Im więcej przeprowadzanych testów, tym więcej zidentyfikowanych
                      zakażeń, nawet w przypadkach lekkich objawów choroby. Ma to zatem wpływ na 
                      statystkę śmiertelność, która została wyznaczona jako liczba zgonów
                      do liczby potwierdzonych zakażeń. Faktyczna śmiertelność jest zatem niższa,
                      ponieważ wiele osób przechodzi chorobę bezobjawowo lub nie zgłosiło się
                      faktu zachorowania.", 
                      style="text-align: justify;")
                ),
                box(width = 3,
                    status = "primary", solidHeader = TRUE,
                    p("W przypadku jakichkolwiek problemów, proszę zgłoś",
                      tags$a(href = "https://github.com/zchmielewska/koronawirus/issues", "tutaj"),
                      "swoje uwagi.")
                    )
                )
            )
        )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    # Prepare data ------------------------------------------------------------

    notify <- function(msg, id = NULL) {
        showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
    }

    ecdc.data <- reactive({
        id <- notify("Wczytuję dane ECDC...")
        on.exit(removeNotification(id), add = TRUE)
        data <- loadData()
    })
    
    ecdc.date <- reactive(
        ecdc.date <- ecdc.data() %>% select(Date) %>% pull() %>% max()
    )
    
    poland.data <- reactive({
        id <- notify("Przygotowuję dane dla Polski...")
        on.exit(removeNotification(id), add = TRUE)
        ecdc.data() %>% filter(Country == "Poland")
    }) 
    
    world.data.today <- reactive({
        id <- notify("Sprawdzam dzisiejsze dane...")
        on.exit(removeNotification(id), add = TRUE)
        getWorldToday(ecdc.data(), ecdc.date())
    })
    
    # Poland page -------------------------------------------------------------

    output$epidemiaDayBox <- renderValueBox({
        valueBox(
            value = as.double(difftime(lubridate::ymd(ecdc.date()), lubridate::ymd("2020-03-03"), units = "days")),
            subtitle = "Dzień epidemii", icon = icon("first-aid"),
            color = "purple"
        )
    })
    
    output$casesBox <- renderValueBox({
        poland.today <- filter(poland.data(), Date == ecdc.date())
        total        <- poland.today %>% select(CasesTotal) %>% pull()
        delta        <- poland.today %>% select(CasesDelta) %>% pull()
        valueBox(
            paste0(total, " (+", delta, ")"), "Zakażenia", icon = icon("diagnoses"),
            color = "orange"
        )
    })
    
    output$deathsBox <- renderValueBox({
        poland.today <- filter(poland.data(), Date == ecdc.date())
        total        <- poland.today %>% select(DeathsTotal) %>% pull()
        delta        <- poland.today %>% select(DeathsDelta) %>% pull()
        valueBox(
            paste0(total, " (+", delta, ")"), "Zgony", icon = icon("times"),
            color = "navy"
        )
    })
    
    output$polandPlot <- plotly::renderPlotly({
        y.var <- input$polandYVar # y.var <- "Całkowita liczba zakażeń"
        y     <- settings$y.vars %>% filter(FullName == y.var) %>% select(ColumnName) %>% pull()
        
        p1 <- ggplot(poland.data(), aes_string(x = "Date", y = y)) +
            geom_point() +
            geom_line() +
            theme(panel.grid.minor = element_blank()) +
            ggtitle(y.var) +
            xlab(NULL)
        
        if(input$polandLogScale) {
            p2 <- p1 + scale_y_continuous("", labels = scales::comma_format(accuracy = 1, big.mark = " "), trans='log10')
        } else {
            p2 <- p1 + scale_y_continuous("", labels = scales::comma_format(accuracy = 1, big.mark = " "))
        }
        
        p2
    })
    
    output$datestamp <- renderText(
        paste("Dane z dnia:", ecdc.date())
    )
    
    # World page --------------------------------------------------------------

    output$worldPlot <- plotly::renderPlotly({
        x.var <- input$worldXVar # y.var <- "Dzień kalendarzowy"
        x     <- settings$x.vars %>% filter(FullName == x.var) %>% select(ColumnName) %>% pull()
        y.var <- input$worldYVar # y.var <- "Całkowita liczba zakażeń"
        y     <- settings$y.vars %>% filter(FullName == y.var) %>% select(ColumnName) %>% pull()
        
        if(length(input$worldCountry) > 0) {
            world.data.plot <- filter(ecdc.data(), Country %in% input$worldCountry)
            
            p1 <- ggplot(world.data.plot, aes_string(x = x, y = y, colour = "Country")) +
                geom_point(size = 1, alpha = 0.8) +
                geom_line(alpha = 0.8) +
                ggtitle(y.var) +
                xlab(NULL) +
                scale_colour_discrete("Kraj")
        } else {
            p1 <- ggplot() + ggtitle(y.var)
        }
        
        if(input$worldLogScale) {
            p2 <- p1 + 
                scale_y_continuous("", labels = scales::comma_format(accuracy = 1,  big.mark = " "), trans ='log10')
        } else {
            p2 <- p1 + 
                scale_y_continuous("", labels = scales::comma_format(accuracy = 1,  big.mark = " "))
        }
    })
    
    observeEvent(ecdc.data(), {
        choices <- unique(ecdc.data()$Country)
        updateSelectInput(session, "worldCountry", choices = choices, 
                          selected = c("China", "Italy")) 
    })

    # Today page --------------------------------------------------------------

    output$todayTable <- renderDT(
        datatable(world.data.today(),
        rownames = FALSE,
        selection = "none",
        options = list(lengthMenu = c(10, 50, 100))) %>% 
            formatPercentage(columns = c(5, 6), digits = 2)
    )
}

# Run application ---------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Koronawirus"),
    sidebar,
    body   
)

shinyApp(ui = ui, server = server)