
# Setup -------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(DT)

source("utils/utility-functions.R")

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

settings    <- loadSettings()
ecdc        <- loadECDC()
poland.data <- getPolandData(ecdc$data)
world.data  <- getWorldData(ecdc$data)


# Application -------------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Polska", tabName = "poland", icon = icon("flag")),
        menuItem("Świat",  tabName = "world",  icon = icon("globe")),
        menuItem("Dziś",   tabName = "today",  icon = icon("calendar")),
        menuItem("Info",   tabName = "info",   icon = icon("info")),
        br(),
        p(paste("Dane z dnia:", ecdc$date), 
          align = "left", 
          style="margin-left: 1.5em; font-size:80%;")
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
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
                    plotly::plotlyOutput("polandPlot")
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
        
        if(length(input$worldCountry)) {
            world.data.plot <- filter(world.data, Country %in% input$worldCountry)
            
            ggplot(world.data.plot, aes_string(x = "Date", y = y, color = "Country")) +
                geom_point() +
                geom_line() +
                ggtitle(y.var) +
                xlab("") +
                ylab("")    
        } else {
            ggplot() +
                ggtitle(y.var)
        }
    })
    
    # This function is here because otherwise R has problems with Polish letters
    getWorldDataTable <- function() {
        world.data %>% 
            filter(Date == ecdc$date) %>% 
            filter(complete.cases(.)) %>% 
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
    }
    
    output$todayTable <- renderDT(
        datatable(getWorldDataTable(), 
        rownames = FALSE,
        selection = "none",
        options = list(lengthMenu = c(10, 50, 100))) %>% 
            formatPercentage(columns = c(5, 6), digits = 2)
    )
    
}

ui <- dashboardPage(
    dashboardHeader(title = "Koronawirus"),
    sidebar,
    body   
)

shinyApp(ui = ui, server = server)