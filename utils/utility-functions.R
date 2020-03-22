# returnBreaks(357)
returnBreaks <- function(max) {
  by <- max(round(max/9), 1)
  result <- seq(0, max + by, by)  
  return(result)
}

# data <- data.frame(v = rep(1, 5), Country = c(rep("PL", 2), rep("IT", 3)))
# deltaToTotal(data, "v")
deltaToTotal <- function(data.1, ColumnDelta) {
  n <- nrow(data.1)
  result <- rep(NA, n)
  
  for(i in 1:n) {
    if(i == 1) {
      result[i] <- pull(data.1[i, ColumnDelta])
    } else if (data.1$Country[i] != data.1$Country[i-1]) {
      result[i] <- pull(data.1[i, ColumnDelta])
    } else {
      result[i] <- result[[i-1]] + pull(data.1[i, ColumnDelta])
    }
  }
  
  result <- unlist(result)
  return(result)
}

# df <- data.frame(
#   Country = c(rep("PL", 3), rep("IT", 2)),
#   Date    = as.Date(c("2020-02-01", "2020-02-02", "2020-02-04", "2020-01-15", "2020-01-17")),
#   Value   = c(1, 2, 4, 24, 27)
# )
# addMissingDates(df)
addMissingDates <- function(data) {
  output <- data.frame()
  countries <- unique(data$Country)
  
  for(country in countries) {
    date.min     <- data %>% filter(Country == country) %>% select(Date) %>% pull() %>% min()
    date.max     <- data %>% filter(Country == country) %>% select(Date) %>% pull() %>% max()
    date.period  <- seq(date.min, date.max, by = "1 day")
    data.country <- filter(data, Country == country)
    
    for(i in 1:length(date.period)) {
      d <- date.period[i]
      n <- data.country %>% filter(Date == d) %>% nrow()
      
      # for no data, assume the same as previous day
      if(n == 0) {
        prev.data    <- data.country %>% filter(Date == d-1) %>% select(-c(Date))
        prev.day     <- cbind(data.frame(Date = d), prev.data)
        data.country <- rbind(data.country, prev.day)
      }
    }
    output <- rbind(output, data.country)
  }
  output <- arrange(output, Country, Date)
  return(output)
}

addEpidemiaDay <- function(data.4) {
  output <- mutate(data.4, EpidemiaDay = NA)
  for(i in 1:nrow(data.4)) {
    if(i == 1) {
      output$EpidemiaDay[i] <- 1
    } else{
      if(output$Country[i] == output$Country[i-1]) {
        output$EpidemiaDay[i] <- output$EpidemiaDay[i-1] + 1
      } else {
        output$EpidemiaDay[i] <- 1
      }
    }
  }  
  return(output)
}

prepareData <- function(data.raw) {
  
  # Rename, order, filter only PL and IT
  data.0 <- as_tibble(data.raw) %>% 
    rename(
      Date = DateRep,
      Country = `Countries and territories`,
      CasesDelta = Cases,
      DeathsDelta = Deaths
    ) %>% 
    filter(Country == "Poland" | Country == "Italy") %>% 
    arrange(Country, Date) %>% 
    mutate(Date = as.Date(Date),
           DeathsDelta = as.integer(DeathsDelta)) %>% 
    select(-c(Day, Month, Year, GeoId))
  
  # Change Italy's 1st epidemia day
  data.1 <- data.0
  data.1[data.1$Date == as.Date("2020-01-31") & data.1$Country == "Italy", "CasesDelta"] <- 0
  data.1[data.1$Date == as.Date("2020-02-21") & data.1$Country == "Italy", "CasesDelta"] <- 3
  
  # Add totals
  data.2 <- data.1 %>% 
    mutate(
      CasesTotal  = deltaToTotal(data.1, ColumnDelta = "CasesDelta"),
      DeathsTotal = deltaToTotal(data.1, ColumnDelta = "DeathsDelta")
    )
  
  data.3 <- filter(data.2, CasesTotal != 0)
  data.4 <- addMissingDates(data.3)
  data.5 <- addEpidemiaDay(data.4)
  
  return(data.5)
}

loadECDC <- function(last.known.date = "2020-03-21") {
  url.base <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"
  ecdc <- list()
  tryCatch(
    {
      ecdc$data.raw <- rio::import(paste0(url.base, Sys.Date(), ".xlsx"))  
      ecdc$date     <- Sys.Date()
    }, 
    error = function(e) {
      tryCatch(
        {
          ecdc$data.raw <- rio::import(paste0(url.base, Sys.Date()-1, ".xlsx"))  
          ecdc$date     <- Sys.Date() - 1
        },
        error = function(er) {
          ecdc$data.raw <- rio::import(paste0(url.base, last.known.date, ".xlsx"))  
          ecdc$date     <- last.known.date
        }
      )
    }
  )
  return(ecdc)
}
