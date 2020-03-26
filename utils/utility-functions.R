aggregate <- function(v) {
  result <- c()
  for(i in 1:length(v)) {
    if(i == 1) {
      result[i] <- v[i] 
    } else {
      result[i] <- result[i-1] + v[i]
    }
  }
  return(result)
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

loadECDC <- function(last.known.date = "2020-03-22") {
  url.base <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"
  ecdc <- list()
  if.loaded <- FALSE
  
  # ECDC loads data some time during the day
  if(!testit::has_error(rio::import(paste0(url.base, Sys.Date(), ".xlsx")))) {
    ecdc$data.raw <- rio::import(paste0(url.base, Sys.Date(), ".xlsx"))  
    ecdc$date     <- Sys.Date()
    if.loaded     <- TRUE
  }  
  
  if(!isTRUE(if.loaded)) {
    if(!testit::has_error(rio::import(paste0(url.base, Sys.Date()-1, ".xlsx")))) {
      ecdc$data.raw <- rio::import(paste0(url.base, Sys.Date()-1, ".xlsx"))  
      ecdc$date     <- Sys.Date()-1
      if.loaded     <- TRUE
    } 
  }
  
  if(!isTRUE(if.loaded)) {
    if(!testit::has_error(rio::import(paste0(url.base, last.known.date, ".xlsx")))) {
      ecdc$data.raw <- rio::import(paste0(url.base, last.known.date, ".xlsx"))  
      ecdc$date     <- last.known.date
    } 
  }
  
  return(ecdc)
}

# getPolandData(ecdc$data.raw)
getPolandData <- function(data) {
  
  # filter and rename
  result <- data %>% 
    as_tibble() %>% 
    rename(
      Date = DateRep,
      Country = `Countries and territories`,
      CasesDelta = Cases,
      DeathsDelta = Deaths) %>% 
    filter(Country == "Poland")  %>% 
    mutate(Date = as.Date(Date),
           DeathsDelta = as.integer(DeathsDelta)) %>% 
    select(-c(Day, Month, Year, Country, Pop_Data.2018, GeoId)) %>% 
    arrange(Date) %>% 
    mutate(
      CasesTotal  = aggregate(CasesDelta),
      DeathsTotal = aggregate(DeathsDelta)
    )
  
  # add missing days
  date.min     <- min(result$Date)
  date.max     <- max(result$Date)
  date.period  <- seq(date.min, date.max, by = "1 day")
  
  for(i in 1:length(date.period)) {
    d <- date.period[i]
    n <- result %>% filter(Date == d) %>% nrow()
    
    # for no data, assume the same as previous day
    if(n == 0) {
      prev.data <- result %>% filter(Date == d-1) %>% select(-c(Date))
      prev.day  <- cbind(data.frame(Date = d), prev.data)
      result     <- rbind(result, prev.day)
    }
  }
  
  # add epidemia day
  result <- result %>% 
    arrange(Date) %>% 
    mutate(EpidemiaDay = 1:nrow(result))
  
  return(result)
}