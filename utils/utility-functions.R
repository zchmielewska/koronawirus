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

loadECDC <- function(last.known.date = "2020-03-28") {
  url.base <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"
  ecdc <- list()
  if.loaded <- FALSE
  
  # ECDC loads data some time during the day
  if(!testit::has_error(rio::import(paste0(url.base, Sys.Date(), ".xlsx")))) {
    ecdc$data <- rio::import(paste0(url.base, Sys.Date(), ".xlsx"))  
    ecdc$date <- Sys.Date()
    if.loaded <- TRUE
  }  
  
  if(!if.loaded) {
    if(!testit::has_error(rio::import(paste0(url.base, Sys.Date()-1, ".xlsx")))) {
      ecdc$data <- rio::import(paste0(url.base, Sys.Date()-1, ".xlsx"))  
      ecdc$date <- Sys.Date()-1
      if.loaded <- TRUE
    } 
  }
  
  # ECDC sometimes changes the column names which crashes the app
  if(if.loaded) {
    if.columns.ok <- all(colnames(ecdc$data) %in% c("dateRep", "day", "month", 
                                                    "year", "cases", "deaths", 
                                                    "countriesAndTerritories", 
                                                    "geoId", "countryterritoryCode", 
                                                    "popData2018"))
  }
  
  # For safety, use the data from the last known day
  if(!(if.loaded & if.columns.ok)) {
    if(!testit::has_error(rio::import(paste0(url.base, last.known.date, ".xlsx")))) {
      ecdc$data <- rio::import(paste0(url.base, last.known.date, ".xlsx"))  
      ecdc$date <- as.Date(last.known.date)
    } 
  }
  
  # Prepare
  ecdc$data <- ecdc$data %>% 
    as_tibble() %>% 
    rename(
      Date = dateRep,
      Country = `countriesAndTerritories`,
      CasesDelta = cases,
      DeathsDelta = deaths) %>% 
    mutate(Date = as.Date(Date),
           DeathsDelta = as.integer(DeathsDelta))
  
  return(ecdc)
}

getPolandData <- function(data) {
  
  # filter and rename
  result <- data %>% 
    filter(Country == "Poland")  %>% 
    select(-c(day, month, year, Country, popData2018, geoId)) %>% 
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

getWorldData <- function(data) {
  result <- data %>% 
    select(Date, CasesDelta, DeathsDelta, Country, popData2018) %>% 
    group_by(Country) %>% 
    arrange(Country, Date) %>% 
    mutate(
      CasesTotal = aggregate(CasesDelta),
      DeathsTotal = aggregate(DeathsDelta),
    ) %>% 
    ungroup()
  return(result)
}
