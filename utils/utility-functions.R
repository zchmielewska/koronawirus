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