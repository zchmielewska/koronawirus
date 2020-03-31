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

loadECDC <- function() {
  ecdc <- list()
  data.raw <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = FALSE)
  
  ecdc$data <- data.raw %>% 
    as_tibble() %>% 
    mutate(Date = as.Date(paste(year, month, day, sep="-"))) %>% 
    rename(
      Country = `countriesAndTerritories`,
      CasesDelta = cases,
      DeathsDelta = deaths) %>% 
    mutate(Date = as.Date(Date),
           DeathsDelta = as.integer(DeathsDelta))
  
  ecdc$date <- max(ecdc$data$Date)
  
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
