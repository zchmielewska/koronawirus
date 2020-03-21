# data <- data.frame(v = rep(1, 5))
# deltaToTotal(data, "v")
deltaToTotal <- function(data, column) {
  result <- c()
  for(i in 1:nrow(data)) {
    result[i] <- sum(data[1:i, column])
  }
  return(result)
}

prepareData <- function(data.raw) {
  data.1 <- as_tibble(data.raw) %>% 
    rename(
      Date = DateRep,
      Country = `Countries and territories`,
      CasesDelta = Cases,
      DeathsDelta = Deaths
    ) %>% 
    filter(Country == "Poland") %>% 
    arrange(Date) %>% 
    mutate(Date = as.Date(Date),
           DeathsDelta = as.integer(DeathsDelta)) %>% 
    select(-c(Day, Month, Year, Country, GeoId))
  
  data.2 <- data.1 %>% 
    mutate(
      CasesTotal = deltaToTotal(data.1, "CasesDelta"),
      DeathsTotal = deltaToTotal(data.1, "DeathsDelta")
    )
  return(data.2)
}
# returnBreaks(357)
returnBreaks <- function(max) {
  by <- max(round(max/9), 1)
  result <- seq(0, max + by, by)  
  return(result)
}
