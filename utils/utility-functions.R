nest_period <- function(min_date, max_date) {
  result <- data.frame(Date = seq(min_date, max_date, by = "1 day")) %>% 
    group_by(Date) %>%
    nest(data = c(Date))
  return(result)
}

loadData <- function() {
  # Data from ECDC
  raw.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = FALSE)  

  # Rename columns, change types, filter unncessary data and arrange
  clean.data <- as_tibble(raw.data) %>% 
    rename(
      Country = `countriesAndTerritories`,
      CasesDelta = cases,
      DeathsDelta = deaths) %>% 
    mutate(Date = as.Date(paste(year, month, day, sep="-")),
           CasesDelta = as.numeric(CasesDelta),
           DeathsDelta = as.numeric(DeathsDelta)) %>% 
    select(c(Country, Date, CasesDelta, DeathsDelta, Date, popData2018)) %>% 
    filter(complete.cases(.) & CasesDelta != 0) %>% 
    arrange(Country, Date)
  
  # Prepare df with full periods
  periods <- clean.data %>% 
    group_by(Country) %>% 
    summarize(
      MinDate = min(Date),
      MaxDate = max(Date)
    ) %>% 
    mutate(data = map2(MinDate, MaxDate, nest_period)) %>% 
    select(-c(MinDate, MaxDate)) %>% 
    unnest(data) %>% unnest(data)
  
  # Prepare population data
  populations <- clean.data %>% 
    select(Country, popData2018) %>% 
    unique()
  
  # Join data to the full periods df
  full.data <- periods %>% 
    left_join(select(clean.data, -c(popData2018)), by = c("Country", "Date")) %>% 
    replace_na(list(CasesDelta = 0, DeathsDelta = 0)) %>% 
    left_join(populations, by = "Country") %>% 
    group_by(Country) %>% 
    mutate(CasesTotal = cumsum(CasesDelta),
           DeathsTotal = cumsum(DeathsDelta))
  
  # Number of epidemia days
  no.days <- full.data %>% tally() %>% select(n) %>% pull()
  
  # Final data
  result <- full.data %>%
    ungroup() %>%
    mutate(EpidemiaDay = unlist(sapply(no.days, function(x) 1:x)))
 
  return(result) 
}
