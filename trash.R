
loadLastFile <- function() {
  last.file <- drop_dir("ECDC") %>% select(name) %>% arrange(desc(name)) %>% slice(1) %>% pull()
  last.data <- drop_read_csv(paste0("ECDC/", last.file))
  return(last.data)
}


checkNewData <- function(data) {
  result <- data
  
  # if the data is from yesterday; check if new exists
  last.file <- drop_dir("ECDC") %>% select(name) %>% arrange(desc(name)) %>% slice(1) %>% pull()
  if(as.Date(gsub(".csv", "", last.file)) < Sys.Date()) {
    base.url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"
    new.exists <- !(testit::has_error(rio::import(paste0(base.url, Sys.Date(), ".xlsx"))))
    if(new.exists) {
      ecdc.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = FALSE)
      ecdc.filename <- paste0(Sys.Date(), ".csv")
      write.csv(ecdc.data, ecdc.filename)
      drop_upload(ecdc.filename, path = "ECDC")
      last.data <- drop_read_csv(paste0("ECDC/", ecdc.filename))
      result <- last.data
    }
  }
  
  return(result)
}

# add epidemia day
result <- result %>% 
  arrange(Date) %>% 
  mutate(EpidemiaDay = 1:nrow(result))