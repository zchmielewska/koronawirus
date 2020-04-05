library(rdrop2)
library(dplyr)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)



last.file <- drop_dir("ECDC") %>% select(name) %>% arrange(desc(name)) %>% slice(1) %>% pull()
last.data <- drop_read_csv(paste0("ECDC/", last.file))

if(!(as.Date(gsub(".csv", "", last.file)) == Sys.Date())) {
  base.url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"
  new.exists <- !(testit::has_error(readxl::read_excel(paste0(base.url, Sys.Date(), ".xlsx"))))
  if(new.exists) {
    ecdc.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", stringsAsFactors = FALSE)
    ecdc.filename <- paste0(Sys.Date(), ".csv")
    write.csv(ecdc.data, ecdc.filename)
    drop_upload(ecdc.filename, path = "ECDC")
    last.data <- drop_read_csv(paste0("ECDC/", ecdc.filename))
  }
}

return(last.data)



