library(tidyverse)
library(rsdmx)


my_read_istat <- function(id) {
  
  resources <- readSDMX(providerId = "ISTAT", resource = "dataflow") |> 
    as_tibble()
  
  tmp <- readSDMX(
    providerId = "ISTAT",
    resource = "data", 
    flowRef  = id,
    dsd = TRUE
    ) %>% 
    as.data.frame(labels = TRUE)
  
  saveRDS(tmp, str_c("~/R/OIID/topics/datasets/tmp_", id, ".rds"))
}

my_read_istat("163_156")
