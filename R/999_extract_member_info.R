library(xml2)
library(tidyverse)
library(stringr)
library(parallel)

n_cores <- 20
here::i_am("R/001_extract_member_info.R")
infiles <- list.files(here::here("data/ca-members"),
                      full.names = TRUE)
                      
get_vitals <- function(f) {
    report <- read_xml(f)
    id <- xml_find_first(report, "//pm:member") |>
        xml_attr("id")
    dob <- xml_find_first(report, "//pm:born") |>
        xml_attr("date") |>
        as.Date()
    gender <- xml_find_first(report, "//pm:gender") |>
        xml_text()
    return(data.frame(id = id,
                      dob = dob,
                      gender = gender))
}

res <- bind_rows(lapply(infiles, get_vitals))

write.csv(res,
          file = here::here("working", "member_vitals.csv"),
          row.names = FALSE)
