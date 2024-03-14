library(tidyverse)
library(here)
library(xml2)

here::i_am("R/04_integrate_party_and_biogs.R")

### Read in the data
dat <- readRDS(file = here::here("outputs", "daily_speaker.rds"))

dat <- dat |>
    filter(opid != "") |>
    mutate(speechdate = as.Date(speechdate, format = "%Y-%m-%d"))

### Some IDs are missing the prefix
dat <- dat |>
    mutate(opid = ifelse(!grepl("ca.m.", opid),
                         paste0("ca.m.", opid),
                         opid))

### We're going to read in all the XML files in ca-members
infiles <- list.files(here::here("data/ca-members"),
                      full.names = TRUE)

### We're writing two functions to process this stuff since we can't
### assume that the numbers of rows of data frames returned are equal
parse_biogs_and_parties <- function(file) {
    dat <- read_xml(file)
### get ID
    id <- dat |> xml_find_first("//meta")|> xml_attr("id")
###
    if (!grepl("ca.m.", id)) {
        ## id <- NA_character_
        id <- paste0("ca.m.", id)
    }
### get name
    fullname <- dat |> xml_find_first("//pm:full") |> xml_text()
### get DOB
    borns <- dat |>
        xml_find_first("//pm:born") |>
        xml_attr("date")

    borns <- as.Date(borns)
    ## get gender
    gender <- dat |>
        xml_find_first("//pm:gender") |>
        xml_text()

### Get party memberships
    parties <- dat |>
        xml_find_all("//pm:membership[@pm:party-ref]")

    parties <- lapply(parties, function(p) {
        party_ref <- p |> xml_attr("party-ref")
        start_date <- p |> xml_find_first("pm:period") |> xml_attr("from")
        end_date <- p |> xml_find_first("pm:period") |> xml_attr("till")
        data.frame(party_ref = party_ref,
                   start_date = start_date,
                   end_date = end_date)
    })
    parties <- bind_rows(parties)
    parties$id <- id
    
    dat <- data.frame(id = id,
                      fullname = fullname,
                      dob = borns,
                      gender = gender)

    dat <- left_join(dat, parties,
                     by = join_by(id),
                     relationship = "one-to-many")
    return(dat)
}


parse_seats <- function(file) {
    dat <- read_xml(file)
### get ID
    id <- dat |> xml_find_first("//meta")|> xml_attr("id")
###
    if (!grepl("ca.m.", id)) {
        ## id <- NA_character_
        id <- paste0("ca.m.", id)
    }
    memberships <- dat |> xml_find_all("//pm:membership[@pm:body = 'commons']")
    starts <- sapply(memberships, function(m) {
        m |>
            xml_find_first("./pm:period") |>
            xml_attr("from")
    })
    stops <- sapply(memberships, function(m) {
        m |>
            xml_find_first("./pm:period") |>
            xml_attr("till")
    })

    starts <- unlist(starts)
    stops <- unlist(stops)
    if (length(starts) == 0) {
        starts <- NA_character_
    }
    if (length(stops) == 0) {
        stops <- NA_character_
    }
    data.frame(id = id,
               from = starts,
               till = stops)
}

aux <- lapply(infiles, parse_biogs_and_parties)
aux <- bind_rows(aux)

aux <- aux |>
    mutate(end_date = case_when(end_date == "present" ~ as.character(Sys.Date()),
                                TRUE ~ end_date),
           end_date = as.Date(end_date),
           start_date = as.Date(start_date))

aux2 <- lapply(infiles, parse_seats)
aux2 <- bind_rows(aux2)

aux2 <- aux2 |>
    mutate(from = as.Date(from),
           till = as.Date(till)) |>
    mutate(member = 1)

### Join first to get vitals
dat <- left_join(dat,
                 aux |>
                 dplyr::select(id, fullname, dob, gender) |>
                 distinct(),
                 by = join_by(opid == id))

### Join again to get parties
dat <- left_join(dat,
                 aux |>
                 dplyr::select(id, start_date, end_date, party_ref) |>
                 distinct(),
                 by = join_by(opid == id,
                              speechdate >= start_date,
                              speechdate <= end_date))

### Join again to get membership
dat <- left_join(dat,
                 aux2 |>
                 dplyr::select(id, from, till, member) |>
                 distinct(),
                 by = join_by(opid == id,
                              speechdate >= from,
                              speechdate <= till))

### We're going to restrict to stuff before the 2011 election
### Because the data quality after that is questionable
dat <- dat |>
    filter(speechdate < as.Date("2011-05-02"))

dat <- dat |>
    filter(!is.na(member))

### Calculated quantities
dat <- dat |>
    mutate(age = as.numeric(speechdate - dob) / 365.25,
           yrs_since_1945 = as.numeric(speechdate - as.Date("1945-01-01")) / 365.25)

### Individuals below 18
## 1 ca.m.9679                         FULTON,  James Ross, B.A.
## 2 ca.m.2518        FORTIN,  André-Gilles, B.A., B.Ped., Br.A.
## 3 ca.m.2771 VINCENT, The Hon. Pierre H., P.C., LL.B., M.Fisc.
## 4 ca.m.9759                              COOPER,  Albert Glen
## 5 ca.m.2111                  ROBINSON,  Svend Johannes, LL.B.
## 6 ca.m.3949        GREY,  Deborah C., P.C., O.C., B.A., B.Ed.
## 7 ca.m.6256        MARCHI, The Hon. Sergio, P.C., B.A.(Hons).

### Individuals above 100
## 1 ca.m.4492                                CHEVRIER, The Hon. Noé E. 1846-04-27
## 2 ca.m.2710                                          CHARLTON,  John 1829-02-03
## 3  ca.m.931                          MACLEAN,  William Findlay, B.A. 1854-08-10
## 4 ca.m.6144                           MARTIN, The Hon. Peter Francis 1856-01-13
## 5 ca.m.4601                         FARRELL, The Hon. Edward Matthew 1854-03-31
## 6 ca.m.8363 BROWN, The Hon. Albert Joseph, K.C., B.A., B.C.L., LL.D. 1861-07-08


### Include earliest start
dat <- dat |>
    group_by(opid) |>
    mutate(earliest_start = min(from))

### Drop some of the date variables
dat <- dat |>
    dplyr::select(opid, speechdate,
                  Present, Past, Future, nchars,
                  fullname, dob, gender,
                  party_ref,
                  earliest_start) |>
    as.data.frame()

dat <- dat |>
    mutate(opid = factor(opid),
           gender = factor(gender),
           party_ref = factor(party_ref))

### Handle parties
dat <- dat |>
    mutate(party_7cat = dplyr::recode(party_ref,
                                      "ca.p.liberal" = "liberal",
                                      "ca.p.progressiveconservative" = "progressiveconservative",
                                      "ca.p.newdemocraticparty" = "CCF/NDP",
                                      "ca.p.conservative" = "conservative",
                                      "ca.p.blocquebecois" = "blocquebecois",
                                      "ca.p.CCF" = "CCF/NDP",
                                      "ca.p.reform" = "reform",
                                      .default = "all_others"))

## spells in govt
govt_spells <- read.csv(here::here("data", "spells_in_govt.csv")) |>
    mutate(in_govt = 1) |>
    mutate(start_date = as.Date(start_date),
           end_date = as.Date(end_date))

dat <- left_join(dat,
                 govt_spells,
                 by = join_by(party_ref,
                              speechdate >= start_date,
                              speechdate <= end_date)) |>
    mutate(in_govt = coalesce(in_govt, 0L))

###
dat <- dat |>
    mutate(age = as.numeric(speechdate - dob) / 365.25,
           cohort = factor(earliest_start))

dat <- dat |>
    mutate(wday = wday(speechdate, label = TRUE),
           month = month(speechdate, label = TRUE),
           yrs_since_1945 = as.numeric(speechdate - as.Date("1945-01-01")) / 365.25)

saveRDS(dat, file = here::here("working", "model_data.rds"))
