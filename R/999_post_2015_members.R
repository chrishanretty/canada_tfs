library(tidyverse)
library(WikidataQueryServiceR)
library(reclin2)
here::i_am("R/999_post_2015_members.R")

### Get in helper functions
source(here::here("R", "00_helpers.R"), echo = FALSE)

infiles <- sort(list.files(here::here("data/"),
                           pattern = "*.csv",
                           recursive = TRUE,
                           full.names = TRUE))

infiles <- grep("//201", infiles, value = TRUE)
dat <- bind_rows(lapply(infiles, function(x) {
    read_csv(x) |> distinct(opid, speakername)
    }))

dat <- dat |>
    filter(!is.na(opid)) |>
    distinct()

### Work out which ones aren't present in the XML
xml_search <- paste0("ca.m.", dat$opid, ".xml")

test <- sapply(xml_search, function(f) file.exists(here::here("data/ca-members", f)))

dat <- dat[-test,]

### Now write this out
write.csv(dat, file = "post_2015_members.csv", row.names = FALSE)

### What's the Wikidata query?
qry <- "
SELECT DISTINCT ?item ?itemLabel ?born ?year ?month ?day ?sexLabel WHERE
{
 ?item wdt:P31 wd:Q5 ; p:P39 ?ps . ?ps ps:P39 ?term .
 ?term wdt:P279* wd:Q15964890 .      # Canadian member of the Commons
 ?item wdt:P569 ?born .              # with a known birthday
 BIND( YEAR(?born) AS ?year )          # identify the month of birth
 BIND( DAY(?born) AS ?day )          # identify the month of birth
 BIND( MONTH(?born) AS ?month )      # identify the month of birth
 OPTIONAL { ?item wdt:P21 ?sex . }
 SERVICE wikibase:label { bd:serviceParam wikibase:language 'en' }
}
"

qry.bak <- query_wikidata(qry)


#### Greedy matching
library(stringdist)
foo <- amatch(dat$speakername, qry.bak$itemLabel)
bar <- cbind(dat, qry.bak[foo,])
### write the distance between the two
bar$dist <- stringdist(bar$speakername, bar$itemLabel)
write.csv(bar, file = "post_2015_members.csv", row.names = FALSE)
## ## 
## pairs <- pair(dat, qry.bak)
## pairs <- compare_pairs(pairs, on = c("itemLabel"))

## m <- problink_em(~ itemLabel, data = pairs)
## pairs <- predict(m, pairs = pairs, add = TRUE)
## pairs <- select_n_to_m(pairs, "weights", variable = "ntom", threshold = 0)
## linked_data_set <- link(pairs, selection = "ntom")
