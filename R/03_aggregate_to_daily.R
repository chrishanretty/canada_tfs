library(tidyverse)
library(arrow)
library(doParallel)
library(foreach)
here::i_am("R/03_aggregate_to_daily.R")

infiles <- list.files(here::here("working/distilled"),
                      full.names = TRUE)

### written question headings
banned_headings <- c("J. D. M. QUESTIONS",
                     "QUESTION PASSED AS ORDER FOR RETURNS",
                     "QUESTIONS PASSED AS ORDERS FOR RETURNS",
                     "QUESTIONS",
                     "ROUTINE PROCEEDINGS",
                     "QUESTIONS ON THE ORDER PAPER")

parse_file <- function(i) {
    ## the_date <- sub(".*debates", "", i)
    ## the_date <- gsub("[^0-9]", "", the_date)
    ## the_date <- as.Date(the_date, format = "%Y%m%d")
### Aggregate to speaker/topic/da
    ## ignoring sents where there is no alphanumeric char.
    retval <- read_parquet(i) |>
        filter(grepl("[A-Za-z]", sents)) |>
        filter(!is.element(maintopic, banned_headings)) |>
        mutate(nchars = nchar(sents)) |>
        group_by(opid, speechdate) |>
        summarize(Present = weighted.mean(Present, nchars, na.rm = TRUE),
                  Past = weighted.mean(Past, nchars, na.rm = TRUE),
                  Future = weighted.mean(Future, nchars, na.rm = TRUE),
                  nchars = sum(nchars, na.rm = TRUE),
                  .groups = "drop") |>
        mutate(opid = as.character(opid)) |>
        as.data.frame() 
    return(retval)
}

num_cores <- 20
cl <- makeCluster(num_cores)
registerDoParallel(cl)
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(arrow))
Sys.time()
dat <- foreach(i=infiles,.combine = 'bind_rows') %dopar% {
    parse_file(i)
}
Sys.time()

stopCluster(cl)

dat <- as.data.frame(dat)

saveRDS(dat, file = here::here("outputs", "daily_speaker.rds"))
