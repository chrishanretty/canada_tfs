### PURPOSE OF THIS CODE: to take in the CSVs from the LIPAD
### project, and turn this into R data frames which include
### information on the speaker, the member, the date, the heading, and
### what they actually said, filtering out procedural crap

### Load libraries
library(tidyverse)
library(tidytext)
library(rvest)
library(parallel)
library(xml2)
library(furrr)
library(jsonlite)

num_cores_to_use <- 20

here::i_am("R/01_tidy_files.R")

### Get in helper functions
source(here::here("R", "00_helpers.R"), echo = FALSE)

tidy_file <- function(file, overwrite = FALSE, debug = FALSE) {
    require(tidyverse)
    require(tidytext)
    require(stringr)

    first_part_of_filename <- sub("lipad/data/.*", "lipad/data/", file)
    second_part_of_filename <- sub(".*lipad/data/", "", file)
    first_part_of_filename <- sub("data/", "working/debates/", first_part_of_filename)
    second_part_of_filename <- gsub("/", "_", second_part_of_filename)
    
    outfile <- paste0(first_part_of_filename,
                      second_part_of_filename)

    outfile <- sub(".csv", ".rds", outfile)

### if overwrite is false, and the file exists
    if (!overwrite) { 
        if (file.exists(outfile)) return(TRUE)
    }
    
    x <- read.csv(file)


### Restrict just to speeches with an opid, not by speaker
    x <- x |>
        filter(!is.na(opid)) |>
        filter(speakerposition != "Speaker of the House of Commons") |>
        filter(speakerposition != "stagedirection") |>
        filter(speakerposition != "subtopic") |>
        filter(speakerposition != "topic")

    ### write it out

### select the columns we want
    x <- x |>
        dplyr::select(speechdate, opid, maintopic, subtopic, subsubtopic,
                      speechtext)

### Replace abbreviations
    x <- x |>
        mutate(speechtext = sub("Mr. ", "Mr ", speechtext),
               speechtext = sub("Dr. ", "Dr ", speechtext),
               speechtext = sub("Mrs. ", "Mrs ", speechtext),
               speechtext = sub("Ms. ", "Ms ", speechtext),
               speechtext = sub("Prof. ", "Prof ", speechtext),
               speechtext = sub("Cpt. ", "Captain ", speechtext),
               speechtext = sub("Hon. ", "Honourable ", speechtext),
               speechtext = sub("hon. ", "honourable ", speechtext),
               speechtext = sub("Cpl. ", "Corporal ", speechtext))
               
    x <- x |>
        unnest_tokens(output = sents,
                      speechtext,
                      token = "sentences",
                      to_lower = FALSE)
    
   
    if (nrow(x) == 0) {
        warning(paste0("No speeches in file ", file))
        return(FALSE)
    }
    
    if (debug) {
        print(x |>
              sample_n(10) |>
              pull(sents))
    } else { 
        x <- x |>
            mutate(docid = seq_len(n()))
        saveRDS(x, outfile)
    }
    return(TRUE)
}


infiles <- sort(list.files(here::here("data/"),
                           pattern = "*.csv",
                           recursive = TRUE,
                           full.names = TRUE))
length(infiles)

### Shuffle the files
infiles <- sample(infiles, length(infiles), replace = FALSE)

           
## ## # Initiate cluster
cl <- makeCluster(num_cores_to_use)
clusterEvalQ(cl, library(jsonlite))
clusterEvalQ(cl, here::i_am("R/01_tidy_files.R"))
clusterEvalQ(cl, source(here::here("R", "00_helpers.R")))

### takes about five minutes
Sys.time()
res <- parLapply(cl,
                 X = infiles,
                 fun = tidy_file,
                 overwrite = FALSE,
                 debug = FALSE)
Sys.time()

stopCluster(cl)
