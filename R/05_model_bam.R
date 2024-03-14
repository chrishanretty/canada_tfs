library(tidyverse)
library(mgcv)
here::i_am("R/05_model_bam.R")

dat <- readRDS(file = here::here("working", "model_data.rds"))

dat <- dat |>
    mutate(month = factor(month, ordered = FALSE),
           wday = factor(wday, ordered = FALSE))

dat <- dat |>
    ungroup() |>
    mutate(w8 = nchars / mean(nchars))

### This gives reasonableish CIs in a linear model
f <- Future ~ s(age, bs = "cr", k = 20) +
    s(yrs_since_1945, bs = "cr", k = 30) +
    s(party_7cat, bs = "re") +
    s(cohort, bs = "re") + 
    s(opid, bs = "re") +
    in_govt +
    wday + month 

system.time(mod <- bam(f,
           data = dat,
           weights = dat$w8,
           family = betar(),
           discrete = TRUE,
           threads = 20
           ))

saveRDS(mod, file = here::here("working", "canada_bam_mod.rds"))

s <- summary(mod)

pdf(file = "~/Desktop/canada_plot.pdf")
plot(mod, select = 1)
dev.off()
