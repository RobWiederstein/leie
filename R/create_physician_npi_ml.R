# libraries ----
library(forcats)
library(colorspace)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(skimr)
library(readr)
library(dplyr)
library(vroom)
library(scales)

file <- "./data-raw/leie_oig_excluded.csv"

## create features ----
leie <- vroom::vroom(file = file,
                     .name_repair = ~tolower(.),
                     n_max = Inf,
                     na = c("00000000", "0000000000", 0, ""),
                     col_types = paste0(rep("c", 18), collapse = ""),
                     show_col_types = F
) |>

    mutate(npi = as.character(npi)) |>
    mutate(excldate = as.Date(excldate, format = "%Y%m%d"),
           dob = as.Date(dob, format = "%Y%m%d"))  |>
    # keep only full years
    filter(excldate < as.Date("2023-01-01")) |>
    # drop obs that were reinstated (10ish)
    filter(is.na(reindate)) |>
    # drop obs that were waived (10ish)
    filter(is.na(waiverdate)) |>
    # set up factor for plot facet on business/individual
    mutate(type = case_when(is.na(lastname) ~ 1, TRUE ~ 0)) |>
    mutate(type = factor(type, labels = c("Individual", "Entity"))) |>
    # set up factor for plot facet permissive/mandatory
    mutate(exclusion = case_when(
        grepl("a", excltype) ~ "mandatory",
        grepl("b", excltype) ~ "permissive",
        TRUE ~ "other")) |>
    mutate(age_at_excl = year(excldate) - year(dob)) |>
    # unitemized statues
    filter(!(excltype %in% c("1128Aa", "BRCH CIA", "BRCH SA", "1160")))

leie_ml <-
    leie |>
    # only individuals
    filter(type == "Individual") |>
    filter(exclusion == "mandatory") |>
    filter(general %in% c("MEDICAL PRACTICE, MD",
                          "PHYSICIAN (MD, DO)")
           ) |>
    filter(!(specialty %in% c("TECHNICIAN",
                              "SOCIAL_WORKER",
                              "NURSE/NURSES AIDE",
                              "CLERK/OTHER CLERICAL",
                              "BUSINESS MANAGER",
                              "OWNER/OPERATOR",
                              "EMPLOYEE"))) |>
    select(general, specialty, upin, npi, dob, city, state,
           zip, excltype, excldate, age_at_excl) |>
    mutate(npi_or_upin = ifelse(is.na(npi) & is.na(upin), 0, 1)) |>
    select(!c(npi, upin, general)) |>
    mutate(identifier = 1:nrow(leie_ml), .before = specialty)
table(leie_ml$npi_or_upin)

write_csv(leie_ml, file = "./data/leie_physicians_id_vip.csv")
library(randomForest)
library(tidyverse)


