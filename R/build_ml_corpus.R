# libraries ----
library(sparklyr)
library(vroom)
library(dplyr)
library(lubridate)

file <- "./data-raw/Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2021.csv"

## create features ----
col_select <- c("rndrng_npi", "rndrng_prvdr_type",
                "place_of_srvc", "hcpcs_cd", "tot_srvcs")
ptb <- vroom::vroom(
    file = file,
    .name_repair = ~tolower(.),
    n_max = Inf,
    col_types = "ccccii",
    col_select = all_of(col_select),
    show_col_types = F)



# leie
library(vroom)
file <- "./data-raw/leie_oig_excluded.csv"
leie <- vroom::vroom(
    file = file,
    .name_repair = ~tolower(.),
    n_max = Inf,
    na = c("00000000", "0000000000", 0, ""),
    col_select = c(npi, excldate, excltype),
    show_col_types = F
) |>
    mutate(excldate = as.Date(as.character(excldate), format = "%Y%m%d")) |>
    #filter(excldate < "2021-12-31") |>
    # drop unknown exclusion types
    filter(!(excltype %in% c("1128Aa", "BRCH CIA", "BRCH SA", "1160"))) |>
    # only mandatory excl
    filter(grepl("a", excltype)) |>
    # must have an npi to match
    mutate(npi = as.character(npi)) |>
    drop_na(npi) |>
    # create lable
    mutate(fraud = "yes") |>
    rename(rndrng_npi = npi) |>
    select(rndrng_npi, fraud)
# michigan
x <- dplyr::left_join(ptb, leie, by = join_by(rndrng_npi))
x <- x |>
    mutate(fraud = ifelse(is.na(fraud), "no", "yes"))
x$fraud |> table()

476/9886177 * 100
#x <- x |>
#   mutate(fraud = ifelse(is.na(fraud), "no", "yes"))
table(x$fraud)

# flatten
ptb_sof <-
    x |>
    drop_na() |>
    pivot_wider(names_from = c(hcpcs_cd, place_of_srvc), values_from = tot_srvcs)
ptb_sof
