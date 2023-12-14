library(sparklyr)
library(dplyr)
# configure
conf <- spark_config()
conf$`sparklyr.cores.local` <- 8
conf$`sparklyr.shell.driver-memory` <- "16G"
conf$spark.memory.fraction <- 0.9

# create connection
sc <- spark_connect(master = "local", version = "3.4", config = conf)

ptb_2020_tbl <- spark_read_csv(sc, name = "ratings", 's3://cmspartb/partb/Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2021.csv')

library(aws)
data <-
    aws.s3::s3read_using(read.csv, object = "s3://your_bucketname/your_object_name.csv.gz")

# ptb ----
## import ptb ----
##
filename <- "Medicare_Physician_Other_Practitioners_by_Provider_and_Service_"
ptb_21 <- spark_read_csv(sc, name = "ptb_21_tbl",  path = paste0("./data-raw/cms_ptb/", filename, "2021.csv"))
ptb_20 <- spark_read_csv(sc, name = "ptb_20_tbl",  path = paste0("./data-raw/cms_ptb/", filename, "2020.csv"))
ptb_19 <- spark_read_csv(sc, name = "ptb_19_tbl",  path = paste0("./data-raw/cms_ptb/", filename, "2019.csv"))


## stack ptb ---
ptb_3y <- sdf_bind_rows(ptb_21, ptb_20, ptb_19)

## clean ----
ptb_clean <-
    ptb_3y |>
    rename_with(~tolower(.x)) |>
    #filter(rndrng_prvdr_state_abrvtn %in% c("CA", "TX", "NY", "FL")) |>
    filter(rndrng_prvdr_mdcr_prtcptg_ind == "Y") |>
    filter(rndrng_prvdr_type == "Internal Medicine") |>
    select(rndrng_npi,
           rndrng_prvdr_type,
           rndrng_prvdr_gndr,
           rndrng_prvdr_state_abrvtn,
           rndrng_prvdr_zip5,
           rndrng_prvdr_ruca,
           hcpcs_cd,
           tot_srvcs,
           tot_benes
    ) |>
    mutate(srvcs_per_bene = tot_srvcs/tot_benes) |>
    select(!c(tot_srvcs, tot_benes))

## pivot ----
ptb <-
    sdf_pivot(ptb_clean,
              rndrng_npi +
              rndrng_prvdr_type +
              rndrng_prvdr_gndr +
              rndrng_prvdr_state_abrvtn +
              rndrng_prvdr_zip5 +
              rndrng_prvdr_ruca ~
              hcpcs_cd,
              # median?
              fun.aggregate = list(srvcs_per_bene = "mean")
              )

# leie ----
## import ----
leie <- spark_read_csv(sc, name = "leie_tbl",  path = "./data/leie_revised.csv")
## clean ----
leie1 <-
    leie |>
    select(npi) |>
    filter(npi != "NA") |>
    mutate(fraud = "true") |>
    rename(rndrng_npi = npi)
# merge ptb + leie
df <- dplyr::left_join(ptb,  leie1, by = "rndrng_npi") |>
      mutate(fraud = ifelse(is.na(fraud), "false", "true"))

# check fraud true/false
    df |>
    select(fraud) |>
    group_by(fraud) |>
    summarize(n = n())
# check for distinct values
    str(df %>% head(1) %>% collect())
    sdf_describe(df)
# Model ----

## partition ----

glm <-
    df |>
    ml_logistic_regression(fraud ~ .
                           - rndrng_prvdr_type)

summary(glm)

filename <-
    paste0(
    "https://cmspartb.s3.us-west-2.amazonaws.com/Medicare_Physician_Other_",
    "Practitioners_by_Provider_and_Service_2021.csv"
    )
