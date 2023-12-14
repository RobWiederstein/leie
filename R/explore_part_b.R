file <- "./data-raw/2020_partB_claims.csv"
df <- vroom::vroom(file = file,
                   .name_repair = ~janitor::make_clean_names(.x))
library(dplyr)
df  |>
    select(rndrng_npi,
           rndrng_prvdr_type,
           hcpcs_cd,
           rndrng_prvdr_state_abrvtn,
           tot_srvcs) |>
    group_by(rndrng_prvdr_type,
             rndrng_prvdr_state_abrvtn) |>
    summarize(hcpcs_billed = n_distinct(hcpcs_cd)) |>
    filter(rndrng_prvdr_state_abrvtn %in% c("CA")) |>
    arrange(desc(hcpcs_billed))
    print(n = 25)
