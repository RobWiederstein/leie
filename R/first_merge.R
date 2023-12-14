# leie data
file <- "~/Dropbox/coding/pyproj/medicare_fraud/data/leie_oig_excluded.csv"
le <- vroom::vroom(file = file, .name_repair = ~tolower(.))
le["excluded"] <- 1

# part b 2020
file <- "~/Dropbox/coding/pyproj/medicare_fraud/data/MUP_PHY_R22_P05_V10_D20_Prov_Svc.csv"
pb <- vroom::vroom(file = file,
                   .name_repair = ~tolower(.),
                   col_types = "c"
                   )
# join by npi
library(dplyr)
pb1 <- dplyr::left_join(pb, le[c("npi", "excluded")],
                       by = join_by(rndrng_npi == npi),
                       relationship = "many-to-many"
)

#
table(pb1$excluded)
1953/9449372



