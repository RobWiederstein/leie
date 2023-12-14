# leie data ----
## libraries ----
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
library(randomForest)
## import ----
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

write.csv(leie,
          file = "./data/leie_revised.csv",
          row.names = F)
## table npi completion rate all years = 8.74% ----
leie_summary <-
    skimr::skim(leie) |>
    focus(skim_variable, skim_type, n_missing, complete_rate,
          numeric.mean, Date.n_unique, character.n_unique)|>
    tibble::as_tibble()
leie_summary
readr::write_csv(leie_summary, file = "./data/leie_summary_table.csv")

## table count instances of statutory exclusion ----
statutory_excl_n_count <-
    leie |>
    select(excltype) |>
    group_by(excltype) |>
    summarize(n_count = n()) |>
    rename(statute = excltype)

statutory_excl <- read.csv(file = "./data/statutory_exclusion.csv")
statutory_exclusion_w_count <-
    left_join(statutory_excl_n_count,
              statutory_excl,
              by = "statute"
    ) |>
    arrange(desc(n_count)) |>
    select(statute, agency:description, n_count)
statutory_exclusion_w_count
write_csv(statutory_exclusion_w_count,
          file = "./data/statutory_exclusion_w_count.csv"
          )
## plot year and type ----
leie_year_type <-
    leie |>
    group_by(year = lubridate::floor_date(excldate, "year"),
             type) |>
    summarize(value = n(),
              .groups = "drop")
### plot ----
colors = qualitative_hcl(2, palette = "harmonic")
    leie_year_type |>
    ggplot() +
    aes(year, value, group = type, color = type) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    scale_color_manual("Type", values = colors) +
    theme_minimal() +
    scale_x_date(name = "") +
    scale_y_continuous(name = "",
                       labels = label_comma()) +
    labs(title = "LEIE Exclusions by Type",
         subtitle = "1979 - 2023",
         caption = "CMS Office of Inspector General")
### save ----
filename <- "./plots/leie_exclusions_by_type.jpg"
ggsave(filename = filename,
       dpi = 600,
       width = 9,
       height = 5,
       units = "in",
       )
## plot pct npis vs upins ----
### calculate----
leie_missing <-
    leie |>
    select(excldate, npi, upin) |>
    group_by(year = lubridate::floor_date(excldate, "year")) |>
    summarize(n_npi = sum(!is.na(npi)),
              n_upin = sum(!is.na(upin)),
              n_obs = n()) |>
    mutate(pct_npi = n_npi / n_obs,
           pct_npi = pct_npi * 100,
           pct_upin = n_upin / n_obs,
           pct_upin = pct_upin * 100) |>
    select(year, pct_npi, pct_upin) |>
    pivot_longer(cols = pct_npi:pct_upin,
                 names_to = "identifier",
                 values_to = "percent",
                 names_prefix = "pct_") |>
    mutate(identifier = factor(identifier))

### plot ----
colors = qualitative_hcl(2, palette = "harmonic")
leie_missing |>
    ggplot() +
    aes(year, percent, group = identifier, color = identifier) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    scale_y_continuous(name = "",
                       breaks = c(0, 10, 20, 30, 40),
                       labels = paste0(seq(0, 40, by = 10), "%")) +
    scale_color_manual("Identifier", values = colors) +
    scale_x_date(name = "") +
    theme_minimal() +

    labs(title = "LEIE Missing Identifiers by Year",
         caption = "CMS Office of Inspector General"
         )
### save ----
ggsave(filename = "./plots/leie_missing_identifiers_by_year.jpg",
       width = 9,
       height = 5,
       units = "in",
       dpi = 600)

## plot by year type ----
### calculate ----
leie_year_type_excl <-
    leie |>
    mutate(exclusion = factor(exclusion)) |>
    group_by(year = lubridate::floor_date(excldate, "year"),
             type,
             exclusion) |>
    summarize(exclusions = n(),
              .groups = "drop")
### plot ----
colors = qualitative_hcl(3, palette = "harmonic")
leie_year_type_excl |>
    ggplot() +
    aes(x = year, y = exclusions, group = exclusion, color = exclusion) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    scale_color_manual("Exclusion", values = colors) +
    facet_wrap(vars(type), nrow = 2) +
    theme_minimal() +
    scale_y_continuous(name = "",
                       labels = label_comma()) +
    scale_x_date(name = "") +
    labs(title = "LEIE Exclusions by Category",
         subtitle = "1978 - 2022",
         caption = "CMS Office of Inspector General")
### save ----
ggsave("./plots/leie_exclusions_by_category.jpg",
       width = 9,
       height = 5,
       dpi = 600,
       units = "in")

## plot by year and statutory exclusion ----
### calculate ----
leie_2 <-
    leie |>
    mutate(excltype = fct_lump(excltype, 5)) |>
    filter(type == "Individual") |>
    group_by(year = lubridate::floor_date(excldate, "year"),
             excltype) |>
    summarize(exclusions = n(),
              .groups = "drop")
### plot ----
colors <- qualitative_hcl(6, palette = "harmonic")
leie_2 |>
    ggplot() +
    aes(year, exclusions, group = excltype, color = excltype) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    scale_color_manual("Statute", values = colors) +
    scale_x_date(name = "") +
    scale_y_continuous(name = "",
                       labels = label_comma()) +
    theme_minimal() +
    labs(title = "Individuals Excluded by Statute Provision",
         subtitle = "1978 - 2022",
         caption = "CMS Office of Inspector General.")

### save ----
ggsave(filename = "./plots/individuals_excluded_by_statute_provision.jpg",
    width = 9,
    height = 5,
    dpi = 600,
    units = "in"
)

## tile map of statutory excl by general -----
### calculate ----
leie_3 <-
    leie |>
    #mutate(general = fct_lump_lowfreq(general)) |>
    #mutate(excltype = fct_lump_lowfreq(excltype)) |>
    select(general, excltype, npi) |>
    group_by(general, excltype) |>
    summarize(value = n(),
              .groups = "drop")

## plot heatmap ----
leie_3_hm <-
    leie_3 |>
    mutate(excltype = as.character(excltype)) |>
    pivot_wider(names_from = excltype)|>
    mutate(across(everything(), ~replace_na(.x, 0)))
jpeg(filename = "./plots/hm_leie_excl_to_general.jpg",
     width = 7,
     height = 7,
     units = "in",
     quality = 95,
     res = 600)
heatmap(
    as.matrix(leie_3_hm[, -1]),
    scale = "col",
    labRow = leie_3_hm$general,
    labCol = colnames(leie_3_hm[, -1]),
    keep.dendro = T,
    Colv = ,
    Rowv = ,
    na.rm = T,
    margins = c(5, 8),
    main = "LEIE Statutory Exclusion to General"
)
dev.off()
## heatmap of statutory excl by state -----
### calculate ----
leie_4 <-
    leie |>
    select(state, excltype) |>
    group_by(state, excltype) |>
    summarize(count = n())
### plot ----
leie_4_hm <-
    leie_4 |>
    pivot_wider(id_cols = state,
                names_from = excltype,
                values_from = count
    )

leie_4_hm[, -1][is.na(leie_4_hm[, -1])] <- 0

jpeg(filename = "./plots/hm_by_excl_state.jpg",
     width = 7,
     height = 7,
     units = "in",
     res = 600,
     quality = 95,
     )
heatmap(
    as.matrix(leie_4_hm[, -1]),
    scale = "column",
    labRow = leie_4_hm$state,
    labCol = colnames(leie_4_hm[, -1]),
    keep.dendro = TRUE,
    Colv = ,
    Rowv = ,
    na.rm = T,
    main = "LEIE Statutory Exclusion to State"
    )
dev.off()
## plot by year and state missing npi ----
library(lubridate)
leie_by_state_year <-
    leie |>
    mutate(year = year(excldate)) |>
    select(state, year, npi) |>
    group_by(year, state) |>
    summarize(missing = sum(is.na(npi))) |>
    ungroup() |>
    pivot_wider(names_from = state,
                values_from = missing) |>
    mutate(across(everything(), ~ifelse(is.na(.x), 0, .x))) |>
    select(year, all_of(state.abb))
### plot ----
jpeg(
    filename = "./plots/hm_by_state_year.jpg",
    quality = 95,
    res = 600,
    width = 7,
    height = 7,
    pointsize = 12,
    units = "in"
)
heatmap(
    as.matrix(leie_by_state_year[, -1]),
    scale = "col",
    labRow = leie_by_state_year$year,
    labCol = colnames(leie_by_state_year[, -1]),
    Colv = NA,
    Rowv = NA,
    keep.dendro = F,
    na.rm = T
)
dev.off()
## dotplots counts by profession and exclusion
leie_general <-
    leie |>
    filter(type == "Individual") |>
    select(general) |>
    group_by(general) |>
    summarize(n = n()) |>
    mutate(n_pct = n/sum(n)) |>
    filter(n_pct > .01) |>
    arrange(desc(n)) |>
    mutate(order = 1:18) |>
    mutate(general = factor(general),
           general = fct_reorder(general, n_pct ))
colors <- qualitative_hcl(n = 1, palette = "harmonic")
    leie_general |>
    ggplot() +
    aes(n_pct, general) |>
    geom_point(size = 2, color = colors) +
    scale_color_manual(values = colors) +
    scale_x_continuous(name = "",
                       label = label_percent()) +
    scale_y_discrete(name = "") +
    theme_minimal() +
    labs(title = "Exclusions by General Category",
         subtitle = "1978 - 2022",
         caption = "CMS Office of Inspector General")
### save ----
ggsave(filename = "./plots/leie_exclusions_by_gen_categ.jpg",
       width = 5,
       height = 5,
       units = "in",
       dpi = 600
       )
## LEIE Exclusion by M.D. Specialty ----
### calculate ----
other_jobs <- c("NURSE/NURSES AIDE", "EMPLOYEE", "TECHNICIAN",
                   "BUSINESS MANAGER", "CLERK/OTHER CLERICAL",
                   "SOCIAL WORKER", NA)
leie_md_specialty <-
        leie |>
        filter(type == "Individual") |>
        filter(general == "MEDICAL PRACTICE, MD") |>
        filter(!(specialty %in% other_jobs)) |>
        mutate(specialty = gsub("GENERAL PRACTICE/FP", "GENERAL PRACTICE", specialty)) |>
        select(specialty) |>
        group_by(specialty) |>
        summarize(n = n()) |>
        mutate(n_pct = n/sum(n)) |>
        arrange(desc(n)) |>
        mutate(specialty = factor(specialty),
               specialty = fct_reorder(specialty, n_pct )) |>
        slice_head(n = 20)
### plot ----
colors <- qualitative_hcl(n = 1, palette = "harmonic")
leie_md_specialty |>
    ggplot() +
    aes(n_pct, specialty) |>
    geom_point(size = 2, color = colors) +
    scale_color_manual(values = colors) +
    scale_x_continuous(name = "",
                       label = label_percent()) +
    scale_y_discrete(name = "") +
    theme_minimal() +
    labs(title = "LEIE Physician Exclusions by Specialty",
         subtitle = "Top 20",
         caption = "CMS Office of Inspector General")
### save ----
ggsave(filename = "./plots/leie_physican_excl_by_specialty.jpg",
       width = 6,
       height = 6,
       units = "in",
       dpi = 600
       )
## plot M.D. exclusions ----
### calculate ----
leie_md_excl_type <-
    leie |>
    filter(type == "Individual") |>
    filter(general %in% c("MEDICAL PRACTICE, MD",
                          "PHYSICIAN (MD, DO)")) |>
    group_by(year = lubridate::floor_date(excldate, "year"),
             exclusion,
             general) |>
    summarize(value = n(),
              .groups = "drop")
### plot ----
colors <- qualitative_hcl(3, palette = "harmonic")
leie_md_excl_type |>
    ggplot() +
    aes(year, value, group = exclusion, color = exclusion) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    facet_wrap(vars(general), nrow = 1) +
    scale_color_manual("Exclusion", values = colors) +
    scale_x_date(name = "") +
    scale_y_continuous(name = "") +
    theme_minimal() +
    labs(title = "LEIE Exclusions by Type & Category",
         subtitle = "1978 - 2023",
         caption = "CMS Office of Inspector General")

### save ----
filename <- "./plots/leie_exclusions_type_and_category.jpg"
ggsave(filename = filename,
       dpi = 600,
       width = 9,
       height = 5,
       unit = "in")

## heatmap of physician specialties ----
### calculate ----
leie_md_excl_specialist <-
    leie |>
    filter(type == "Individual") |>
    filter(general %in% c("MEDICAL PRACTICE, MD",
                          "PHYSICIAN (MD, DO)")) |>
    filter(!(specialty %in% c("TECHNICIAN",
                              "SOCIAL_WORKER",
                              "NURSE/NURSES AIDE",
                              "CLERK/OTHER CLERICAL",
                              "BUSINESS MANAGER",
                              "OWNER/OPERATOR",
                              "EMPLOYEE"))) |>
    filter(!is.na(specialty)) |>
    group_by(excltype,
             specialty) |>
    summarize(value = n(),
              .groups = "drop") |>
    pivot_wider(names_from = excltype) |>
    mutate(across(!specialty, ~replace_na(.x, 0)))
### plot ----
jpeg(filename = "./plots/hm_physician_specialty_by_excl.jpg",
     width = 7,
     height = 7,
     quality = 95,
     units = "in",
     res = 600)
heatmap(
    as.matrix(leie_md_excl_specialist[, -1]),
    scale = "col",
    labRow = leie_md_excl_specialist$specialty,
    labCol = colnames(leie_md_excl_specialist[, -1]),
    Colv = ,
    Rowv = ,
    keep.dendro = T,
    na.rm = T,
    main = "LEIE Statutory Exclusion to Physican Specialty",
    margins = c(5, 9)
)
dev.off()
## plot Medical Practice md vs Physician (MD, OD) ----
leie_prac_dr <-
leie |>
    filter(general %in% c("MEDICAL PRACTICE, MD",
                            "PHYSICIAN (MD, DO)")) |>
    group_by(year = lubridate::floor_date(excldate, "year"),
            general) |>
    summarize(n = n(),
              .groups = "drop")
## plot ----
colors <- qualitative_hcl(n = 2, palette = "harmonic")
leie_prac_dr |>
    ggplot() +
    aes(year, n, group = general, color = general) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    scale_color_manual("General Category", values = colors) +
    theme_minimal() +
    scale_y_continuous(name = "") +
    scale_x_date(name = "") +
    labs(title = "LEIE Excluded Physicians by General Category",
         subtitle = "1978 - 2022",
         caption = "CMS Office of Inspector General")
### save ----
ggsave(filename = "./plots/leie_excluded_phys_by_category.jpg",
       width = 9,
       height = 5,
       dpi = 600,
       units = "in"
       )
## plot age
leie |>
    select(age_at_excl, excltype) |>
    mutate(excltype = factor(excltype)) |>
    mutate(excltype = fct_reorder(excltype, age_at_excl,
                                  .fun = median)) |>
    ggplot() +
    aes(age_at_excl, excltype) +
    geom_boxplot() +
    theme_minimal()
## plot variable importance ----
file = "./data/leie_physicians_id_vip.csv"
leie_ml <- read_csv(file = file)

### Random forest model ----
leie_ml <-
    leie_ml |>
    select(!identifier) |>
    drop_na() |>
    mutate(npi_or_upin = factor(npi_or_upin, labels = c("missing", "present")))
leie_ml_rf <- randomForest(npi_or_upin ~ ., data=leie_ml, importance=TRUE)
imp = as.data.frame(importance(leie_ml_rf))
imp$vars <- row.names(imp)
row.names(imp) <- NULL
### plot ----
colors <- qualitative_hcl(2, palette = "harmonic")
imp |>
    mutate(vars = fct_reorder(vars, MeanDecreaseAccuracy)) |>
    pivot_longer(cols=matches("Mean")) %>%
    ggplot(aes(value, vars, group = name, color = name)) +
    geom_point() +
    scale_color_manual(values = colors) +
    facet_grid(. ~ name) +
    scale_x_continuous(name = "", limits=c(0,NA), expand=expansion(c(0,0.04))) +
    theme_minimal() +
    theme(legend.position="none") +
    scale_y_discrete(name = "") +
    labs(title = "LEIE Random Forest Variable Importance Plot",
         caption = "CMS Office of Inspector General")
### save ----
ggsave(filename = "./plots/variable_importance_plot.jpg",
       width = 9,
       height = 5,
       units = "in",
       dpi = 600)


leie_missing_by_state <-
    leie |>
    filter(excldate > "2000-01-01") |>
    filter(!(state %in% c("DC", "VI", "XX", "FM", "GU", "MP", "PR", "AE"))) |>
    drop_na(state) |>
    group_by(year = lubridate::floor_date(excldate, "year"),
             state) |>
    summarize(obs = n(),
              missing = sum(is.na(npi)),
              pct = ((obs - missing)/obs),
              .groups = "drop") |>
leie_missing_by_state |>
    ggplot() +
    aes(year, pct) +
    geom_line() +
    facet_wrap(state ~ .) +
    theme_minimal()
# create month column and group exclusions by month
leie_monthly_totals <-
    leie |>
    mutate(month = month(excldate)) |>
    group_by(month) |>
    summarize(count = n()/46) |>
    mutate(month = factor(month, labels = month.abb))
leie_monthly_avg = mean(leie_monthly_totals$count)
colors = qualitative_hcl(1, palette = "harmonic")
leie_monthly_totals |>
    ggplot() +
    aes(month, count) +
    geom_col(size = 3, fill = colors) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "",
                       labels = label_comma(),
                       limits = c(0, 300)) +
    geom_hline(yintercept = leie_monthly_avg,
               linetype = "dashed") +
    theme_minimal() +
    labs(title = "LEIE Average Monthly Exclusions",
         subtitle = "1977 - 2022",
         caption = "CMS Office of Inspector General"
    )
ggsave(filename = "./plots/leie_avg_monthly_excl.jpg",
       width = 4,
       height = 4,
       dpi = 600,
       units = "in"
       )
leie_top_10_by_statute <-
    leie |>
    select(excltype) |>
    group_by(excltype) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    slice_head(n = 10)

colors = qualitative_hcl(1, palette = "harmonic")
leie_top_10_by_statute |>
    mutate(excltype = fct_reorder(excltype, n)) |>
    ggplot() +
    aes(n, excltype) +
    geom_point(size = 2.5, color = colors) +
    scale_y_discrete(name = "") +
    scale_x_continuous(name = "", label = scales::label_comma()) +
    theme_minimal() +
    labs(title = "LEIE Exclusions by Statutory Provision",
         subtitle = "1977 - 2022",
         caption = "CMS Office of Inspector General")
ggsave(filename = "./plots/leie_exclusions_by_statutorty_provision.jpg",
       width = 5,
       height = 5,
       dpi = 600,
       units = "in")

leie_by_state_missing_num <-
    leie |>
    filter(excldate > "2021-01-01") |>
    filter(!(state %in% c("DC", "PR"))) |>
    select(state, npi, excldate) |>
    group_by(state) |>
    summarize(n_npi = (sum(!is.na(npi))),
                                n_obs = n()) |>
    mutate(pct_npi = n_npi / n_obs,
           pct_excl = n_obs / sum(n_obs))

leie_by_state_missing_num |>
    ggplot() +
    aes(pct_npi, pct_excl, label = state) +
    geom_text(check_overlap = T) +
    scale_y_log10(name = "excluded (log 10)",
                  label = scales::label_percent()) +
    scale_x_continuous(name = "NPIs",
                  label = scales::label_percent()) +
    theme_minimal() +
    labs(title = '2022 Excluded(%) and Missing(%) by State',
         caption = "CMS Office of Inspector General")
ggsave("./plots/leie_2022_pct_missing_pct_excluded.jpg",
       dpi = 600,
       width = 5,
       height = 5,
       unit = "in")
## plot california hhs excluded list ----
### import ----
ca <- readr::read_csv("./data-raw/ca_hhs_excluded_list.csv",
                      name_repair = ~janitor::make_clean_names(.x),
                      na = "N/A") |>
    mutate(across(date_of_suspension, ~as.Date(.x, format = "%m/%d/%Y"))) |>
    rename(dba = a_k_a_also_known_as_d_b_a_doing_business_as)
### calculate ----
ca_tot <-
    ca |>
    select(date_of_suspension) |>
    mutate(year = lubridate::floor_date(date_of_suspension, "year")) |>
    group_by(year) |>
    summarize(n = n()) |>
    filter(year >= as.Date("1972-01-01")) |>
    mutate(agency = "ca_hhs") |>
    print()
### combine ----
leie_tot <-
    leie |>
    filter(state == "CA") |>
    select(excldate) |>
    mutate(year = lubridate::floor_date(excldate, "year")) |>
    group_by(year) |>
    summarize(n = n()) |>
    mutate(agency = "cms_oig") |>
    print()
### plot ----
ca_us_tot <- dplyr::bind_rows(ca_tot, leie_tot)
colors <- qualitative_hcl(2, palette = "harmonic")
ca_us_tot |>
    ggplot() +
    aes(year, n , group = agency, color = agency) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    theme_minimal() +
    scale_color_manual(values = colors, name = "Agency")+
    scale_x_date(name = "") +
    scale_y_continuous(name = "",
                       label = scales::label_comma()) +
    labs(title = "Provider Exclusion Lists Compared",
         subtitle = "CMS OIG vs CA HHS",
         caption = "Source: CMS OIG; CA HHS")
ggsave("./plots/us_ca_exclusion_lists_compared.jpg",
       units = "in",
       height = 5,
       width = 9,
       dpi = 600
)
### plot nys office office of medicaid inspector ----
path <- "./data-raw/ny_omig_exclusions_list.xlsx"
ny <- readxl::read_xlsx(path = path) |>
    mutate(across(exclusion_effective_date, ~as.Date(.x, format = "%m/%d/%Y"))) |>
    rename(excldate = exclusion_effective_date)

ny_tot <-
    ny |>
    select(excldate) |>
    mutate(year = lubridate::floor_date(excldate, "year")) |>
    group_by(year) |>
    summarize(n = n()) |>
    mutate(agency = "ny_omig")

ny_us_tot <- dplyr::bind_rows(ny_tot, leie_tot)

colors <- qualitative_hcl(2, palette = "harmonic")
ny_us_tot |>
    ggplot() +
    aes(year, n, group = agency, color = agency) +
    scale_color_manual("Agency", values = colors) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    scale_y_continuous(name = "") +
    scale_x_date(name = "") +
    theme_minimal() +
    labs(title = "Provider Exclusion Lists Compared",
         subtitle = "CMS OIG vs NY OMIG",
         caption = "Source: CMS OIG; NY OMIG")

ggsave("./plots/us_ny_exclusion_lists_compared.jpg",
       width = 9,
       height = 5,
       dpi = 600,
       units = "in"
       )

## plot tx to us
tx <- readxl::read_xls(path = "./data-raw/tx_oig_excluded_list.xls")
tx_tot <-
    tx |>
    select(StartDate) |>
    group_by(year = floor_date(StartDate, "year")) |>
    summarize(n = n()) |>
    mutate(agency = "tx_oig")

us_tx_tot <- rbind(leie_tot, tx_tot)

colors <- qualitative_hcl(2, palette = "harmonic")

us_tx_tot |>
    ggplot() +
    aes(year, n, group = agency, color = agency) +
    geom_line(lwd = .75) +
    geom_point(size = 1) +
    scale_color_manual("Agency", values = colors) +
    scale_x_date(name = "") +
    scale_y_continuous(name = "") +
    theme_minimal() +
    labs(title = "Provider Exclusion Lists Compared",
         subtitle = "CMS OIG vs TX OIG",
         caption = "Source: CMS OIG; TX OIG")
ggsave("./plots/us_tx_exclusion_lists_compared.jpg",
       width = 9,
       height = 5,
       dpi = 600,
       units = "in"
)
