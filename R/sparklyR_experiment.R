library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local")
cars <- copy_to(sc, mtcars)
summarize_all(cars, mean, na.rm = T)

cars %>%
    mutate(transmission = ifelse(am == 0, "automatic", "manual")) %>%
    group_by(transmission) %>%
    summarise_all(mean)

summarise(cars, mpg_percentile = percentile(mpg, 0.25)) %>%
    show_query()

summarise(cars, mpg_percentile = percentile(mpg, array(0.25, 0.5, 0.75)))
ml_corr(cars)

library(corrr)
correlate(cars, use = "pairwise.complete.obs", method = "pearson")

correlate(cars, use = "pairwise.complete.obs", method = "pearson") %>%
    shave() %>%
    rplot()

library(ggplot2)
ggplot(aes(as.factor(cyl), mpg), data = mtcars) + geom_col()

car_group <- cars %>%
    group_by(cyl) %>%
    summarise(mpg = sum(mpg, na.rm = TRUE)) %>%
    collect() %>%
    print()

ggplot(aes(as.factor(cyl), mpg), data = car_group) +
    geom_col(fill = "#999999") + coord_flip()

library(dbplot)

cars %>%
    dbplot_histogram(mpg, binwidth = 3) +
    labs(title = "MPG Distribution",
         subtitle = "Histogram over miles per gallon") +
    theme_minimal()

# model

## linear
cars %>%
    ml_linear_regression(mpg ~ .) %>%
    summary()

## new recipe
cars %>%
    ml_linear_regression(mpg ~ hp + cyl) %>%
    summary()

## different model
cars %>%
    ml_generalized_linear_regression(mpg ~ hp + cyl) %>%
    summary()

# Cache -- compute()
cached_cars <- cars %>%
    mutate(cyl = paste0("cyl_", cyl)) %>%
    compute("cached_cars")



spark_disconnect(sc)
