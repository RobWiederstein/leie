library(sparklyr)
library(tidyverse)

sc <- spark_connect(master = "local")

mtcars_tbl <- copy_to(sc, mtcars, overwrite = TRUE)

partitions <- mtcars_tbl %>%
    select(mpg, wt, cyl) %>%
    sdf_random_split(training = 0.5, test = 0.5, seed = 1099)

fit <- partitions$training %>%
    ml_linear_regression(mpg ~ .)

fit
summary(fit)
pred <- ml_predict(fit, partitions$test)
pred
spark_web(sc)
spark_disconnect(sc)
