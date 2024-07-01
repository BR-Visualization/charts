## code to prepare `scatterplot` dataset goes here

set.seed(1234)
b1 <- rnorm(500, 248 / 500, 0.1)
b2 <- rnorm(500, 50 / 500, 0.1)
r1 <- rnorm(500, 44 / 500, 0.07)
r2 <- rnorm(500, 24 / 500, 0.07)
bdiff <- b1 - b2
rdiff <- r1 - r2
scatterplot <- data.frame(bdiff, rdiff)

usethis::use_data(scatterplot, overwrite = TRUE)
