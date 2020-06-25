## code to prepare `my_dataset` dataset goes here

data_1 <- read.table('data-raw/C1_test.txt')
data_2 <- read.table('data-raw/C2_test.txt')

usethis::use_data(data_1, overwrite = TRUE)
usethis::use_data(data_2, overwrite = TRUE)
