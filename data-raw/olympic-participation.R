## code to prepare `olympic-participation` dataset goes here

olympics_participation <- readr::read_csv("data-raw/olympics_data.csv")

usethis::use_data(olympics_participation, overwrite = TRUE)
