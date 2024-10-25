## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
#  # If you don't have the remotes package installed, install it first
#  install.packages("remotes")
#  
#  # Install the genderOlympics package from GitHub
#  remotes::install_github("ETC5523-2024/assignment-4-packages-and-shiny-apps-Billy0624")

## ----setup--------------------------------------------------------------------
library(genderOlympics)

## -----------------------------------------------------------------------------
dplyr::glimpse(olympics_participation)

## -----------------------------------------------------------------------------
basketball_men_1992 <- olympics_participation |>
  dplyr::filter(sport == "Basketball", sex == "M", year == 1992, season == "Summer")

head(basketball_men_1992)

## -----------------------------------------------------------------------------
table(olympics_participation$medal)

## ----eval=FALSE---------------------------------------------------------------
#  run_app()

## -----------------------------------------------------------------------------
?olympics_participation

