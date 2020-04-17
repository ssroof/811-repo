# ----------------------------------------------------
#   Load Data
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")

ads <- read_csv(here("Data", "Data Set.csv"))
ads <- select(ads, -X11)


# ----------------------------------------------------
#   Code Book
# ----------------------------------------------------

# Ideology: 0 = Republican, 1 = Democrat
# Incumbent: 0 = Yes, 1 = No
# Media Type: 0 = Television, 1 = Online
# Source: 0 = Vimeo, 1 = YouTube, 2 = Twitter, 3 = Facebook (where accessed)
# Theme: 0 = Policy, 1 = Character
# Function: 0 = Acclaims, 1 = Attacks, 2 = Defends, 3 = Other


# ----------------------------------------------------
#   Recode Variables
# ----------------------------------------------------

ads <- ads %>%
  mutate(
    Party = case_when(
      Ideology == 0 ~ "Republican",
      Ideology == 1 ~ "Democrat"
    )
  )




