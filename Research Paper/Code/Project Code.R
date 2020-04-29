# ----------------------------------------------------
#   Load Data
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
library("broom")
library("tidylog")
library("stargazer")
library("nnet")
library("foreign")

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
      Ideology == 0 ~ "republican",
      Ideology == 1 ~ "democrat"
    )
  )

ads <- rename(
  ads,
  title = "Video Title",
  candidate = Candidate,
  ideology = Ideology,
  incumbent = "Incumbent?",
  media_type = "Media Type",
  source = Source,
  theme = Theme,
  func = Function,
  length = "Length (s)",
  funding = "Funding Source",
  party = Party
)



# ----------------------------------------------------
#   Descriptive Statistics
# ----------------------------------------------------


ggplot(ads) +
  aes(x = media_type) +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 1),
    labels = c("Television", "Online")
  ) +
  labs(
    title = "Media Type Distribution",
    x = "Media Type", y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(ads) +
  aes(x = source) +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 3, 1),
    labels = c("Vimeo", "YouTube", "Twitter", "Facebook")
  ) +
  labs(
    title = "Ad Access Sites",
    x = "Source", y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(ads) +
  aes(x = theme) +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 1),
    labels = c("Policy", "Character")
  ) +
  labs(
    title = "Ad Themes",
    x = "Theme", y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(ads) +
  aes(x = func) +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 3, 1),
    labels = c("Acclaim", "Attack", "Defend" ,"Other")
  ) +
  labs(
    title = "Ad Functions",
    x = "Function", y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))



# ----------------------------------------------------
#   Data Analysis
# ----------------------------------------------------

f.source <- as.factor(ads$source)

f.func <- as.factor(ads$func)


# Theme

theme_reg <- glm(theme ~ ideology + incumbent + media_type,
  family = binomial(link = "logit"),
  data = ads
)

theme_tidy <- tidy(theme_reg, conf.int = TRUE)
theme_tidy

glance(theme_reg)

ggplot(data = augment(theme_reg)) +
  aes(x = .fitted, y = .resid) +
  geom_point()

ggplot(theme_tidy) +
  aes(x = reorder(term, -estimate), y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "red")

stargazer(theme_reg, type = "html", out = "theme_tidy.htm")


# Function

func_reg <- multinom(func ~ ideology + incumbent + media_type, data = ads)

func_tidy <- tidy(func_reg, conf.int = TRUE)
func_tidy

glance(func_reg)

stargazer(func_reg, type = "html", out = "func_tidy.htm")


# Smaller Models

# - Incumbent and media type

incumb_media <- glm(media_type ~ ideology + incumbent,
                 family = binomial(link = "logit"),
                 data = ads
)

incumb_media_tidy <- tidy(incumb_media, conf.int = TRUE)
incumb_media_tidy

ggplot(incumb_media_tidy) +
  aes(x = reorder(term, -estimate), y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "red")

stargazer(incumb_media, type = "html", out = "incumb_media.htm")


# - Incumbent and theme

incumb_theme <- glm(theme ~ ideology + incumbent,
                    family = binomial(link = "logit"),
                    data = ads
)

incumb_theme_tidy <- tidy(incumb_theme, conf.int = TRUE)
incumb_theme_tidy

ggplot(incumb_theme_tidy) +
  aes(x = reorder(term, -estimate), y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "red")

stargazer(incumb_theme, type = "html", out = "incumb_theme.htm")


# - Incumbent and function

incumb_func <- multinom(func ~ ideology + incumbent, data = ads)

incumb_func_tidy <- tidy(incumb_func, conf.int = TRUE)
incumb_func_tidy

stargazer(incumb_func, type = "html", out = "incumb_func.htm")


# - Media type and theme

type_theme <- glm(theme ~ ideology + media_type,
                    family = binomial(link = "logit"),
                    data = ads
)

type_theme_tidy <- tidy(type_theme, conf.int = TRUE)
type_theme_tidy

ggplot(type_theme_tidy) +
  aes(x = reorder(term, -estimate), y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "red")

stargazer(type_theme, type = "html", out = "type_theme.htm")


# - Media type and function

type_func <- multinom(func ~ ideology + media_type, data = ads)

type_func_tidy <- tidy(type_func, conf.int = TRUE)
incumb_func_tidy

stargazer(type_func, type = "html", out = "type_func.htm")


# Media type and length

type_length <- lm(length ~ ideology + media_type, data = ads)

type_length_tidy <- tidy(type_theme, conf.int = TRUE)
type_length_tidy

ggplot(type_length_tidy) +
  aes(x = reorder(term, -estimate), y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "red")

stargazer(type_length, type = "html", out = "type_length.htm")


# - Theme and function

theme_func <- glm(theme ~ ideology + f.func,
                  family = binomial(link = "logit"),
                  data = ads
)

theme_func_tidy <- tidy(theme_func, conf.int = TRUE)
theme_func_tidy

ggplot(theme_func_tidy) +
  aes(x = reorder(term, -estimate), y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "red")

stargazer(theme_func, type = "html", out = "theme_func.htm")


