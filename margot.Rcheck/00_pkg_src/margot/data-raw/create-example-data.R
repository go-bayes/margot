# Script to create small example dataset for margot package
# This creates a lightweight dataset suitable for examples and tests

library(dplyr)
library(tidyr)

set.seed(2025)  # for reproducibility

# parameters
n_participants <- 100
n_waves <- 3
years <- 2018:2020

# create participant-level baseline data
participants <- tibble(
  id = 1:n_participants,
  male = rbinom(n_participants, 1, 0.5),
  baseline_age = round(rnorm(n_participants, 45, 15))
) %>%
  mutate(
    baseline_age = pmax(18, pmin(80, baseline_age)),  # constrain to 18-80
    eth_cat = sample(c("euro", "maori", "pacific", "asian"), 
                     n_participants, 
                     replace = TRUE, 
                     prob = c(0.7, 0.15, 0.08, 0.07)),
    # personality traits (relatively stable)
    trait_forgiveness = rnorm(n_participants, 4.5, 1),
    trait_gratitude = rnorm(n_participants, 5, 0.8),
    trait_modesty = rnorm(n_participants, 4, 0.9)
  )

# create longitudinal data
df_margot_example <- expand_grid(
  id = 1:n_participants,
  wave = 0:(n_waves - 1)
) %>%
  left_join(participants, by = "id") %>%
  mutate(
    # time-varying variables
    year = years[wave + 1],
    age = baseline_age + wave,
    
    # partner status can change
    partner = rbinom(n(), 1, 0.6 + 0.1 * (age > 30) - 0.1 * (age > 60)),
    
    # sample weights (slightly varying by wave)
    sample_weights = rnorm(n(), 1, 0.1) %>% pmax(0.5) %>% pmin(2),
    
    # exposures with some individual variation and time trends
    forgiveness = trait_forgiveness + rnorm(n(), 0, 0.3) + wave * 0.05,
    gratitude = trait_gratitude + rnorm(n(), 0, 0.3) + wave * 0.02,
    modesty = trait_modesty + rnorm(n(), 0, 0.3) - wave * 0.02,
    
    # outcomes influenced by exposures and other factors
    alcohol_frequency = rpois(n(), 
      lambda = exp(1.5 - 0.2 * forgiveness + 0.1 * male + 0.02 * age)
    ) %>% pmin(7),
    
    alcohol_intensity = rpois(n(),
      lambda = exp(1.2 - 0.15 * forgiveness + 0.2 * male - 0.01 * age + 
                   0.5 * (alcohol_frequency > 3))
    ) %>% pmin(7),
    
    hours_exercise = rgamma(n(), 
      shape = 2, 
      rate = 0.5 / (1 + 0.1 * gratitude - 0.02 * age + 0.3 * male)
    ) %>% round(1),
    
    ego_rubin = 3.5 + 0.3 * forgiveness + 0.2 * gratitude - 0.1 * modesty +
                0.1 * partner + rnorm(n(), 0, 0.5)
  ) %>%
  # constrain to reasonable ranges
  mutate(
    across(c(forgiveness, gratitude, modesty, ego_rubin), ~pmax(1, pmin(7, .))),
    hours_exercise = pmax(0, pmin(30, hours_exercise))
  ) %>%
  # add some missing data patterns
  mutate(
    # some people miss wave 2
    across(c(alcohol_frequency, alcohol_intensity, hours_exercise, ego_rubin),
           ~if_else(wave == 2 & runif(n()) < 0.15, NA_real_, .)),
    # some baseline variables have missing data
    partner = if_else(wave == 0 & runif(n()) < 0.05, NA_real_, as.numeric(partner))
  ) %>%
  # select final variables in order
  select(
    id, wave, year, male, age, partner, eth_cat, sample_weights,
    forgiveness, gratitude, modesty, 
    alcohol_frequency, alcohol_intensity, hours_exercise, ego_rubin
  ) %>%
  # remove temporary trait variables
  arrange(id, wave)

# save as .rda file
usethis::use_data(df_margot_example, overwrite = TRUE)

# create deprecation notice for df_nz
# we'll keep the existing df_nz for now but add documentation about deprecation