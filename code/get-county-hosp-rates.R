##########################################
## Get county age breakdowns
##########################################
library(tidycensus)
library(tidyverse)


# Pull the data and extract the age distirbutions by county ---------------

# v17 <- load_variables(2015, "acs5", cache = TRUE)

cty_data <- get_acs("county", variables = paste0("B01001_", str_pad(3:49, width = 3, pad = "0")))

get_age_label <- function(label){
  str_split(label, "!!")[[1]][4]
}


age_cat <- tibble::tribble(
           ~age_group, ~category,
      "Under 5 years",     "0-9",
       "5 to 9 years",     "0-9",
     "10 to 14 years",   "10-19",
     "15 to 17 years",   "10-19",
    "18 and 19 years",   "10-19",
           "20 years",   "20-29",
           "21 years",   "20-29",
     "22 to 24 years",   "20-29",
     "25 to 29 years",   "20-29",
     "30 to 34 years",   "30-39",
     "35 to 39 years",   "30-39",
     "40 to 44 years",   "40-49",
     "45 to 49 years",   "40-49",
     "50 to 54 years",   "50-59",
     "55 to 59 years",   "50-59",
    "60 and 61 years",   "60-69",
     "62 to 64 years",   "60-69",
    "65 and 66 years",   "60-69",
     "67 to 69 years",   "60-69",
     "70 to 74 years",   "70-79",
     "75 to 79 years",   "70-79",
     "80 to 84 years",     "80+",
  "85 years and over",     "80+"
  )



county_age_data <- cty_data %>% 
  left_join(v17, by = c("variable" = "name")) %>% 
  select(GEOID, NAME, variable, estimate, label) %>% 
  mutate(label = purrr::map(label, get_age_label) %>% unlist()) %>% 
  inner_join(age_cat, by = c("label" = "age_group")) %>% 
  group_by(GEOID, NAME, category) %>% 
  summarize(population = sum(estimate)) %>% 
  rename(geoid = GEOID, county = NAME, age_group = category) %>% 
  ungroup()

county_age_data %>% write_csv("processed_data/county_age_data.csv")  


# Get hospitalization rates for each Austin MSA --------------------------
covid_hosp_data <- tibble::tribble(
  ~age_group, ~prop_hospitalized, ~lb_prop_hospitalized, ~ub_prop_hospitalized,
       "0-9",                  0,                     0,                     0,
     "10-19",           0.000408,              0.000243,              0.000832,
     "20-29",             0.0104,               0.00622,                0.0213,
     "30-39",             0.0343,                0.0204,                  0.07,
     "40-49",             0.0425,                0.0253,                0.0868,
     "50-59",             0.0816,                0.0486,                 0.167,
     "60-69",              0.118,                0.0701,                  0.24,
     "70-79",              0.166,                0.0987,                 0.338,
       "80+",              0.184,                  0.11,                 0.376
  )

get_hospitalization_rate <- function(df){
  df %>% 
    summarize(pop_hosp = sum(pop_frac*(prop_hospitalized)),
              pop_hosp_lb = sum(pop_frac*(lb_prop_hospitalized)),
              pop_hosp_ub = sum(pop_frac*(ub_prop_hospitalized)))
}

county_age_data <- read_csv("processed_data/county_age_data.csv")
austin_msa <- tibble(geoid = c('48021', '48055', '48209', '48453', '48491'))
county_age_data %>% 
  inner_join(austin_msa) %>% 
  group_by(age_group) %>% 
  summarize(population = sum(population)) %>% 
  mutate(pop_frac = population/sum(population)) %>% 
  left_join(covid_hosp_data) %>% 
  get_hospitalization_rate


nyc <- tibble(geoid = c('36005', '36047', '36061', '36081', '36085'))
county_age_data %>% 
  inner_join(nyc) %>% 
  group_by(age_group) %>% 
  summarize(population = sum(population)) %>% 
  mutate(pop_frac = population/sum(population)) %>% 
  left_join(covid_hosp_data) %>% 
  get_hospitalization_rate

##Utah
county_age_data %>% 
  filter(grepl(", Utah", county)) %>% 
  group_by(age_group) %>% 
  summarize(population = sum(population)) %>% 
  mutate(pop_frac = population/sum(population)) %>% 
  left_join(covid_hosp_data) %>% 
  get_hospitalization_rate
