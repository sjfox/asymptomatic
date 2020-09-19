################################
### Get potential start dates
################################

library(tidyverse)
library(pomp)

sapply(c("code/ar-mod-start-date.R", 
         "code/data-processing-fxns.R",
         "code/post-processing-fxns.R"), source)

## Run this line if running locally on Rstudio, without command line parameters
# start_day = 20; tau = 0.82; omega = 0.67; city = "nyc"; hour_of_run = "2020-01-01-12";
city <- "utah"

# Setup data and parameters ----------------------------------------------------
## Prepares for city specification

## Gets raw data and initial parameters
first_nh <- get_hospitalization_data(city) %>% 
  head(1) %>% pull(new_admits)

## Pull in the best guess estimates for initial parameters
init_parms <- get_init_parms_ar_start(city)

## Get the initial R0 for simulations
calc_ar_rnot(exp(init_parms['logbeta_0']), init_parms)


# Run simulations and find event timing -----------------------------------
tibble(day = 0:200,
       new_admits = 0,
       new_discharges = 0) %>% 
  pomp(times="day", 
       t0 = 0,
       rinit=rinit,
       rmeasure = covid_rmeas,
       rprocess = pomp::euler(covid_rprocess, 1),
       statenames= covid_statenames,
       paramnames= covid_paramnames,
       params = init_parms,
       accumvars = c("NI", "NH", "LH", "NIY")
  ) -> covid_mod

sims <- covid_mod %>% 
  simulate(nsim = 1000, seed = 34786) %>% 
  as.data.frame()

## Check the doubling time of epidemic
# sims %>% 
#   group_by(.id) %>% 
#   mutate(cum_i = cumsum(NI)) %>% 
#   ungroup() %>% 
#   select(day, cum_i) %>% 
#   group_by(day) %>% 
#   summarize(cum_i = mean(cum_i)) %>% 
#   mutate(log_cumi = log(cum_i)) %>% 
#   filter(day != 0, day < 60)  %>% 
#   lm(log_cumi ~ day, data = .) %>% 
#   summary() %>% coef()

## Get reuslts
utah_results <- sims %>% 
  filter(NH >= first_nh) %>% 
  group_by(.id) %>% 
  slice(1) %>%
  ungroup() %>% 
  mutate(day = day) %>% 
  summarize(min_day = min(day),
            lb_day = quantile(day, probs = 0.25),
            avg_day = mean(day),
            med_day = median(day),
            ub_day = quantile(day, probs = 0.75),
            max_day = max(day)
  )


# Run for NYC -------------------------------------------------------------
city <- "nyc"

# Setup data and parameters ----------------------------------------------------
## Prepares for city specification

## Gets raw data and initial parameters
first_nh <- get_hospitalization_data(city) %>% 
  head(1) %>% pull(new_admits)

## Pull in the best guess estimates for initial parameters
init_parms <- get_init_parms_ar_start(city)

## Get the initial R0 for simulations
calc_ar_rnot(exp(init_parms['logbeta_0']), init_parms)


# Run simulations and find event timing -----------------------------------
tibble(day = 0:200,
       new_admits = 0,
       new_discharges = 0) %>% 
  pomp(times="day", 
       t0 = 0,
       rinit=rinit,
       rmeasure = covid_rmeas,
       rprocess = pomp::euler(covid_rprocess, 1),
       statenames= covid_statenames,
       paramnames= covid_paramnames,
       params = init_parms,
       accumvars = c("NI", "NH", "LH", "NIY")
  ) -> covid_mod

sims <- covid_mod %>% 
  simulate(nsim = 1000, seed = 903) %>% 
  as.data.frame()

nyc_results <- sims %>% 
  filter(NH >= first_nh) %>% 
  group_by(.id) %>% 
  slice(1) %>%
  ungroup() %>% 
  mutate(day = day) %>%  
  summarize(min_day = min(day),
            lb_day = quantile(day, probs = 0.25),
            avg_day = mean(day),
            med_day = median(day),
            ub_day = quantile(day, probs = 0.75),
            max_day = max(day)
  )




# Print results -----------------------------------------------------------
utah_results %>% 
  mutate_all(function(x) ymd("2020-03-07") - x)
nyc_results %>% 
  mutate_all(function(x) ymd("2020-02-29") - x)




sims %>% 
  filter(NH >= first_nh) %>% 
  group_by(.id) %>% 
  slice(1) %>%
  ungroup() %>% 
  mutate(day = day) %>% 
  pull(day) %>% 
  
  ggplot(aes(day)) + geom_histogram()