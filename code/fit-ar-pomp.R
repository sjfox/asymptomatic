rm(list=ls())

#####################################################
## Reads in the arguments from command line running runs MIF and estimates posteriors
## Parameters required to be specified:

##    start_day - should be integer from 0:40
##    tau - symptomatic proportion - should be from 0 -1 
##    rho - symptomatic hospitalization probability (0 - 1)
##    omega - relative infectiousness of asymptomatic individuals - should be >= 0
##    city - city for fitting, should be "austin" or "nyc"
##    hour_of_run - rounded date to hour to denote for the folder 

#####################################################
args <- (commandArgs(TRUE)) ## load arguments from R CMD BATCH

if(length(args)>0)  { ## Then cycle through each element of the list and evaluate the expressions.
  print(paste0('loading in ', args, ' from R CMD BATCH'))
  for(i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}



# Start here if running locally -------------------------------------------
library(tidyverse)
library(pomp)
library(foreach)

base_url <- "asymptomatic"
if(grepl('spencerfox', Sys.info()['user'])){
  setwd(file.path("~", "projects", base_url))
  save_loc <- "processed_data/"
} else if(grepl('vagrant', Sys.info()['user'])) {
  setwd( file.path("/vagrant", base_url) )
  save_loc <- file.path("$WORK", base_url, "processed_data/")
} else if(grepl('stampede', Sys.info()['nodename'])) {
  setwd(file.path('/home1/02958/sjf826', base_url))
  save_loc <- file.path("$WORK", base_url, "processed_data/")
} else if(grepl('wrangler', Sys.info()['nodename'])){
  setwd(file.path('/home/02958/sjf826', base_url))
  save_loc <- file.path("$WORK", base_url, "processed_data/")
} else if (grepl('frontera', Sys.info()['nodename'])){
  setwd(file.path('/home1/02958/sjf826', base_url))  
  save_loc <- file.path("/work/02958/sjf826/frontera", base_url, "processed_data/")
}


sapply(c("code/ar-mod-start-date.R", 
         "code/data-processing-fxns.R",
         "code/post-processing-fxns.R"), source)

## Run this line if running locally on Rstudio, without command line parameters
# start_day = 0; tau = 0.82; omega = 0.67; city = "nyc"; hour_of_run = "2020-01-01-12"; rho = 0.05;
## city options  = "nyc", "austin", "utah"

# Prepare for fitting ----------------------------------------------------
## Prepares for city specification

days_back <- case_when(city == "austin" ~ 23,
                       city == "nyc" ~ 42,
                       city == "utah" ~ 23)

## Gets raw data and initial parameters
raw_data <- get_hospitalization_data(city)
init_parms <- get_init_parms_ar_start(city)

## Changes parameters based on input from script
init_parms <- replace_parms(c("tau" = tau, 
                              "omega" = omega,
                              "day_to_start" = start_day,
                              "rho" = rho), init_parms)

## Gets the data setup for fitting (adds 50 zeroes before data)
data_for_fitting <- get_data_and_parms(days_back, raw_data, init_parms)
maxT = nrow(data_for_fitting$data)


folder_name <- paste0(save_loc, hour_of_run, "_mif-fits")
if(!dir.exists(folder_name)){
  dir.create(folder_name)  
}

save_file_path <- with(as.list(data_for_fitting$parms),{
  paste0(folder_name, 
         "/city=", city,
         "_tau=", tau, 
         "_omega=", omega, 
         "_rho=", rho, 
         "_startdate=", 
         day_to_start, ".rda")  
})


# Run the fitting procedure -----------------------------------------------
## Setup the model
if(fitting_nyc){
  data_for_fitting$data %>%
    pomp(times="day", 
         t0 = 0,
         rinit=rinit,
         rmeasure=covid_rmeas,
         dmeasure=covid_dmeas_nyc_admit, 
         rprocess = pomp::euler(covid_rprocess, 1),
         statenames= covid_statenames,
         paramnames= covid_paramnames,
         params = data_for_fitting$parms,
         accumvars = c("NI", "NH", "LH", "NIY")
    ) -> covid_mod
  
  rerun(5, covid_mod %>%
    mif2(
      Np = 10000,
      Nmif = 1000,
      # Np = 100,
      # Nmif = 10,
      paramnames= covid_paramnames,
      params = data_for_fitting$parms,
      # partrans=parameter_trans(log=c("logbeta_stdev")),
      # rw.sd=rw.sd(logbeta_0=0.1, logbeta_stdev=0.05),
      partrans=parameter_trans(log=c("logbeta_stdev", "r")),
      rw.sd=rw.sd(logbeta_0=0.1, logbeta_stdev=0.05,  r=0.02),
      cooling.fraction.50=0.5
    )) -> mf2
  
} else {
  data_for_fitting$data %>% 
    pomp(times="day", 
         t0 = 0,
         rinit=rinit,
         rmeasure=covid_rmeas,
         dmeasure=covid_dmeas_admitdischarge, 
         rprocess = pomp::euler(covid_rprocess, 1),
         statenames= covid_statenames,
         paramnames= covid_paramnames,
         params = data_for_fitting$parms,
         accumvars = c("NI", "NH", "LH", "NIY")
    ) -> covid_mod
  ## Run mif fitting
  rerun(5, covid_mod %>%
    mif2(
      Np = 10000,
      Nmif = 1000,
      # Np = 100,
      # Nmif = 10,
      paramnames= covid_paramnames,
      params = data_for_fitting$parms,
      partrans=parameter_trans(log=c("logbeta_stdev", "mu")),
      rw.sd=rw.sd(logbeta_0=0.1, logbeta_stdev=0.05,  mu=0.02),
      cooling.fraction.50=0.5
    )) -> mf2
}

get_mle <- function(mf_mod, n_reps){
  replicate(n_reps, mf_mod %>% pfilter() %>% logLik()) %>% 
    logmeanexp(se=TRUE)  
}

mles <- mf2 %>% 
  purrr::map(get_mle, n_reps = 20)

best_fit_mle <- mles %>% purrr::map(function(x) x[1]) %>% which.max()
mle <- mles[[best_fit_mle]]
best_mod <- mf2[[best_fit_mle]]

fitted_parms <- coef(best_mod)
trace_info <- best_mod@traces

# Get the posterior draws for all states ----------------------------------

smoothed_states <- get_state_posteriors(best_mod, 
                                        maxT = maxT,
                                        N_smooth_draws = 1000, 
                                        N_particles_per_draw = 2000)

state_df <- get_state_ci_table(data_for_fitting$data, 
                               smoothed_states, 
                               state_names = c("logbeta_t", "H", "S", "NI","IA","IY", "NH", "LH", "D", "R"), 
                               clean_state_names = c("logbetat", "H", "S", "NI","IA","IY", "NH", "LH", "D", "R"))


# Save results
save(state_df,
     trace_info,
     mle,
     fitted_parms,
     file = save_file_path)


# save(est_alphas_df, file = file.path("..","workfolder","data","ZikaEstimatoR_data", "est_rr_post",
#                                      paste0("alpha_daily_mcmc_", temperature, "_",
#                                             ifelse(is.na(include_trans), 0, include_trans),
#                                             ifelse(extra_imports==0, "_false", "_true"),
#                                             ".rda")))
# 
# state_df %>%
#   filter(day >= 42) %>% 
#   ggplot(aes(date, exp(logbetat_mean))) +
#   geom_line() +
#   geom_ribbon(aes(ymin = exp(logbetat_lo), ymax = exp(logbetat_hi)), alpha = .1)
# 
# 
# state_df %>%
#   filter(day >= 42) %>%
#   mutate(rnot_mean = calc_ar_rnot(exp(logbetat_mean), fitted_parms),
#          rnot_lo = calc_ar_rnot(exp(logbetat_lo), fitted_parms),
#          rnot_hi = calc_ar_rnot(exp(logbetat_hi), fitted_parms)) %>%
#   ggplot(aes(date, rnot_mean)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = rnot_lo, ymax = rnot_hi), alpha = .1)

# 
# state_df %>%
#   filter(day >= 42) %>% 
#   ggplot(aes(date, S_mean)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = S_lo, ymax = S_hi), alpha = .1)
# 
# state_df %>%
#   filter(day >= 42) %>%
#   ggplot(aes(date, NH_mean)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = NH_lo, ymax = NH_hi), alpha = .1) +
#   geom_point(data = data_for_fitting$data %>% filter(day>=42), aes(date, new_admits))
# 
# fitted_parms
