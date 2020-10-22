## Script for MS figures
library(tidyverse)
library(pomp)
library(cowplot)
theme_set(theme_cowplot())

source("code/ar-mod-start-date.R")

read_fitting_data <- function(file_path){
  # browser()
  test <- tryCatch({
    load(file_path)
  }, error = function(cond) {
    return(NA)
  }
  )
  if(is.na(test)){
    return(NA)
  }
  mle_val <- mle %>% enframe() %>% mutate(name = c("mle", "mle_se")) %>% spread(name,value)
  parms <- fitted_parms %>% 
    enframe() %>% 
    spread(name,value)
  city_fit <- str_split(str_split(file_path, pattern = "city=")[[1]][2], "_")[[1]][1]
  
  state_df %>%  
    tail(1) %>% 
    bind_cols(mle_val, 
              parms, 
              tibble(city = city_fit), 
              tibble(full_df = list(state_df)), 
              tibble(fitted_parms = list(parms)))
}

studies <- tibble(study = c("18% (Diamond Princess)", "43% (Iceland)", "88% (Greece)"),
                  asymp_frac = c("18%", "43%", "88%"),
                  tau = c(0.82, 0.57, 0.12),
                  omega = 0.67)
pop_size <- tibble(city = c("austin", "nyc", "utah"), 
                   population = c(2168317, 8398748, 3205958),
                   best_fit_days_back = c(23, 48, 23),
                   first_hosp_data = lubridate::ymd(c("2020-03-10", "2020-02-29", "2020-03-07")),
                   mle_rho = c(0.0443, 0.0513, .0412))

rho_tbl <- tibble(city = rep(c("austin", "nyc", "utah"), each = 3),
                  rho = c(0.0443, 0.0264, 0.0905, 0.0513, 0.0305, 0.105, 0.0412, 0.0245, 0.0841),
                  rho_label = factor(rep(c("Mean", "Lowerbound", "Upperbound"), 3),
                                     levels = c("Lowerbound", "Mean", "Upperbound")))

## folder_with_fits <- "processed_data/2020-05-18-16_mif-fits/" ## This one is with nbinom and fixed AR process
folder_with_fits <- "processed_data/2020-09-19-00_mif-fits/" ## Prior on sd of AR and nbinom dispersion fitting for NYC
vec <- list.files(folder_with_fits, full.names = T)
vec %>% length()

fit_results <- vec %>% 
  purrr::map(read_fitting_data)
fit_results <- fit_results[!purrr::map(fit_results, 
                                       function(x){any(class(x) == "logical")}) %>% 
                             unlist()] %>% 
  bind_rows()


# Get city data -----------------------------------------------------------
# get_prob_positive_by_day_since_infection <- function(day){
#   antibody_positivity_vec <- c(rep(1, 30), (8/19 - 1) / 60 * 1:1000 + 1)
#   antibody_positivity_vec <- ifelse(antibody_positivity_vec <0, 0, antibody_positivity_vec)
#   return(antibody_positivity_vec[day])
# }
# 
# # get_prob_positive_by_day_since_infection(1:1000)
# prob_positive <- get_prob_positive_by_day_since_infection(1:1000)
# fit_results %>% 
#   select(tau, omega, rho, city, full_df) %>% 
#   unnest(full_df) %>% 
#   bind_rows() %>% 
#   # select(date, NI_mean) %>% 
#   mutate(days_since_infection = as.numeric(lubridate::ymd("2020-07-07") - date)) %>% 
#   mutate(positives = NI_mean * prob_positive[days_since_infection]) %>% 
#   group_by(city, tau, rho, omega) %>% 
#   summarize(per_pos = sum(positives)/8398748) -> temp
# 

## NYC Serology results
convert_cdc_to_nyc <- function(cdc_sero){
  cdc_sero
  # nyc_pop <- 43.3
  # other_pop <- 14.4 + 6.4
  # cdc_sero*(nyc_pop + other_pop) / (1.6*nyc_pop + other_pop) * 1.6
}
# 95% CI
nyc_results <- tibble(immune_fraction = c(0.227, 0.215, 0.24),
                      label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "NYC")

nyc_results_rd1 <- tibble(immune_fraction = convert_cdc_to_nyc(c(0.069, 0.05, 0.089)),
                           label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "NYC")
nyc_results_rd2 <- tibble(immune_fraction = convert_cdc_to_nyc(c(0.232, .2, 0.263)),
                           label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "NYC")
nyc_results_rd3 <- tibble(immune_fraction = convert_cdc_to_nyc(c(0.195, 0.172, 0.219)),
                           label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "NYC")
nyc_results_rd4 <- tibble(immune_fraction = convert_cdc_to_nyc(c(0.176, 0.155, 0.2)),
                           label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "NYC")

nyc_dates <- lubridate::ymd(c("2020-03-23", "2020-04-25", "2020-06-15", "2020-07-07")) - lubridate::days(7)
nyc_results_all <- bind_rows(nyc_results_rd1, nyc_results_rd2, nyc_results_rd3,nyc_results_rd4) %>% 
  mutate(date = nyc_dates)


utah_results_rd1 <- tibble(immune_fraction = c(0.022, 0.012, 0.034),
                       label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "Utah")
utah_results_rd2 <- tibble(immune_fraction = c(0.011, 0.006, 0.021),
                           label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "Utah")
utah_results_rd3 <- tibble(immune_fraction = c(0.015, 0.009, 0.026),
                           label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "Utah")
utah_results_rd4 <- tibble(immune_fraction = c(0.027, 0.018, 0.039),
                           label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "Utah")

utah_dates <- lubridate::ymd(c("2020-04-20", "2020-05-25", "2020-06-15", "2020-07-06")) - lubridate::days(7)
utah_results_all <- bind_rows(utah_results_rd1, utah_results_rd2, utah_results_rd3,utah_results_rd4) %>% 
  mutate(date = utah_dates)



# Get asymptomatic proportion estimates fo rNYC/Utah ----------------------
## Calculate for all dates in Utah
utah_immune_cdc <- fit_results %>% 
  filter(city == "utah") %>% 
  select(tau, rho, omega) %>% 
  bind_cols(fit_results %>% 
              filter(city == "utah") %>% 
              pull(full_df) %>% 
              bind_rows() %>% 
              filter(date %in% lubridate::ymd(utah_dates)) %>% 
              mutate(immune_hi = 1 - S_lo/3205958,
                     immune_mean = 1 - S_mean/3205958,
                     immune_lo = 1 - S_hi/3205958,
                     immune_sd = S_sd/3205958) %>% 
              mutate(var = rep(seq(1:(n()/length(utah_dates))), each = length(utah_dates))) %>% 
              nest(data = c(date, hospitalized, new_admits, new_discharges, zt, day, logbetat_mean, 
                            logbetat_sd, logbetat_lo, logbetat_hi, H_mean, H_sd, H_lo, 
                            H_hi, S_mean, S_sd, S_lo, S_hi, NI_mean, NI_sd, NI_lo, NI_hi, 
                            IA_mean, IA_sd, IA_lo, IA_hi, IY_mean, IY_sd, IY_lo, IY_hi, 
                            NH_mean, NH_sd, NH_lo, NH_hi, LH_mean, LH_sd, LH_lo, LH_hi, 
                            D_mean, D_sd, D_lo, D_hi, R_mean, R_sd, R_lo, R_hi, immune_hi, 
                            immune_mean, immune_lo, immune_sd))
  ) %>% 
  unnest(data) %>% 
  select(date, tau, rho, omega, immune_lo, immune_hi, immune_mean) 

utah_asymp <- utah_immune_cdc %>% 
  left_join(utah_results_all, by = "date") %>% 
  filter((immune_lo <= imin & immune_hi >= imin) |
           (immune_lo <= imax & immune_hi >= imax) |
           (immune_lo >= imin & immune_hi <= imax)
  )  %>%
  mutate(immune_diff = abs(immune_mean - imean)) %>%
  group_by(date, rho, omega) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)]) %>% 
  ungroup()





nyc_compare <- fit_results %>% 
  filter(city == "nyc") %>% 
  select(tau, rho, omega) %>% 
  bind_cols(fit_results %>% 
              filter(city == "nyc") %>% 
              pull(full_df) %>% 
              bind_rows() %>% 
              filter(date %in% lubridate::ymd(nyc_dates)) %>% 
              mutate(immune_hi = 1 - S_lo/8398748,
                     immune_mean = 1 - S_mean/8398748,
                     immune_lo = 1 - S_hi/8398748,
                     immune_sd = S_sd/8398748) %>% 
              mutate(var = rep(seq(1:(n()/length(nyc_dates))), each = length(nyc_dates))) %>% 
              nest(data = c(date, hospitalized, new_admits, new_discharges, zt, day, logbetat_mean, 
                            logbetat_sd, logbetat_lo, logbetat_hi, H_mean, H_sd, H_lo, 
                            H_hi, S_mean, S_sd, S_lo, S_hi, NI_mean, NI_sd, NI_lo, NI_hi, 
                            IA_mean, IA_sd, IA_lo, IA_hi, IY_mean, IY_sd, IY_lo, IY_hi, 
                            NH_mean, NH_sd, NH_lo, NH_hi, LH_mean, LH_sd, LH_lo, LH_hi, 
                            D_mean, D_sd, D_lo, D_hi, R_mean, R_sd, R_lo, R_hi, immune_hi, 
                            immune_mean, immune_lo, immune_sd))
  ) %>% 
  unnest(data) %>% 
  select(date, tau, rho, omega, immune_lo, immune_hi, immune_mean) 

nyc_asymp <- nyc_compare %>% 
  left_join(nyc_results_all, by = "date") %>% 
  filter((immune_lo <= imin & immune_hi >= imin) |
           (immune_lo <= imax & immune_hi >= imax) |
           (immune_lo >= imin & immune_hi <= imax)
  )  %>%
  mutate(immune_diff = abs(immune_mean - imean)) %>%
  group_by(date, rho, omega) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)]) %>% 
  ungroup()
nyc_asymp


# convert_cdc_to_nyc <- function(cdc_sero){
#   nyc_pop <- 43.3
#   other_pop <- 14.4 + 6.4
#   cdc_sero*(nyc_pop + other_pop) / (1.6*nyc_pop + other_pop) * 1.6  
# }


# Figure 1 version 1 ------------------------------------------------------
rnot_df <- fit_results %>% 
  inner_join(studies) %>% 
  left_join(pop_size, by = "city") %>% 
  filter(rho == mle_rho) %>%
  select(city, tau, omega, rho, study, asymp_frac, full_df, first_hosp_data, fitted_parms, population) %>% 
  unnest(full_df) %>% 
  mutate(rnot_mean = map2(exp(logbetat_mean), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_lo = map2(exp(logbetat_lo), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_hi = map2(exp(logbetat_hi), fitted_parms, calc_ar_rnot) %>% unlist()) %>% 
  filter(date >= first_hosp_data) %>% 
  mutate(city = case_when(city == "austin" ~ "Austin", 
                          city == "nyc"~"NYC",
                          city == "utah" ~ "Utah")) %>% 
  mutate(rnot_mean = rnot_mean*S_mean/population,
         rnot_hi = rnot_hi*S_mean/population,
         rnot_lo = rnot_lo*S_mean/population) %>% 
  filter(city != "Austin")
  # filter(date <= "2020-04-27")

fig1_fits <- rnot_df %>% 
  filter(city != "Austin") %>% 
  # filter(city == city_name) %>% 
  filter(tau == 0.12) %>% 
  ggplot(aes(date, new_admits)) +
    geom_vline(data = utah_results_all, aes(xintercept = date), inherit.aes = FALSE, color = "steelblue4", lty = 2) + 
    geom_vline(data = nyc_results_all, aes(xintercept = date), inherit.aes = FALSE,color = "steelblue4", lty = 2) +   
    geom_line(data = rnot_df, aes(date, NH_mean, color = study)) +
    geom_ribbon(data = rnot_df , aes(date, ymin = NH_lo, ymax = NH_hi, fill = study), alpha = .2) +
    geom_point() +
    facet_wrap(~city, scales = "free", ncol = 1)+
    scale_fill_brewer(type = "qual", palette = 2)+
    scale_color_brewer(type = "qual", palette = 2)+
    background_grid(major = "xy") +
    theme(strip.background = element_rect(fill = NA), legend.position = "bottom") +
    # scale_x_date(date_breaks = "1 week", date_labels = "%b-%d", limits = c(min(rnot_df$date), max(rnot_df$date))) +
    labs(y = "Hospital admissions", x = "Date", color = "Asymptomatic rate:", fill = "Asymptomatic rate:") 
fig1_fits

nyc_date_label <- "NYC (April 18, 2020)"
utah_date_label <- "Utah (May 18, 2020)"
factor_date_label <- c(nyc_date_label, utah_date_label)

immune_data <- fit_results %>% 
  left_join(pop_size, by = "city") %>% 
  filter(city == "utah", rho == mle_rho) %>% 
  select(tau, omega) %>% 
  bind_cols(fit_results %>% 
              left_join(pop_size, by = "city") %>% 
              filter(city == "utah", rho == mle_rho) %>% 
              pull(full_df) %>% 
              bind_rows() %>% 
              filter(date == utah_dates[2]) %>% 
              mutate(population = 3205958,
                     immune_hi = 1 - S_lo/population,
                     immune_mean = 1 - S_mean/population,
                     immune_lo = 1 - S_hi/population,
                     immune_sd = S_sd/population) 
  ) %>% 
  select(date, tau, omega, immune_lo, immune_hi, immune_mean, population) %>% 
  mutate(city_label = utah_date_label,
         city = "utah") %>% 
  bind_rows(fit_results %>% 
              left_join(pop_size, by = "city") %>% 
              filter(city == "nyc", rho == mle_rho) %>% 
              select(tau, omega) %>% 
              bind_cols(fit_results %>% 
                          left_join(pop_size, by = "city") %>% 
                          filter(city == "nyc", rho == mle_rho) %>% 
                          pull(full_df) %>% 
                          bind_rows() %>% 
                          filter(date == nyc_dates[2]) %>% 
                          mutate(population = 8398748,
                                 immune_hi = 1 - S_lo/population,
                                 immune_mean = 1 - S_mean/population,
                                 immune_lo = 1 - S_hi/population,
                                 immune_sd = S_sd/population) 
              ) %>% 
              select(date, tau, omega, immune_lo, immune_hi, immune_mean, population) %>% 
              mutate(city_label = nyc_date_label,
                     city = "nyc")
              ) %>% 
  mutate(asymp_frac = 1-tau) %>% 
  filter(omega == "0.67",
         tau >= 0.15) %>% 
  select(city, city_label, tau, immune_mean, population) %>% 
  mutate(Susceptible = 1-immune_mean,
         Symptomatic = immune_mean*tau,
         Asymptomatic = immune_mean - Symptomatic
  ) %>% 
  mutate(Symptomatic_lower = 0,
         Symptomatic_upper = Symptomatic,
         Asymptomatic_lower = Symptomatic,
         Asymptomatic_upper = Asymptomatic+Symptomatic) %>% 
  select(-Susceptible, - population, - Asymptomatic, -Symptomatic, -immune_mean) %>% 
  gather(key, value, Symptomatic_lower:Asymptomatic_upper) %>% 
  separate(col = "key", into = c("key", "ribbon_side"), sep = "_") %>% 
  spread(ribbon_side, value)
  

fig1_immunity <- immune_data %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible"))),
         city_label = factor(city_label, levels = factor_date_label)) %>%
  ggplot(aes(x = 1-tau, ymin = lower, ymax = upper,
             fill = key)) +
  geom_ribbon() +
  facet_wrap(~city_label, ncol = 1, scales = "free_y") +
  geom_rect(data = nyc_results_rd2 %>% 
              mutate(city_label = factor(nyc_date_label, levels = factor_date_label)), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_rect(data = utah_results_rd2 %>% 
              mutate(city_label = factor(utah_date_label, levels = factor_date_label)), aes(xmin = -Inf, 
                                                                                    xmax = Inf, 
                                                                                    ymin = imin, 
                                                                                    ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_hline(yintercept = 0.6, lty = 2) +
  geom_errorbarh(data = nyc_asymp %>% 
                   filter(rho == 0.0513, omega == 0.67, date == nyc_dates[2]) %>% 
                   mutate(city_label = factor(nyc_date_label, levels = factor_date_label)), height = 0.02,
            aes(y = nyc_results$imax+.05, xmin = asymp_min, xmax = asymp_max),
            inherit.aes=FALSE)+
  geom_point(data = nyc_asymp %>% 
                   filter(rho == 0.0513, omega == 0.67, date == nyc_dates[2]) %>% mutate(city_label = factor(nyc_date_label, levels = factor_date_label)), 
                 aes(y = nyc_results$imax+.05, x = most_sim_asymp),
                 inherit.aes=FALSE)+
  geom_errorbarh(data = utah_asymp %>% 
                   filter(rho == 0.0412, date == utah_dates[2], omega == 0.67) %>% mutate(city_label = factor(utah_date_label, levels = factor_date_label)), height = 0.02,
                 aes(y = utah_results_rd4$imax+.05, xmin = asymp_min, xmax = asymp_max),
                 inherit.aes=FALSE)+
  geom_point(data = utah_asymp %>% 
               filter(rho == 0.0412, date == utah_dates[2], omega == 0.67) %>% mutate(city_label = factor(utah_date_label, levels = factor_date_label)), 
             aes(y = utah_results_rd4$imax+.05, x = most_sim_asymp),
             inherit.aes=FALSE)+
  # geom_vline(xintercept = nyc_asymp %>% 
  #                  filter(rho == 0.0513) %>% 
  #                  pull(asymp_min), lty = 3) +
  # geom_vline(xintercept = nyc_asymp %>% 
  #              filter(rho == 0.0513) %>% 
  #              pull(asymp_max), lty = 3) +
  # scale_y_log10() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic rate", 
       y = "Cumulative infections", 
       fill = "Infection type:") +
  theme(strip.background = element_rect(fill = NA), legend.position = "bottom") +
  annotate("text", x = 0.01, y = 0.61, label = "Herd immunity threshold", hjust = 0, vjust = 0) +
  geom_text(data = tibble(city_label = factor(nyc_date_label, levels = factor_date_label)),
            x = 0.01, y = nyc_results_rd2$imax+.01, 
            label = "Seroprevalence estimate",
            hjust = 0, 
            vjust = 0, 
            color = "steelblue4", inherit.aes=FALSE) +
  geom_text(data = tibble(city_label = factor(utah_date_label, levels = factor_date_label)),
            x = 0.01, y = utah_results_rd2$imax+.01, 
            label = "Seroprevalence estimate",
            hjust = 0, 
            vjust = 0, 
            color = "steelblue4", inherit.aes=FALSE) +
  # background_grid(major = "xy") +
  # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
  # scale_fill_brewer(type = "qual", palette = 2, direction = -1) 
  scale_fill_manual(values = c("gray", "darkslategrey"))
fig1_immunity

ms_fig1 <- plot_grid(fig1_fits, fig1_immunity, align = "h", ncol = 2, labels = c("A", "B"))
save_plot("figures/asymp-fig1.png", ms_fig1, base_height = 7.5, base_asp = 1.7)

## Save into separate for different panels
save_plot("figures/asymp-fig1A.png", fig1_fits, base_height = 6, base_asp = 1.3)
save_plot("figures/asymp-fig1B.png", fig1_immunity, base_height = 6, base_asp = 1.1)


# Figure S1 ---------------------------------------------------------------
rnot_df <- fit_results %>% 
  filter(city !="austin") %>% 
  inner_join(studies) %>% 
  left_join(pop_size, by = "city") %>% 
  filter(rho == mle_rho) %>%
  select(city, tau, omega, rho, study, asymp_frac, full_df, first_hosp_data, fitted_parms, population) %>% 
  unnest(full_df) %>% 
  mutate(rnot_mean = map2(exp(logbetat_mean), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_lo = map2(exp(logbetat_lo), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_hi = map2(exp(logbetat_hi), fitted_parms, calc_ar_rnot) %>% unlist()) %>% 
  filter(date >= first_hosp_data) %>% 
  mutate(rnot_mean = rnot_mean*S_mean/population,
         rnot_hi = rnot_hi*S_mean/population,
         rnot_lo = rnot_lo*S_mean/population)

p1 <- rnot_df %>% 
  mutate(city = ifelse(city == "utah", "Utah", "NYC")) %>% 
  ggplot(aes(date, rnot_mean, color = study, fill = study)) + 
  geom_ribbon(aes(ymin = rnot_lo, ymax = rnot_hi), alpha = .1, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~city) +
  scale_fill_brewer(type = "qual", palette = 2)+
  scale_color_brewer(type = "qual", palette = 2)+
  labs(x ="", y = expression(R[t]), 
       fill = "Asymptomatic Proportion:", 
       color = "Asymptomatic Proportion:") +
  background_grid(major = "xy") +
  geom_hline(yintercept = 1, lty = 3, size = 1) +
  ylim(0,10) +
  theme(strip.background = element_rect(fill = NA), strip.text = element_text(face = "bold", size = 18)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%d") 
p1a <- p1 + theme(legend.position = "none")
p2 <- rnot_df %>% 
  # filter(city == city_name) %>% 
  filter(tau == 0.12) %>% 
  ggplot(aes(date, new_admits)) +
  geom_line(data = rnot_df, aes(date, NH_mean, color = study)) +
  geom_ribbon(data = rnot_df, aes(date, ymin = NH_lo, ymax = NH_hi, fill = study), alpha = .2) +
  geom_point() +
  facet_wrap(~city, scales = "free_y")+
  scale_fill_brewer(type = "qual", palette = 2)+
  scale_color_brewer(type = "qual", palette = 2)+
  background_grid(major = "xy") +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%d") +
  labs(y = "Hospital admissions", x = "") 
p2a <- p2 + theme(legend.position = "none")

p3 <- rnot_df %>% 
  # left_join(city_data, by = c("date", "city")) %>% 
  mutate(city = ifelse(city == "utah", "Utah", "NYC")) %>% 
  mutate(NIY_mean = NI_mean*tau, 
         NIA_mean = NI_mean-NIY_mean,
         NIY_lo = NI_lo*tau, 
         NIA_lo = NI_lo-NIY_lo,
         NIY_hi = NI_hi*tau, 
         NIA_hi = NI_hi-NIY_hi) %>% 
  group_by(city, tau, omega, rho) %>% 
  mutate(cumIY_mean = cumsum(NIY_mean), 
         cumIA_mean = cumsum(NIA_mean),
         cumIY_lo = cumsum(NIY_lo), 
         cumIA_lo = cumsum(NIA_lo),
         cumIY_hi = cumsum(NIY_hi), 
         cumIA_hi = cumsum(NIA_hi)) %>% 
  ungroup() %>% 
  gather(key, value, cumIY_mean:cumIA_hi) %>% 
  select(city, date, study, key, value) %>% 
  separate(col = key, into = c("compartment", "metric"), sep = "_") %>% 
  spread(metric, value) %>% 
  mutate(compartment = ifelse(compartment == "cumIA", "Asymptomatic", "Symptomatic")) %>% 
  ggplot(aes(date, mean, color = study, lty = compartment, fill = study)) + 
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .1, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~city, scales = "free_y") +
  scale_color_brewer(type = "qual", palette = 2)+
  scale_fill_brewer(type = "qual", palette = 2)+
  labs(x ="", y = "Cumulative infections", 
       fill = "Asymptomatic\nProportion", 
       color = "Asymptomatic\nProportion",
       lty = "Infection type:") +
  guides(fill=FALSE, color = FALSE) +
  background_grid(major = "xy") +
  # geom_hline(yintercept = 1, lty = 2) +
  # ylim(0,30) +
  scale_y_continuous(labels = scales::comma) +
  theme(strip.background = element_rect(fill = NA), strip.text = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%d") 
p3a <- p3 + theme(legend.position = "none")

color_leg <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1),
           fill = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.box.margin = margin(0, 0, 0, 12))
)

lty_legend <- get_legend(
  p3 + 
    guides(lty = guide_legend(nrow = 1,override.aes = list(fill = NA))) +
    theme(legend.position = "bottom",
          legend.box.margin = margin(0, 0, 0, 12))
)
fig1_plots <- plot_grid(p1a, p2a, p3a, ncol = 1, align = "v")
fig1_legends <- plot_grid(color_leg, lty_legend, ncol = 1, align = "v", axis = "l")
fig1 <- plot_grid(fig1_plots, fig1_legends, ncol = 1, rel_heights = c(1, .1))
save_plot("figures/asymp-fig-s1.png", fig1, base_height = 8.3, base_asp = 1.4)


# Figure S2-S4 ---------------------------------------------------------------
rnots <- fit_results %>% 
  filter(city != "austin") %>% 
  # filter(tau == "0.44") %>% 
  left_join(pop_size, by = "city") %>%
  inner_join(rho_tbl) %>% 
  select(city, tau, omega, rho, full_df, first_hosp_data, fitted_parms, population, rho_label) %>% 
  unnest(full_df) %>% 
  mutate(rnot_mean = map2(exp(logbetat_mean), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_lo = map2(exp(logbetat_lo), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_hi = map2(exp(logbetat_hi), fitted_parms, calc_ar_rnot) %>% unlist()) %>% 
  filter(date >= first_hosp_data) %>% 
  mutate(rnot_mean = rnot_mean*S_mean/population,
         rnot_hi = rnot_hi*S_mean/population,
         rnot_lo = rnot_lo*S_mean/population) %>% 
  group_by(city) %>% 
  filter(date == "2020-05-01" | date == min(date)) %>% 
  mutate(date_label = ifelse(date == "2020-05-01", "last_day", "first_day")) %>% 
  select(city, tau, omega, rho, rho_label, date_label, rnot_mean) %>% 
  ungroup()



rnot_sensitivity0 <- rnots %>% 
  spread(date_label,rnot_mean) %>% 
  mutate(percent_change = (first_day - last_day)/first_day) %>% 
  mutate(omega_lab = paste0("omega=", omega),
         city = ifelse(city == "utah", "Utah", "NYC")) %>% 
  ggplot(aes(1-tau, percent_change, 
             color = rho_label,
             fill = rho_label)) + 
  geom_point() +
  facet_grid(city~omega_lab, scales = "free") +
  stat_smooth() +
  background_grid(major = "xy", minor = "xy") + 
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme(strip.background = element_rect(fill = NA)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent) +
  labs(color = "Symptomatic\nhospitalization\nrate",
       fill = "Symptomatic\nhospitalization\nrate",
       x = "Asymptomatic rate",
       y = "Reduction in Reproduction Number (Rt)")

save_plot("figures/asymp-fig-s4.png", 
          rnot_sensitivity0,
          base_height = 7,
          base_asp = 1.7)


rnot_sensitivity1 <- rnots %>% 
  spread(date_label,rnot_mean) %>% 
  mutate(percent_change = (first_day - last_day)/first_day) %>% 
  mutate(omega_lab = paste0("omega=", omega),
         city = ifelse(city == "utah", "Utah", "NYC")) %>% 
  filter(first_day < 20) %>% 
  ggplot(aes(1-tau, first_day, 
             color = rho_label,
             fill = rho_label)) + 
  geom_point() +
  facet_grid(city~omega_lab, scales = "free") +
  stat_smooth() +
  background_grid(major = "xy", minor = "xy") + 
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme(strip.background = element_rect(fill = NA)) +
  labs(color = "Symptomatic\nhospitalization\nrate",
       fill = "Symptomatic\nhospitalization\nrate",
       x = "Asymptomatic rate",
       y = "Initial Reproduction Number (Rt)")

save_plot("figures/asymp-fig-s2.png", 
          rnot_sensitivity1,
          base_height = 7,
          base_asp = 1.7)

rnot_sensitivity2 <- rnots %>% 
  spread(date_label,rnot_mean) %>% 
  mutate(percent_change = (first_day - last_day)/first_day) %>% 
  mutate(omega_lab = paste0("omega=", omega),
         city = ifelse(city == "utah", "Utah", "NYC")) %>% 
  ggplot(aes(1-tau, last_day, 
             color = rho_label,
             fill = rho_label)) + 
  geom_point() +
  facet_grid(city~omega_lab, scales = "free") +
  stat_smooth() +
  background_grid(major = "xy", minor = "xy") + 
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme(strip.background = element_rect(fill = NA)) +
  labs(color = "Symptomatic\nhospitalization\nrate",
       fill = "Symptomatic\nhospitalization\nrate",
       x = "Asymptomatic rate",
       y = "Rt on May 1st, 2020")

save_plot("figures/asymp-fig-s3.png", 
          rnot_sensitivity2,
          base_height = 7,
          base_asp = 1.7)




# Figure S5 ----------------------------------------
# nyc_compare <- fit_results %>% 
#   # filter(omega == 0.67) %>% 
#   left_join(pop_size, by = "city") %>% 
#   inner_join(rho_tbl) %>% 
#   mutate(immune_hi = 1 - S_lo/population,
#          immune_mean = 1 - S_mean/population,
#          immune_lo = 1 - S_hi/population,
#          immune_sd = S_sd/population) %>% 
#   mutate(city = ifelse(city == "utah", "Utah", "NYC"),
#          immune_sd = ifelse(immune_sd < 0.0001, 0.0001, immune_sd)) %>% 
#   # filter(immune_hi - immune_lo < 0.05 | tau<0.05) %>% 
#   mutate(asymp_frac = 1-tau)
# 
# 
# immune_data <- nyc_compare %>% 
#   filter(omega == "0.67") %>% 
#   mutate(cumi_mean = (population - S_mean)/population,
#          cumi_upper = (population - S_lo)/population,
#          cumi_lower = (population - S_hi)/population) %>% 
#   select(city, tau, rho, rho_label, cumi_mean:cumi_lower)


immune_data <- fit_results %>% 
  left_join(pop_size, by = "city") %>% 
  inner_join(rho_tbl) %>% 
  filter(city == "utah") %>% 
  select(tau, rho_label, omega) %>% 
  bind_cols(fit_results %>% 
              left_join(pop_size, by = "city") %>% 
              filter(city == "utah") %>% 
              pull(full_df) %>% 
              bind_rows() %>% 
              filter(date == utah_dates[2]) %>% 
              mutate(population = 3205958,
                     immune_hi = 1 - S_lo/population,
                     immune_mean = 1 - S_mean/population,
                     immune_lo = 1 - S_hi/population,
                     immune_sd = S_sd/population) 
  ) %>% 
  select(date, rho_label, tau, omega, immune_lo, immune_hi, immune_mean, population) %>% 
  mutate(city_label = utah_date_label,
         city = "utah") %>% 
  bind_rows(fit_results %>% 
              left_join(pop_size, by = "city") %>% 
              left_join(rho_tbl) %>% 
              filter(city == "nyc") %>% 
              select(tau, rho_label, omega) %>% 
              bind_cols(fit_results %>% 
                          left_join(pop_size, by = "city") %>% 
                          filter(city == "nyc") %>% 
                          pull(full_df) %>% 
                          bind_rows() %>% 
                          filter(date == nyc_dates[2]) %>% 
                          mutate(population = 8398748,
                                 immune_hi = 1 - S_lo/population,
                                 immune_mean = 1 - S_mean/population,
                                 immune_lo = 1 - S_hi/population,
                                 immune_sd = S_sd/population) 
              ) %>% 
              select(date, rho_label, tau, omega, immune_lo, immune_hi, immune_mean, population) %>% 
              mutate(city_label = nyc_date_label,
                     city = "nyc")
  ) %>% 
  mutate(asymp_frac = 1-tau)

immune_data %>% 
  filter(omega == 0.67) %>% 
  mutate(city = factor(city, levels = c("NYC", "Utah")),
         city_label = factor(city_label, levels = factor_date_label)) %>%
  ggplot(aes(x = 1-tau, 
             y = immune_mean, 
             ymin = immune_lo, 
             ymax = immune_hi,
             color = rho_label)) +
  geom_rect(data = nyc_results_rd2 %>% 
              mutate(city_label = factor(nyc_date_label, levels = c(factor_date_label))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_point() +
  geom_errorbar() +
  facet_wrap(~city_label) +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,8,by = 0.2), limits = c(0,0.8), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Assumed asymptomatic rate", 
       y = "Cumulative infections on April 27, 2020", 
       color = "Symptomatic\nhospitalization\nrate") +
  theme(strip.background = element_rect(fill = NA)) +
  annotate("text", x = 0.01, y = 0.61, label = "Herd immunity threshold", hjust = 0, vjust = 0) +
  geom_text(data = tibble(city_label = factor(nyc_date_label, levels = factor_date_label)),
            x = 0.01, y = nyc_results$imax+.02, 
            label = "Seroprevalence estimate",
            hjust = 0, 
            vjust = 0, 
            color = "steelblue4", inherit.aes=FALSE) +
  geom_rect(data = utah_results_rd2 %>% 
              mutate(city_label = factor(utah_date_label, levels = factor_date_label)), aes(xmin = -Inf, 
                                                                                                                                   xmax = Inf, 
                                                                                                                                   ymin = imin, 
                                                                                                                                   ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_text(data = tibble(city_label = factor(utah_date_label, levels = factor_date_label)),
            x = 0.01, y = utah_results_rd4$imax+.02, 
            label = "Seroprevalence estimate",
            hjust = 0, 
            vjust = 0, 
            color = "steelblue4", inherit.aes=FALSE) +
  # geom_rect(data = tibble(xmin = 0.2, 
  #                         xmax = 0.8, 
  #                         ymin = 0, 
  #                         ymax = 0.05, 
  #                         city = "Austin"), 
  #           aes(xmin=xmin, xmax=xmax,ymin=ymin, ymax=ymax), fill = NA,color = "darkgray", inherit.aes = FALSE) +
  # geom_segment(data = tibble(x = 0.2, 
  #                            xend = 0.01, 
  #                            y = 0.05, 
  #                            yend = 0.36, 
  #                            city = "Austin"), 
  #              aes(x=x, xend=xend,y=y, yend=yend), fill = NA, color = "darkgray", inherit.aes = FALSE) +
  # geom_segment(data = tibble(x = 0.8, 
  #                            xend = 0.9, 
  #                            y = 0.05, 
  #                            yend = 0.36, 
  #                            city = "Austin"), 
  #              aes(x=x, xend=xend,y=y, yend=yend), fill = NA, color = "darkgray", inherit.aes = FALSE) +
  background_grid(major = "xy") +
  # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
  scale_color_brewer(type = "seq", palette = 4, direction = 1) -> sensitivity_immune_plot
sensitivity_immune_plot

save_plot("figures/asymp-fig-s5.png", 
          sensitivity_immune_plot, 
          base_height = 6, base_asp = 2)

# immune_data %>% 
#   filter(city == "Austin") %>% 
#   filter(tau >= 0.2, tau <=0.8) %>% 
#   ggplot(aes(x = 1-tau, 
#              y = cumi_mean, 
#              ymin = cumi_lower, 
#              ymax = cumi_upper,
#              color = rho_label)) +
#   geom_point() +
#   geom_errorbar() +
#   geom_hline(yintercept = 0.6, lty = 2) +
#   scale_y_continuous(labels = scales::percent,limits = c(0,.05), expand = expansion()) +
#   scale_x_continuous(labels = scales::percent, expand = expansion()) +
#   # coord_cartesian(xlim = c(0.2, 0.8)) +
#   labs(x = NULL, 
#        y = NULL) +
#   theme(axis.text = element_text(size = 6), legend.position = "none", 
#         plot.background = element_rect(fill = "gray95")) +
#   background_grid(major = "xy", 
#                   minor = "xy",
#                   color.major = "white",
#                   color.minor = "white") +
#   # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
#   scale_color_brewer(type = "seq", palette = 4, direction = 1) -> austin_inset



# save_plot("figures/asymp-fig-s5.png", 
#           ggdraw(sensitivity_immune_plot) +
#             draw_plot(austin_inset, x = 0.087, y = 0.4, width = 0.35, height = 0.5), 
#           base_height = 6, base_asp = 2)


# Figure S6 ---------------------------------------------------------------

nyc_asymp %>% 
  filter(date == nyc_dates[2]) %>% 
  mutate(city = "NYC") %>% 
  bind_rows(utah_asymp %>% 
              filter(date == utah_dates[2]) %>% 
              mutate(city = "Utah")) %>% 
  left_join(pop_size %>% mutate(city = case_when(city =="austin" ~ "Austin",
                                                 city == "nyc" ~ "NYC",
                                                 city == "utah" ~ "Utah"))) %>%
  left_join(rho_tbl %>% select(-city), by = "rho") %>% 
  mutate(city_label = factor(ifelse(city == "NYC", nyc_date_label, utah_date_label), 
                             levels = factor_date_label)) %>% 
  ggplot(aes(rho_label, most_sim_asymp, color = as.factor(omega))) + 
    geom_point(position=position_dodge(width = 1)) +
    geom_errorbar(aes(ymin = asymp_min, ymax = asymp_max), position=position_dodge(width = 1)) +
    facet_wrap(~city_label) +
    labs(x = "Symptomatic hospitalization rate",
         y = "Asymptomatic rate",
         color = "Relative\nasymptomatic\ninfectiousness") +
    scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    theme(strip.background = element_rect(fill=NA)) +
    background_grid(major = "y", 
                    minor = "y") +
    scale_color_brewer(type = "qual", palette = 2) -> fig_s6

save_plot("figures/asymp-fig-s6.png", fig_s6, base_height = 4.5, base_asp = 1.8)

# Figure S7 ---------------------------------------------------------------
# 
# utah_asymp %>% 
#   filter(omega == 0.67) %>% 
#   left_join(rho_tbl %>% select(-city), by = "rho") %>%
#   ggplot(aes(x = rho_label, most_sim_asymp, color = as.factor(date))) + 
#   geom_point(position=position_dodge(width = 1)) +
#   geom_errorbar(aes(ymin=asymp_min, ymax = asymp_max), position=position_dodge(width = 1)) +
#   scale_y_continuous(limits = c(0,1), labels = scales::percent) +
#   labs(x = "Symptomatic hospitalization rate",
#        y = "Asymptomatic rate",
#        color = "Seroprevalence\nStudy Date") +
#   background_grid(major = "y", 
#                   minor = "y") +
#   scale_color_brewer(type = "qual", palette = 2) -> fig_s7
# save_plot("figures/asymp-fig-s7.png", fig_s7, base_height = 4, base_asp = 1.5)
# 
# nyc_asymp %>% 
#   filter(omega == 0.67) %>% 
#   left_join(rho_tbl %>% select(-city), by = "rho") %>%
#   ggplot(aes(x = rho_label, most_sim_asymp, color = as.factor(date))) + 
#   geom_point(position=position_dodge(width = 1)) +
#   geom_errorbar(aes(ymin=asymp_min, ymax = asymp_max), position=position_dodge(width = 1)) +
#   scale_y_continuous(limits = c(0,1), labels = scales::percent) +
#   labs(x = "Symptomatic hospitalization rate",
#        y = "Asymptomatic rate",
#        color = "Seroprevalence\nStudy Date") +
#   background_grid(major = "y", 
#                   minor = "y") +
#   scale_color_brewer(type = "qual", palette = 2) 

nyc_asymp %>% 
  ungroup() %>% 
  group_by(date) %>% 
  summarize(asymp_most_sim = most_sim_asymp[which(omega == 0.67 & rho == 0.0513)],
            asymp_min = min(asymp_min),
            asymp_max = max(asymp_max)) %>% 
  mutate(city = "NYC") %>% 
  bind_rows(utah_asymp %>% 
              ungroup() %>% 
              group_by(date) %>% 
              summarize(asymp_most_sim = most_sim_asymp[which(omega == 0.67 & rho == 0.0412)],
                        asymp_min = min(asymp_min),
                        asymp_max = max(asymp_max)) %>% 
              mutate(city = "Utah")) %>% 
  group_by(city) %>% 
  mutate(study_rd = match(date, table = unique(date) )) %>% 
  ungroup() %>% 
  mutate(study_rd = paste0("Round ", study_rd)) %>% 
  ggplot(aes(date, asymp_most_sim, group = date, color = study_rd)) + 
  geom_point(position=position_dodge(width = 1)) +
  facet_wrap(~city, nrow = 2) + 
  # scale_colour_viridis_c(trans = "date") +
  geom_errorbar(aes(ymin = asymp_min, ymax = asymp_max), position=position_dodge(width = 1)) +
  labs(x = "Seroprevalence Study Date", y = "Estimated Asymptomatic Rates",color = "CDC Estimate") +
  theme(strip.background = element_rect(fill=NA)) +
  background_grid(major = "xy", minor = "y") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_y_continuous(labels = scales::percent) -> fig_s7b

####
city_fit_ts <- fit_results %>% 
  inner_join(nyc_asymp %>% 
               filter(rho == 0.0513, omega == 0.67) %>% 
               rename(estimate_date = date) %>% 
               mutate(tau = 1-most_sim_asymp), by = c("rho", "omega", "tau")) %>% 
  select(city, estimate_date, full_df) %>% 
  unnest(full_df) %>% 
  mutate(immune_hi = 1 - S_lo/pop_size$population[2],
         immune_mean = 1 - S_mean/pop_size$population[2],
         immune_lo = 1 - S_hi/pop_size$population[2]) %>% 
  select(city, estimate_date, date, immune_hi:immune_lo) %>% 
  bind_rows(fit_results %>% 
              inner_join(utah_asymp %>% 
                           filter(rho == 0.0412, omega == 0.67) %>% 
                           rename(estimate_date = date) %>% 
                           mutate(tau = round((1-most_sim_asymp), digits = 2)), by = c("rho", "omega", "tau")) %>% 
              select(city, estimate_date, full_df) %>% 
              unnest(full_df) %>% 
              mutate(immune_hi = 1 - S_lo/pop_size$population[3],
                     immune_mean = 1 - S_mean/pop_size$population[3],
                     immune_lo = 1 - S_hi/pop_size$population[3]) %>% 
              select(city, estimate_date, date, immune_hi:immune_lo))


city_fit_ts %>% 
  mutate(city = ifelse(city == "nyc", "NYC", "Utah")) %>% 
  group_by(city) %>% 
  mutate(study_rd = match(estimate_date, table = unique(estimate_date) )) %>% 
  ungroup() %>% 
  mutate(study_rd = paste0("Round ", study_rd)) %>% 
  ggplot(aes(date, immune_mean, color = study_rd, fill  = study_rd)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = immune_lo, ymax = immune_hi), alpha = .2, color = NA) +
  facet_wrap(~city, scales = "free_y", nrow = 2) +
  geom_point(data = nyc_results_all %>% 
               mutate(study_rd = paste0("Round ", seq_along(date))), aes(date, imean, color = study_rd), inherit.aes=FALSE) +
  geom_point(data = utah_results_all %>% 
               mutate(study_rd = paste0("Round ", seq_along(date))), aes(date, imean, color = study_rd), inherit.aes=FALSE) + 
  geom_errorbar(data = nyc_results_all %>% 
                  mutate(study_rd = paste0("Round ", seq_along(date))), aes(date, ymin = imin, ymax = imax, color = study_rd), inherit.aes=FALSE) +
  geom_errorbar(data = utah_results_all %>% 
                  mutate(study_rd = paste0("Round ", seq_along(date))), aes(date, ymin = imin, ymax = imax, color = study_rd), inherit.aes=FALSE) +
  labs(x = "Date", y = "Cumulative infections", color = "CDC Estimate",fill = "CDC Estimate") +
  background_grid(major = "xy", minor = "y") +
  theme(strip.background = element_rect(fill = NA), legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) -> fig_s7a

s7_legend <- get_legend(
  fig_s7b + 
    guides(lty = guide_legend(nrow = 1,override.aes = list(fill = NA))) +
    theme(legend.box.margin = margin(0, 12, 0, 12))
)

fig_s7 <- plot_grid(fig_s7a, fig_s7b+theme(legend.position="none"), s7_legend, rel_widths = c(1,1, .25), nrow = 1, labels = c("A", "B", ""))

save_plot("figures/asymp-fig-s7.png",fig_s7, base_height = 6, base_asp = 2)

# Pull necessary metrics --------------------------------------------------
## Asymptomatic proportion
nyc_asymp %>% 
  ungroup() %>% 
  filter(date == nyc_dates[2]) %>% 
  summarize(asymp_most_sim = most_sim_asymp[which(omega == 0.67 & rho == 0.0513)],
            asymp_min = min(asymp_min),
            asymp_max = max(asymp_max)) %>% 
  mutate(city = "nyc") %>% 
  bind_rows(utah_asymp %>% 
              ungroup() %>% 
              filter(date == utah_dates[2]) %>% 
              summarize(asymp_most_sim = most_sim_asymp[which(omega == 0.67 & rho == 0.0412)],
                        asymp_min = min(asymp_min),
                        asymp_max = max(asymp_max)) %>% 
              mutate(city = "utah"))

## Total attack rate (mean)
final_immune_states<- fit_results %>% 
  left_join(pop_size, by = "city") %>% 
  mutate(immune_hi = 1 - S_lo/population,
         immune_mean = 1 - S_mean/population,
         immune_lo = 1 - S_hi/population,
         immune_sd = S_sd/population) %>% 
  select(city, tau, rho, mle_rho, omega, immune_hi:immune_sd)

final_immune_states %>% 
  filter(omega == 0.67, rho == mle_rho) %>% 
  filter((tau == .47 & city == "nyc") | (tau == 0.51 & city == "utah")) 

## Total attack rate for each symptomatic hospitalization rate
final_immune_states %>% 
  inner_join(nyc_asymp %>% 
               filter(date == nyc_dates[2]) %>% 
              group_by(rho) %>% 
              summarize(min_asymp =min(asymp_min),
                        max_asymp = max(asymp_max)) %>% 
              mutate(city = "nyc") %>% 
              bind_rows(utah_asymp %>% 
                          filter(date == utah_dates[2]) %>% 
                          group_by(rho) %>% 
                          summarize(min_asymp =min(asymp_min),
                                    max_asymp = max(asymp_max)) %>% 
                          mutate(city = "utah")), by = c("rho", "city")) %>% 
  filter( (1-tau)>=min_asymp, (1-tau)<max_asymp ) %>% 
  group_by(city, rho) %>% 
  summarize(min(immune_lo), max(immune_hi))
  

## Get initial R0s
mle_rnot_df <- fit_results %>% 
  filter((tau == .47 & city == "nyc") | (tau == 0.51 & city == "utah")) %>% 
  filter(omega == 0.67) %>% 
  left_join(pop_size, by = "city") %>% 
  filter(rho == mle_rho) %>%
  mutate(asymp_frac = 1-tau) %>% 
  select(city, tau, omega, rho, asymp_frac, full_df, first_hosp_data, fitted_parms, population) %>% 
  unnest(full_df) %>% 
  mutate(rnot_mean = map2(exp(logbetat_mean), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_lo = map2(exp(logbetat_lo), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_hi = map2(exp(logbetat_hi), fitted_parms, calc_ar_rnot) %>% unlist()) %>% 
  filter(date >= first_hosp_data) %>%
  mutate(rnot_mean = rnot_mean*S_mean/population,
         rnot_hi = rnot_hi*S_mean/population,
         rnot_lo = rnot_lo*S_mean/population)

mle_rnot_df %>% 
  group_by(city) %>% 
  filter(date == min(date)) %>% 
  select(rnot_mean:rnot_hi)

## Avereage R0 across initial time period
init_rnots <- fit_results %>% 
  filter((tau == .47 & city == "nyc") | (tau == 0.51 & city == "utah")) %>% 
  filter(omega == 0.67) %>% 
  left_join(pop_size, by = "city") %>% 
  filter(rho == mle_rho) %>%
  mutate(asymp_frac = 1-tau) %>% 
  select(city, tau, omega, rho, asymp_frac, full_df, first_hosp_data, fitted_parms, population) %>% 
  unnest(full_df) %>% 
  mutate(rnot_mean = map2(exp(logbetat_mean), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_lo = map2(exp(logbetat_lo), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_hi = map2(exp(logbetat_hi), fitted_parms, calc_ar_rnot) %>% unlist()) %>% 
  filter(date < first_hosp_data) %>%
  mutate(rnot_mean = rnot_mean*S_mean/population,
         rnot_hi = rnot_hi*S_mean/population,
         rnot_lo = rnot_lo*S_mean/population) %>% 
  select(city, date, rnot_mean:rnot_hi)

init_rnots %>% 
  select(-date) %>% 
  group_by(city) %>% 
  summarize_all(mean)


  
## NYC Reporting rate
# (214459 / pop_size$population[2]) / c(.381, .337, .421)
(214459 / pop_size$population[2]) / c(.268, .221, .32)

## Utah cases
read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  filter(state == "Utah") %>% 
  group_by(date) %>%
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>%
  filter(date < "2020-07-01") %>% tail()
(22364/pop_size$population[3]) / c(.027, .012, .063)



# Look at the fits --------------------------------------------------------

accepted_results <- fit_results %>% 
  filter((mle > -660 & city == "nyc") | (mle > -335 & city == "utah"))
accepted_results %>%   
  ggplot(aes(tau, mle, color = as.factor(rho))) + 
    geom_point() + 
    facet_wrap(~city, scales = "free_y")

accepted_results %>% 
  group_by(city, rho) %>% 
  summarize(1-min(tau), 1 - max(tau))

## Austin Reporting rate
# austin_msa <- tibble(fips = c('48021', '48055', '48209', '48453', '48491'))
# read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
#   inner_join(austin_msa, by = "fips") %>% 
#   group_by(date) %>% 
#   summarize(cases = sum(cases),
#             deaths = sum(deaths)) %>% 
#   filter(date < "2020-04-28") %>% tail()
# (1976/pop_size$population[1]) / c(.0094, .0072, .0126) *100
# 

# ## Symptomatic proportion necesssary to reach herd immunity
# nyc_compare <- fit_results %>% 
#   # filter(omega == 0.67) %>% 
#   left_join(pop_size, by = "city") %>% 
#   inner_join(rho_tbl) %>% 
#   mutate(immune_hi = 1 - S_lo/population,
#          immune_mean = 1 - S_mean/population,
#          immune_lo = 1 - S_hi/population,
#          immune_sd = S_sd/population) %>% 
#   mutate(city = ifelse(city == "austin", "Austin", "NYC"),
#          immune_sd = ifelse(immune_sd < 0.0001, 0.0001, immune_sd)) %>% 
#   # filter(immune_hi - immune_lo < 0.05 | tau<0.05) %>% 
#   mutate(asymp_frac = 1-tau)
# 
# nyc_compare %>% 
#   filter(immune_hi>0.6) %>% 
#   group_by(city, rho, omega) %>% 
#   summarize(min(1-tau))
# 
# ## Austin populatinon infected by April 27th
# asymp_frac_est_by_rho <- nyc_compare %>% 
#   select(rho, omega, tau, immune_lo, immune_hi, immune_mean) %>% 
#   filter((immune_lo <= nyc_results$imin & immune_hi >= nyc_results$imin) | 
#            (immune_lo <= nyc_results$imax & immune_hi >= nyc_results$imax))  %>% 
#   mutate(immune_diff = abs(immune_mean - nyc_results$imean)) %>% 
#   filter(omega == 0.67) %>% 
#   group_by(rho) %>% 
#   summarize(asymp_min = 1 - max(tau),
#             asymp_max = 1 - min(tau),
#             asymp_median = 1 - median(tau),
#             most_sim_asymp = 1 - tau[which.min(immune_diff)])
# 
# 
# ## Estimated immunity levels for each hospitalization rate
# nyc_compare %>% 
#   filter(city == "Austin", ) %>% 
#   select(asymp_frac, omega, rho_label, immune_mean, immune_lo,immune_hi) %>% 
#   inner_join(asymp_frac_est_by_rho %>% 
#                select(-asymp_median) %>% 
#                gather(key, value, asymp_min:most_sim_asymp) %>% 
#                arrange(rho), c("rho_label" = "rho_label" , "asymp_frac" = "value")) %>% 
#   filter(omega == 0.67) %>% 
#   group_by(rho_label) %>% 
#   summarize(lowest_immunity = min(immune_lo),
#             mle_immunity = immune_mean[which(key == "most_sim_asymp")],
#             largest_immunity = max(immune_hi)
#   )
