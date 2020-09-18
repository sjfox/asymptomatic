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
                   best_fit_days_back = c(20, 28, 20),
                   first_hosp_data = lubridate::ymd(c("2020-03-10", "2020-03-03", "2020-03-07")),
                   mle_rho = c(0.0443, 0.0513, .0412))

rho_tbl <- tibble(city = rep(c("austin", "nyc", "utah"), each = 3),
                  rho = c(0.0443, 0.0264, 0.0905, 0.0513, 0.0305, 0.105, 0.0412, 0.0245, 0.0841),
                  rho_label = factor(rep(c("Mean", "Lowerbound", "Upperbound"), 3),
                                     levels = c("Lowerbound", "Mean", "Upperbound")))

## folder_with_fits <- "processed_data/2020-05-18-16_mif-fits/" ## This one is with nbinom and fixed AR process
folder_with_fits <- "processed_data/2020-09-16-23_mif-fits/" ## Prior on sd of AR and nbinom dispersion fitting for NYC
vec <- list.files(folder_with_fits, full.names = T)
vec %>% length()

fit_results <- vec %>% 
  purrr::map(read_fitting_data)
fit_results <- fit_results[!purrr::map(fit_results, 
                                       function(x){any(class(x) == "logical")}) %>% 
                             unlist()] %>% 
  bind_rows()


# Get city data -----------------------------------------------------------
austin_msa <- tibble(fips = c('48021', '48055', '48209', '48453', '48491'))
nyc_fips <- "000001"
kc_fips <- "000002"
county_data <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
  mutate(fips = ifelse(county == "New York City", nyc_fips, fips),
         fips = ifelse(county == "Kansas City", kc_fips, fips)) 

## GET Austin daily reported cases
city_data <- county_data %>% 
  left_join(austin_msa %>% mutate(city = "austin"), by = "fips") %>% 
  filter(city == "austin" | fips == nyc_fips) %>% 
  mutate(city = ifelse(is.na(city), "nyc", city)) %>% 
  group_by(date, city) %>% 
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  filter(date < "2020-04-28") 

## NYC Serology results
percent_positive <- 0.227
# 95% CI
nyc_results <- tibble(immune_fraction = c(percent_positive, 0.215, 0.24),
                      label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "NYC")


utah_results <- tibble(immune_fraction = c(0.022, 0.012, 0.034),
                       label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "Utah")


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
         rnot_lo = rnot_lo*S_mean/population)

nyc_compare <- fit_results %>% 
  # filter(omega == 0.67) %>% 
  left_join(pop_size, by = "city") %>% 
  inner_join(rho_tbl) %>% 
  mutate(immune_hi = 1 - S_lo/population,
         immune_mean = 1 - S_mean/population,
         immune_lo = 1 - S_hi/population,
         immune_sd = S_sd/population) %>% 
  mutate(city = case_when(city == "austin" ~ "Austin", 
                          city == "nyc"~"NYC",
                          city == "utah" ~ "Utah"),
         immune_sd = ifelse(immune_sd < 0.0001, 0.0001, immune_sd)) %>% 
  # filter(immune_hi - immune_lo < 0.05 | tau<0.05) %>% 
  mutate(asymp_frac = 1-tau)

nyc_asymp <- nyc_compare %>% 
  select(city, tau, rho, omega, immune_lo, immune_hi, immune_mean) %>% 
  filter(city == "NYC") %>% 
  filter((immune_lo <= nyc_results$imin & immune_hi >= nyc_results$imin) |
           (immune_lo <= nyc_results$imax & immune_hi >= nyc_results$imax) |
           (immune_lo >= nyc_results$imin & immune_hi <= nyc_results$imax)
  )  %>% 
  mutate(immune_diff = abs(immune_mean - percent_positive)) %>% 
  group_by(rho) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)])
nyc_asymp

immune_data <- nyc_compare %>% 
  filter(omega == "0.67",
         rho == mle_rho,
         tau >= 0.1) %>% 
  select(city, tau, Susceptible = S_mean, population) %>% 
  mutate(Symptomatic = (population - Susceptible)*tau/population,
         Asymptomatic = (population - Susceptible - Symptomatic)/population
  ) %>% 
  mutate(Symptomatic_lower = 0,
         Symptomatic_upper = Symptomatic,
         Asymptomatic_lower = Symptomatic,
         Asymptomatic_upper = Asymptomatic) %>% 
  select(-Susceptible, - population, - Asymptomatic, -Symptomatic) %>% 
  gather(key, value, Symptomatic_lower:Asymptomatic_upper) %>% 
  separate(col = "key", into = c("key", "ribbon_side"), sep = "_") %>% 
  spread(ribbon_side, value)

fig1_fits <- rnot_df %>% 
  # filter(city == city_name) %>% 
  filter(tau == 0.12) %>% 
  ggplot(aes(date, new_admits)) +
  geom_line(data = rnot_df, aes(date, NH_mean, color = study)) +
  geom_ribbon(data = rnot_df, aes(date, ymin = NH_lo, ymax = NH_hi, fill = study), alpha = .2) +
  geom_point() +
  facet_wrap(~city, scales = "free", ncol = 1)+
  scale_fill_brewer(type = "qual", palette = 2)+
  scale_color_brewer(type = "qual", palette = 2)+
  background_grid(major = "xy", minor = "xy") +
  theme(strip.background = element_rect(fill = NA), legend.position = "bottom") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d", limits = c(min(rnot_df$date), max(rnot_df$date))) +
  labs(y = "Hospital admissions", x = "Date", color = "Asymptomatic rate:", fill = "Asymptomatic rate:") 




fig1_immunity <- immune_data %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible"))),
         city = factor(city, levels = c("Austin", "NYC"))) %>%
  ggplot(aes(x = 1-tau, ymin = lower, ymax = upper,
             fill = key)) +
  geom_ribbon() +
  facet_wrap(~city, ncol = 1, scales = "free_y") +
  geom_rect(data = nyc_results %>% 
              mutate(city = factor(city, levels = c("Austin", "NYC"))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_hline(yintercept = 0.6, lty = 2) +
  # geom_rect(data = nyc_asymp %>% filter(rho == 0.0513),
  #           aes(ymin = -Inf, ymax = Inf, xmin = asymp_min, xmax = asymp_max),
  #           inherit.aes=FALSE,
  #           alpha = .2)+
  geom_errorbarh(data = nyc_asymp %>% 
                   filter(rho == 0.0513) %>% mutate(city = "NYC"), height = 0.02,
            aes(y = nyc_results$imax+.02, xmin = asymp_min, xmax = asymp_max),
            inherit.aes=FALSE)+
  geom_point(data = nyc_asymp %>% 
                   filter(rho == 0.0513)%>% mutate(city = "NYC"), 
                 aes(y = nyc_results$imax+.02, x = most_sim_asymp),
                 inherit.aes=FALSE)+
  # geom_vline(xintercept = nyc_asymp %>% 
  #                  filter(rho == 0.0513) %>% 
  #                  pull(asymp_min), lty = 3) +
  # geom_vline(xintercept = nyc_asymp %>% 
  #              filter(rho == 0.0513) %>% 
  #              pull(asymp_max), lty = 3) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic rate", 
       y = "Cumulative infections", 
       fill = "Infection type:") +
  theme(strip.background = element_rect(fill = NA), legend.position = "bottom") +
  annotate("text", x = 0.01, y = 0.61, label = "Herd immunity threshold", hjust = 0, vjust = 0) +
  geom_text(data = tibble(city = factor("NYC", levels = c("NYC", "Austin"))),
            x = 0.01, y = nyc_results$imax+.01, 
            label = "Seroprevalence estimate",
            hjust = 0, 
            vjust = 0, 
            color = "steelblue4", inherit.aes=FALSE) +
  # background_grid(major = "xy") +
  # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
  # scale_fill_brewer(type = "qual", palette = 2, direction = -1) 
  scale_fill_manual(values = c("gray", "darkslategrey"))


ms_fig1 <- plot_grid(fig1_fits, fig1_immunity, align = "h", ncol = 2, labels = c("A", "B"))
save_plot("figures/asymp-fig1.png", ms_fig1, base_height = 7.5, base_asp = 1.7)

## Save into separate for different panels
save_plot("figures/asymp-fig1A.png", fig1_fits, base_height = 6, base_asp = 1.3)
save_plot("figures/asymp-fig1B.png", fig1_immunity, base_height = 6, base_asp = 1.1)



# Look at Utah data -------------------------------------------------------
cdc_utah <- tibble(immune_fraction = c(0.011, 0.006, 0.021),
                       label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "Utah")


utah_compare <- fit_results %>% 
  # filter(omega == 0.67) %>% 
  left_join(pop_size, by = "city") %>% 
  inner_join(rho_tbl) %>% 
  mutate(immune_hi = 1 - S_lo/population,
         immune_mean = 1 - S_mean/population,
         immune_lo = 1 - S_hi/population,
         immune_sd = S_sd/population) %>% 
  mutate(#city = ifelse(city == "austin", "Austin", "NYC"),
         immune_sd = ifelse(immune_sd < 0.0001, 0.0001, immune_sd)) %>%
  # filter(immune_hi - immune_lo < 0.05 | tau<0.05) %>% 
  mutate(asymp_frac = 1-tau)

utah_compare %>% 
  select(city, tau, rho, omega, immune_lo, immune_hi, immune_mean) %>% 
  filter(city == "utah") %>% 
  filter((immune_lo <= cdc_utah$imin & immune_hi >= cdc_utah$imin) |
           (immune_lo <= cdc_utah$imax & immune_hi >= cdc_utah$imax) |
           (immune_lo >= cdc_utah$imin & immune_hi <= cdc_utah$imax)
  )  %>%
  ggplot(aes(1-tau, immune_mean, color = rho))   + 
  annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = cdc_utah$imin, ymax = cdc_utah$imax, inherit.aes=FALSE, alpha = .3) +
  geom_point() +
  geom_errorbar(aes(ymin = immune_lo, ymax = immune_hi)) 
  
utah_compare %>% 
  select(city, tau, rho, omega, immune_lo, immune_hi, immune_mean) %>% 
  filter(city == "utah") %>% 
  filter((immune_lo <= cdc_utah$imin & immune_hi >= cdc_utah$imin) |
           (immune_lo <= cdc_utah$imax & immune_hi >= cdc_utah$imax) |
           (immune_lo >= cdc_utah$imin & immune_hi <= cdc_utah$imax)
  )  %>%
  mutate(immune_diff = abs(immune_mean - cdc_utah$imean)) %>%
  group_by(rho) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)])




# # Compare with CDC study: 
# nyc_results
# cdc_nyc <- tibble(imax = .089,
#                   imin = .05,
#                   imean = .069)
# 
# nyc_dates <- c("2020-03-16", "2020-03-24", "2020-03-28")
# nyc_immune_cdc <- fit_results %>% 
#   filter(city == "nyc") %>% 
#   select(tau, rho, omega) %>% 
#   bind_cols(fit_results %>% 
#               filter(city == "nyc") %>% 
#               pull(full_df) %>% 
#               bind_rows() %>% 
#               filter(date %in% lubridate::ymd(nyc_dates)) %>% 
#               mutate(immune_hi = 1 - S_lo/8398748,
#                      immune_mean = 1 - S_mean/8398748,
#                      immune_lo = 1 - S_hi/8398748,
#                      immune_sd = S_sd/8398748) %>% 
#               mutate(var = rep(seq(1:(n()/3)), each = 3)) %>% 
#               nest(data = c(date, hospitalized, new_admits, new_discharges, zt, day, logbetat_mean, 
#                      logbetat_sd, logbetat_lo, logbetat_hi, H_mean, H_sd, H_lo, 
#                      H_hi, S_mean, S_sd, S_lo, S_hi, NI_mean, NI_sd, NI_lo, NI_hi, 
#                      IA_mean, IA_sd, IA_lo, IA_hi, IY_mean, IY_sd, IY_lo, IY_hi, 
#                      NH_mean, NH_sd, NH_lo, NH_hi, LH_mean, LH_sd, LH_lo, LH_hi, 
#                      D_mean, D_sd, D_lo, D_hi, R_mean, R_sd, R_lo, R_hi, immune_hi, 
#                      immune_mean, immune_lo, immune_sd))
#               ) %>% 
#   unnest(data) %>% 
#   select(date, tau, rho, omega, immune_lo, immune_hi, immune_mean) 
#   
# nyc_immune_cdc %>% 
#   ggplot(aes(tau, immune_mean, color = rho))   + 
#   annotate("rect",xmin = 0, xmax = 1, ymin = cdc_nyc$imin, ymax = cdc_nyc$imax, alpha = .2) +
#   geom_point() +
#   geom_errorbar(aes(ymin = immune_lo, ymax = immune_hi)) +
#   facet_wrap(~date, scales = "free")
# 
# 
# nyc_immune_cdc %>% 
#   filter((immune_lo <= cdc_nyc$imin & immune_hi >= cdc_nyc$imin) |
#            (immune_lo <= cdc_nyc$imax & immune_hi >= cdc_nyc$imax) |
#            (immune_lo >= cdc_nyc$imin & immune_hi <= cdc_nyc$imax)
#            )  %>%
#   mutate(immune_diff = abs(immune_mean - cdc_nyc$imean)) %>%
#   group_by(date, rho) %>% 
#   summarize(asymp_min = 1 - max(tau),
#             asymp_max = 1 - min(tau),
#             asymp_median = 1 - median(tau),
#             most_sim_asymp = 1 - tau[which.min(immune_diff)])

## Plot for Utah

cdc_utah <- utah_results
utah_dates <- c("2020-04-13", "2020-04-21", "2020-04-27")
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
              mutate(var = rep(seq(1:(n()/3)), each = 3)) %>% 
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

utah_immune_cdc %>% 
  ggplot(aes(tau, immune_mean, color = rho))   + 
  annotate("rect",xmin = 0, xmax = 1, ymin = cdc_utah$imin, ymax = cdc_utah$imax, alpha = .2) +
  geom_point() +
  geom_errorbar(aes(ymin = immune_lo, ymax = immune_hi)) +
  facet_wrap(~date, scales = "free")

utah_immune_cdc %>% 
  filter((immune_lo <= cdc_utah$imin & immune_hi >= cdc_utah$imin) |
           (immune_lo <= cdc_utah$imax & immune_hi >= cdc_utah$imax) |
           (immune_lo >= cdc_utah$imin & immune_hi <= cdc_utah$imax)
  )  %>%
  mutate(immune_diff = abs(immune_mean - cdc_utah$imean)) %>%
  group_by(date, rho) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)])

# Figure S1 ---------------------------------------------------------------
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
  mutate(rnot_mean = rnot_mean*S_mean/population,
         rnot_hi = rnot_hi*S_mean/population,
         rnot_lo = rnot_lo*S_mean/population)

p1 <- rnot_df %>% 
  mutate(city = ifelse(city == "austin", "Austin", "NYC")) %>% 
  ggplot(aes(date, rnot_mean, color = study, fill = study)) + 
  geom_ribbon(aes(ymin = rnot_lo, ymax = rnot_hi), alpha = .1, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~city, scales = "free_x") +
  scale_fill_brewer(type = "qual", palette = 2)+
  scale_color_brewer(type = "qual", palette = 2)+
  labs(x ="", y = expression(R[t]), 
       fill = "Asymptomatic Proportion:", 
       color = "Asymptomatic Proportion:") +
  background_grid(major = "xy", minor = "xy") +
  geom_hline(yintercept = 1, lty = 3, size = 1) +
  ylim(0,30) +
  theme(strip.background = element_rect(fill = NA), strip.text = element_text(face = "bold", size = 18)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") 
p1a <- p1 + theme(legend.position = "none")
p2 <- rnot_df %>% 
  # filter(city == city_name) %>% 
  filter(tau == 0.12) %>% 
  ggplot(aes(date, new_admits)) +
  geom_line(data = rnot_df, aes(date, NH_mean, color = study)) +
  geom_ribbon(data = rnot_df, aes(date, ymin = NH_lo, ymax = NH_hi, fill = study), alpha = .2) +
  geom_point() +
  facet_wrap(~city, scales = "free")+
  scale_fill_brewer(type = "qual", palette = 2)+
  scale_color_brewer(type = "qual", palette = 2)+
  background_grid(major = "xy", minor = "xy") +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
  labs(y = "Hospital admissions", x = "") 
p2a <- p2 + theme(legend.position = "none")

p3 <- rnot_df %>% 
  left_join(city_data, by = c("date", "city")) %>% 
  mutate(city = ifelse(city == "austin", "Austin", "NYC")) %>% 
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
  facet_wrap(~city, scales = "free") +
  scale_color_brewer(type = "qual", palette = 2)+
  scale_fill_brewer(type = "qual", palette = 2)+
  labs(x ="", y = "Cumulative infections", 
       fill = "Asymptomatic\nProportion", 
       color = "Asymptomatic\nProportion",
       lty = "Infection type:") +
  guides(fill=FALSE, color = FALSE) +
  background_grid(major = "xy", minor = "xy") +
  # geom_hline(yintercept = 1, lty = 2) +
  # ylim(0,30) +
  scale_y_continuous(labels = scales::comma) +
  theme(strip.background = element_rect(fill = NA), strip.text = element_blank()) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") 
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
  filter(date == max(date) | date == min(date)) %>% 
  mutate(date_label = ifelse(date == max(date), "last_day", "first_day")) %>% 
  select(city, tau, omega, rho, rho_label, date_label, rnot_mean) %>% 
  ungroup()



rnot_sensitivity0 <- rnots %>% 
  spread(date_label,rnot_mean) %>% 
  mutate(percent_change = (first_day - last_day)/first_day) %>% 
  mutate(omega_lab = paste0("omega=", omega),
         city = ifelse(city == "austin", "Austin", "NYC")) %>% 
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
  scale_y_continuous(labels = scales::percent) +
  labs(color = "Symptomatic\nhospitalization\nrate",
       fill = "Symptomatic\nhospitalization\nrate",
       x = "Asymptomatic fraction",
       y = "Change in Rt")

save_plot("figures/asymp-fig-s2.png", 
          rnot_sensitivity0,
          base_height = 7,
          base_asp = 1.6)


rnot_sensitivity1 <- rnots %>% 
  spread(date_label,rnot_mean) %>% 
  mutate(percent_change = (first_day - last_day)/first_day) %>% 
  mutate(omega_lab = paste0("omega=", omega),
         city = ifelse(city == "austin", "Austin", "NYC")) %>% 
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
       x = "Asymptomatic fraction",
       y = "Initial Rt")

save_plot("figures/asymp-fig-s3.png", 
          rnot_sensitivity1,
          base_height = 7,
          base_asp = 1.6)

rnot_sensitivity2 <- rnots %>% 
  spread(date_label,rnot_mean) %>% 
  mutate(percent_change = (first_day - last_day)/first_day) %>% 
  mutate(omega_lab = paste0("omega=", omega),
         city = ifelse(city == "austin", "Austin", "NYC")) %>% 
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
       x = "Asymptomatic fraction",
       y = "Final Rt")

save_plot("figures/asymp-fig-s4.png", 
          rnot_sensitivity2,
          base_height = 7,
          base_asp = 1.6)




# Figure S5 ----------------------------------------
nyc_compare <- fit_results %>% 
  # filter(omega == 0.67) %>% 
  left_join(pop_size, by = "city") %>% 
  inner_join(rho_tbl) %>% 
  mutate(immune_hi = 1 - S_lo/population,
         immune_mean = 1 - S_mean/population,
         immune_lo = 1 - S_hi/population,
         immune_sd = S_sd/population) %>% 
  mutate(city = ifelse(city == "austin", "Austin", "NYC"),
         immune_sd = ifelse(immune_sd < 0.0001, 0.0001, immune_sd)) %>% 
  # filter(immune_hi - immune_lo < 0.05 | tau<0.05) %>% 
  mutate(asymp_frac = 1-tau)


immune_data <- nyc_compare %>% 
  filter(omega == "0.67") %>% 
  mutate(cumi_mean = (population - S_mean)/population,
         cumi_upper = (population - S_lo)/population,
         cumi_lower = (population - S_hi)/population) %>% 
  select(city, tau, rho, rho_label, cumi_mean:cumi_lower)

immune_data %>% 
  mutate(city = factor(city, levels = c("Austin", "NYC"))) %>%
  ggplot(aes(x = 1-tau, 
             y = cumi_mean, 
             ymin = cumi_lower, 
             ymax = cumi_upper,
             color = rho_label)) +
  geom_rect(data = nyc_results %>% 
              mutate(city = factor(city, levels = c("Austin", "NYC"))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_point() +
  geom_errorbar() +
  facet_wrap(~city) +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Assumed asymptomatic rate", 
       y = "Cumulative infections on April 27, 2020", 
       color = "Symptomatic\nhospitalization\nrate") +
  theme(strip.background = element_rect(fill = NA)) +
  annotate("text", x = 0.01, y = 0.61, label = "Herd immunity threshold", hjust = 0, vjust = 0) +
  geom_text(data = tibble(city = factor("NYC", levels = c("NYC", "Austin"))),
            x = 0.01, y = nyc_results$imax+.05, 
            label = "Seroprevalence estimate",
            hjust = 0, 
            vjust = 0, 
            color = "steelblue4", inherit.aes=FALSE) +
  geom_rect(data = tibble(xmin = 0.2, 
                          xmax = 0.8, 
                          ymin = 0, 
                          ymax = 0.05, 
                          city = "Austin"), 
            aes(xmin=xmin, xmax=xmax,ymin=ymin, ymax=ymax), fill = NA,color = "darkgray", inherit.aes = FALSE) +
  geom_segment(data = tibble(x = 0.2, 
                             xend = 0.01, 
                             y = 0.05, 
                             yend = 0.36, 
                             city = "Austin"), 
               aes(x=x, xend=xend,y=y, yend=yend), fill = NA, color = "darkgray", inherit.aes = FALSE) +
  geom_segment(data = tibble(x = 0.8, 
                             xend = 0.9, 
                             y = 0.05, 
                             yend = 0.36, 
                             city = "Austin"), 
               aes(x=x, xend=xend,y=y, yend=yend), fill = NA, color = "darkgray", inherit.aes = FALSE) +
  background_grid(major = "xy") +
  # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
  scale_color_brewer(type = "seq", palette = 4, direction = 1) -> sensitivity_immune_plot

immune_data %>% 
  filter(city == "Austin") %>% 
  filter(tau >= 0.2, tau <=0.8) %>% 
  ggplot(aes(x = 1-tau, 
             y = cumi_mean, 
             ymin = cumi_lower, 
             ymax = cumi_upper,
             color = rho_label)) +
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent,limits = c(0,.05), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  # coord_cartesian(xlim = c(0.2, 0.8)) +
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text = element_text(size = 6), legend.position = "none", 
        plot.background = element_rect(fill = "gray95")) +
  background_grid(major = "xy", 
                  minor = "xy",
                  color.major = "white",
                  color.minor = "white") +
  # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
  scale_color_brewer(type = "seq", palette = 4, direction = 1) -> austin_inset



save_plot("figures/asymp-fig-s5.png", 
          ggdraw(sensitivity_immune_plot) +
            draw_plot(austin_inset, x = 0.087, y = 0.4, width = 0.35, height = 0.5), 
          base_height = 6, base_asp = 2)


# Figure S6 ---------------------------------------------------------------
nyc_compare <- fit_results %>% 
  # filter(omega == 0.67) %>% 
  left_join(pop_size, by = "city") %>% 
  inner_join(rho_tbl) %>% 
  mutate(immune_hi = 1 - S_lo/population,
         immune_mean = 1 - S_mean/population,
         immune_lo = 1 - S_hi/population,
         immune_sd = S_sd/population) %>% 
  mutate(city = ifelse(city == "austin", "Austin", "NYC"),
         immune_sd = ifelse(immune_sd < 0.0001, 0.0001, immune_sd)) %>% 
  # filter(immune_hi - immune_lo < 0.05 | tau<0.05) %>% 
  mutate(asymp_frac = 1-tau)

nyc_asymp <- nyc_compare %>% 
  select(city, tau,omega, immune_lo, immune_hi, immune_mean) %>% 
  filter(city == "NYC") %>% 
  filter((immune_lo <= nyc_results$imin & immune_hi >= nyc_results$imin) | 
           (immune_lo <= nyc_results$imax & immune_hi >= nyc_results$imax))  %>% 
  mutate(immune_diff = abs(immune_mean - percent_positive)) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)])
nyc_asymp

nyc_compare %>% 
  select(city, rho, omega, tau, immune_lo, immune_hi, immune_mean) %>% 
  filter(city == "NYC") %>% 
  filter((immune_lo <= nyc_results$imin & immune_hi >= nyc_results$imin) | 
           (immune_lo <= nyc_results$imax & immune_hi >= nyc_results$imax))  %>% 
  mutate(immune_diff = abs(immune_mean - percent_positive)) %>% 
  group_by(rho, omega) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)]) %>% 
  ungroup() %>% 
  mutate(rho = scales::percent(rho, accuracy = 0.01),
         rho = factor(rho, levels = c("3.05%", "5.13%", "10.50%")),
         omega = scales::percent(omega),
         omega = factor(omega, levels = c("29%", "67%", "142%"))
  ) %>% 
  ggplot(aes(rho, most_sim_asymp, 
             color = omega)) + 
  geom_point(position=position_dodge(width = 1)) + 
  geom_errorbar(aes(ymin = asymp_min, ymax = asymp_max), position=position_dodge(width = 1)) +
  labs(x = "Symptomatic hospitalization rate",
       y = "Asymptomatic rate",
       color = "Relative\nasymptomatic\ninfectiousness") +
  # color = "Relative\nasymptomatic\ninfectiousness") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  background_grid(major = "y", 
                  minor = "y") +
  scale_color_brewer(type = "qual", palette = 2) -> est_asymp_plot



save_plot("figures/asymp-fig-s6.png", 
          est_asymp_plot, 
          base_height = 4.5, base_asp = 1.4)

# Get metrics for the paper -----------------------------------------------
## Get initial R0s
mle_rnot_df <- fit_results %>% 
  filter(tau == 0.44, omega == 0.67) %>% 
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
  filter(tau == 0.44, omega == 0.67) %>% 
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

## Symptomatic proportion necesssary to reach herd immunity
nyc_compare <- fit_results %>% 
  # filter(omega == 0.67) %>% 
  left_join(pop_size, by = "city") %>% 
  inner_join(rho_tbl) %>% 
  mutate(immune_hi = 1 - S_lo/population,
         immune_mean = 1 - S_mean/population,
         immune_lo = 1 - S_hi/population,
         immune_sd = S_sd/population) %>% 
  mutate(city = ifelse(city == "austin", "Austin", "NYC"),
         immune_sd = ifelse(immune_sd < 0.0001, 0.0001, immune_sd)) %>% 
  # filter(immune_hi - immune_lo < 0.05 | tau<0.05) %>% 
  mutate(asymp_frac = 1-tau)

nyc_compare %>% 
  filter(immune_hi>0.6) %>% 
  group_by(city, rho, omega) %>% 
  summarize(min(1-tau))

## Austin populatinon infected by April 27th

asymp_frac_est_by_rho <-nyc_compare %>% 
  select(city, rho, rho_label, omega, tau, immune_lo, immune_hi, immune_mean) %>% 
  filter(city == "NYC") %>% 
  filter((immune_lo <= nyc_results$imin & immune_hi >= nyc_results$imin) | 
           (immune_lo <= nyc_results$imax & immune_hi >= nyc_results$imax))  %>% 
  mutate(immune_diff = abs(immune_mean - percent_positive)) %>% 
  filter(omega == 0.67) %>% 
  group_by(rho_label) %>% 
  summarize(asymp_min = 1 - max(tau),
            asymp_max = 1 - min(tau),
            asymp_median = 1 - median(tau),
            most_sim_asymp = 1 - tau[which.min(immune_diff)])


## Estimated immunity levels for each hospitalization rate
nyc_compare %>% 
  filter(city == "Austin", ) %>% 
  select(asymp_frac, omega, rho_label, immune_mean, immune_lo,immune_hi) %>% 
  inner_join(asymp_frac_est_by_rho %>% 
               select(-asymp_median) %>% 
               gather(key, value, asymp_min:most_sim_asymp) %>% 
               arrange(rho_label), c("rho_label" = "rho_label" , "asymp_frac" = "value")) %>% 
  filter(omega == 0.67) %>% 
  group_by(rho_label) %>% 
  summarize(lowest_immunity = min(immune_lo),
            mle_immunity = immune_mean[which(key == "most_sim_asymp")],
            largest_immunity = max(immune_hi)
            )
  



## Austin Reporting rate
austin_msa <- tibble(fips = c('48021', '48055', '48209', '48453', '48491'))
read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
  inner_join(austin_msa, by = "fips") %>% 
  group_by(date) %>% 
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  filter(date < "2020-04-28") %>% tail()
(1976/pop_size$population[1]) / c(.0094, .0072, .0126) *100



