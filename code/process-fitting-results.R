###########################################
## Processing fitting results
###########################################
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
pop_size <- tibble(city = c("austin", "nyc"), 
                   population = c(2168317, 8398748),
                   best_fit_days_back = c(20, 28),
                   first_hosp_data = lubridate::ymd(c("2020-03-10", "2020-03-03")),
                   mle_rho = c(0.0443, 0.0513))

rho_tbl <- tibble(city = rep(c("austin", "nyc"), each = 3),
                  rho = c(0.0443, 0.0264, 0.0905, 0.0513, 0.0305, 0.105),
                  rho_label = factor(rep(c("Mean", "Lowerbound", "Upperbound"), 2),
                                     levels = c("Lowerbound", "Mean", "Upperbound")))

## folder_with_fits <- "processed_data/2020-05-18-16_mif-fits/" ## This one is with nbinom and fixed AR process
folder_with_fits <- "processed_data/2020-05-20-01_mif-fits/" ## Prior on sd of AR and nbinom dispersion fitting for NYC
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



# Make fig comparing admits and daily beta --------------------------------
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
save_plot("figures/asymp-fig1.png", fig1, base_height = 8.3, base_asp = 1.4)


# New York results --------------------------------------------------------
## https://www.governor.ny.gov/news/amid-ongoing-covid-19-pandemic-governor-cuomo-announces-results-completed-antibody-testing
## https://www.nytimes.com/2020/04/23/nyregion/coronavirus-antibodies-test-ny.html
# total_pop <- 1300
percent_positive <- 0.227
# 95% CI
nyc_results <- tibble(immune_fraction = c(percent_positive, 0.215, 0.24),
       label = c("imean", "imin", "imax")) %>% 
  spread(label, immune_fraction) %>% mutate(city = "NYC")


# Show the susceptible fraction for all sims v1 ------------------------------
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

immune_data <- nyc_compare %>% 
  filter(omega == "0.67",
         rho == mle_rho) %>% 
  select(city, tau, Susceptible = S_mean, population) %>% 
  mutate(Symptomatic = (population - Susceptible)*tau,
         Asymptomatic = population - Susceptible - Symptomatic
  )

austin_inset <- immune_data %>% 
  filter(city =="Austin") %>% 
  filter((1-tau) >= nyc_asymp$asymp_min, (1-tau) <= nyc_asymp$asymp_max) %>% 
  gather(key, value, Susceptible,Symptomatic,Asymptomatic) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(1-tau, 
                          value/population, 
                          fill = key)) +
  geom_col(width = .01) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0,.03), expand = F) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = NULL, 
       y = NULL, 
       fill = "Infection state") +
  scale_fill_brewer(type = "seq", palette = 1) +
  theme(strip.background = element_rect(fill = NA), 
        legend.position = "none",
        plot.background = element_rect(fill = "lightgray",colour = NA),
        axis.text = element_text(size = 6),
        plot.margin = margin(7,7,7,10, unit = "pt")) 

immune_data %>% 
  gather(key, value, Susceptible,Symptomatic,Asymptomatic) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible"))),
         city = factor(city, levels = c("NYC", "Austin"))) %>%
  ggplot(aes(1-tau, 
             value/population, 
             fill = key)) +
    geom_col(width = .01) +
    facet_wrap(~city) +
    geom_rect(data = nyc_results %>% 
                mutate(city = factor(city, levels = c("NYC", "Austin"))), aes(xmin = -Inf, 
                                      xmax = Inf, 
                                      ymin = imin, 
                                      ymax = imax), alpha=.3, inherit.aes=FALSE) +
    geom_hline(yintercept = 0.6, lty = 2) +
    scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
    scale_x_continuous(labels = scales::percent, expand = expansion()) +
    labs(x = "Asymptomatic proportion", 
         y = "Percent population", 
         fill = "Infection state") +
    theme(strip.background = element_rect(fill = NA), legend.position = "bottom") +
    scale_fill_brewer(type = "seq", palette = 1) -> immune_plot

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
  geom_point(position=position_dodge(width = 1), size = 0.2) + 
  geom_errorbar(aes(ymin = asymp_min, ymax = asymp_max), position=position_dodge(width = 1), size = 0.2) +
  labs(x = "Symptomatic hospitalization rate",
       y = "Asymptomatic proportion",
       color = "Asymptomatic\ninfectiousness") +
       # color = "Relative\nasymptomatic\ninfectiousness") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  # scale_x_discrete(labels = scales::percent) +
  theme(plot.background = element_rect(fill = "lightgray",colour = NA),
        text = element_text(size = 6),
        axis.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.width = unit(0.2, units = "cm"),
        # plot.margin = margin(0,0,0,0, unit = "cm"),
        legend.margin = margin(t = -.45,b = -.2, unit = "cm"),
        
        # legend.box.margin = margin(-1,-1, -1,-1, unit = "cm")
        ) +
  # guides(color = guide_legend(title.position = "left",
  #                             direction = "vertical")) +
  background_grid(major = "y", 
                  minor = "y", 
                  color.major = "white", 
                  color.minor = "white", 
                  size.major = .1,
                  size.minor = .1) +
  scale_color_brewer(type = "qual", palette = 2) -> est_asymp_plot
# est_asymp_plot

fig2 <- ggdraw(immune_plot) + 
  draw_plot(est_asymp_plot, x = .14, y = .45, width =  .27, height =  .45) +
  draw_plot(austin_inset, x = .58, y = .45,width =  .3, height =  .45) +
  draw_label("A", x = 0.1401, y = 0.899, hjust = 0, vjust = 1, size = 12) +
  draw_label("B", x = 0.5801, y = 0.899, hjust = 0, vjust = 1, size = 12)

save_plot("figures/asymp-fig2.png", fig2, base_height = 4.5, base_asp = 1.8)

### V2 of figure ---------------------------------------
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

immune_data <- nyc_compare %>% 
  filter(omega == "0.67",
         rho == mle_rho) %>% 
  select(city, tau, Susceptible = S_mean, population) %>% 
  mutate(Symptomatic = (population - Susceptible)*tau,
         Asymptomatic = population - Susceptible - Symptomatic
  )



## NYC
nyc_baseline <- immune_data %>% 
  filter(city == "NYC", tau >= 0.1) %>%
  mutate(lower = 0,
         symptomatic = Symptomatic/population,
         asymptomatic = symptomatic + Asymptomatic/population,
         final = 1,
         asymp_frac = 1-tau) %>%
  select(asymp_frac, lower, symptomatic, asymptomatic, final)
  
nyc_baseline %>% 
  mutate(key = "Symptomatic") %>% 
  select(key, asymp_frac, lower, upper = symptomatic) %>% 
  bind_rows(nyc_baseline %>% 
              mutate(key = "Asymptomatic") %>% 
              select(key, asymp_frac, lower = symptomatic, upper = asymptomatic),
            nyc_baseline %>% 
              mutate(key = "Susceptible") %>% 
              select(key, asymp_frac, lower = asymptomatic, upper = final)
            ) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(x = asymp_frac, ymin = lower, ymax = upper, fill = key)) +
  geom_ribbon() +
  geom_rect(data = nyc_results %>% 
              mutate(city = factor(city, levels = c("NYC", "Austin"))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic proportion", 
       y = "Percent population", 
       fill = "Infection state") +
  theme(strip.background = element_rect(fill = NA)) +
  annotate("text", x = 0.01, y = 0.61, label = "Herd immunity threshold", hjust = 0, vjust = 0) +
  annotate("text", x = 0.01, y = nyc_results$imax+.01, 
           label = "Seroprevalence estimate", 
           hjust = 0, 
           vjust = 0, 
           color = "steelblue4") +
  # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
  # scale_fill_brewer(type = "qual", palette = 2, direction = -1) 
  scale_fill_manual(values = c("white", "gray", "darkslategrey")) -> immune_plotv2
immune_plotv2

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
       y = "Estimated asymptomatic proportion",
       color = "Asymptomatic\ninfectiousness") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  background_grid(major = "y", 
                  minor = "y", 
                  size.major = .1,
                  size.minor = .1) +
  scale_color_brewer(type = "qual", palette = 2) -> est_asymp_plot
est_asymp_plot

austin_baseline <- immune_data %>% 
  filter(city == "Austin") %>%
  mutate(lower = 0,
         symptomatic = Symptomatic/population,
         asymptomatic = symptomatic + Asymptomatic/population,
         final = 1,
         asymp_frac = 1-tau) %>%
  select(asymp_frac, lower, symptomatic, asymptomatic, final)

austin_baseline %>% 
  mutate(key = "Symptomatic") %>% 
  select(key, asymp_frac, lower, upper = symptomatic) %>% 
  bind_rows(austin_baseline %>% 
              mutate(key = "Asymptomatic") %>% 
              select(key, asymp_frac, lower = symptomatic, upper = asymptomatic),
            austin_baseline %>% 
              mutate(key = "Susceptible") %>% 
              select(key, asymp_frac, lower = asymptomatic, upper = final)
  ) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(x = asymp_frac, ymin = lower, ymax = upper, fill = key)) +
  geom_ribbon() +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion(), breaks = c(0.2,0.4,0.6,0.8)) +
  labs(x = "Asymptomatic proportion", 
       y = "Percent population", 
       fill = "Infection state") +
  # coord_cartesian(xlim = c(0.2, 0.8)) +
  theme(strip.background = element_rect(fill = NA), legend.position = "none") +
  scale_fill_brewer(type = "seq", palette = 1, direction = -1) -> immune_plot_austin
immune_plot_austin 

lg1 <- get_legend(
  # create some space to the left of the legend
  immune_plotv2 + theme(legend.box.margin = margin(50, 12, 0, 12))
) 
lg2 <- get_legend(
  # create some space to the left of the legend
  est_asymp_plot + theme(legend.box.margin = margin(0, 12, 50, 12))
) 
legends <- plot_grid(lg1, lg2, align = "v", ncol = 1)
figs <- plot_grid(immune_plotv2 +theme(legend.position = "none"), 
          est_asymp_plot +theme(legend.position = "none"), 
          immune_plot_austin,
          labels = c("A", "B", "C"),
          rel_widths = c(1,0.7,1),
          nrow = 1)
alt_fig1 <- plot_grid(figs, legends, nrow = 1, rel_widths = c(0.9,0.1))
save_plot("figures/asymp-fig-alt1.png",alt_fig1, base_height = 4.5, base_asp = 3)


# V3 of Figure ------------------------------------------------------------
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


immune_data %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible"))),
         city = factor(city, levels = c("Austin", "NYC"))) %>%
  ggplot(aes(x = 1-tau, ymin = lower, ymax = upper,
             fill = key)) +
  geom_ribbon() +
  facet_wrap(~city) +
  geom_rect(data = nyc_results %>% 
              mutate(city = factor(city, levels = c("Austin", "NYC"))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), 
            alpha=.4, 
            inherit.aes=FALSE,
            fill = "steelblue4") +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Assumed asymptomatic rate", 
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
  background_grid(major = "xy") +
  # geom_text(, label = "Herd immunity threshold",  fontface="plain", family = "sans") +
  # scale_fill_brewer(type = "qual", palette = 2, direction = -1) 
  scale_fill_manual(values = c("gray", "darkslategrey")) -> city_immune_plot

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
       y = "Estimated asymptomatic proportion",
       color = "Asymptomatic infectiousness:") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  background_grid(major = "y", 
                  minor = "y", 
                  size.major = .1,
                  size.minor = .1) +
  theme(legend.position = "bottom") +
  scale_color_brewer(type = "qual", palette = 2) -> est_asymp_plot
est_asymp_plot

ms_fig1 <- plot_grid(
  plot_grid(city_immune_plot + theme(legend.position="none"), 
            est_asymp_plot + theme(legend.position="none"), 
            labels = c("A", "B"),
            axis = "tb",
            align = "h",
            rel_widths = c(1, 0.45)),
  plot_grid(get_legend(city_immune_plot+ theme(legend.box.margin = margin(0, 0, 0, 150))), 
            get_legend(est_asymp_plot + theme(legend.box.margin = margin(0, 50, 0, 50))), ncol = 2),
  ncol = 1, rel_heights = c(0.9, .08)
)
save_plot("figures/asymp-figt1-alt2.png", ms_fig1, base_height = 4.25, base_asp = 2.8)



# Plot for sensitivity of immunity ----------------------------------------
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



save_plot("figures/asymp-sfig-hospitalization-sensitivity.png", 
          ggdraw(sensitivity_immune_plot) +
            draw_plot(austin_inset, x = 0.087, y = 0.4, width = 0.35, height = 0.5), 
          base_height = 6, base_asp = 2)
# Get metrics for the paper -----------------------------------------------
## Symptomatic proportion necesssary to reach herd immunity
nyc_compare %>% 
  filter(immune_hi>0.6) %>% 
  group_by(city, rho, omega) %>% 
  summarize(min(1-tau))

## Austin populatinon infected
nyc_compare %>% 
  filter(city == "Austin") %>% 
  select(asymp_frac, omega, rho, immune_mean, immune_lo,immune_hi) %>% 
  filter(asymp_frac > nyc_asymp$asymp_min, asymp_frac < nyc_asymp$asymp_max) %>% 
  group_by(rho) %>% 
  summarize(min_infected = min(immune_lo),
            max_infected = max(immune_hi),
            MLE = immune_mean[which(asymp_frac == 0.56 & omega == 0.67)]
            )

## Austin Reporting rate
austin_msa <- tibble(fips = c('48021', '48055', '48209', '48453', '48491'))
read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
  inner_join(austin_msa, by = "fips") %>% 
  group_by(date) %>% 
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  filter(date < "2020-04-28") %>% tail()
(1976/pop_size$population[1]) / c(0.00212, 0.0421, 0.00942) *100

### 
## GET Austin daily reported cases


# Comparing R0s across simulations ----------------------------------------
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

save_plot("figures/asymp-fig-s1b.png", 
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

save_plot("figures/asymp-fig-s2.png", 
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

save_plot("figures/asymp-fig-s3.png", 
          rnot_sensitivity2,
          base_height = 7,
          base_asp = 1.6)





# Supplemental plot figure 1 different hospitalization rate ---------------
s_immune_data <- nyc_compare %>% 
  filter(omega == "0.67") %>% 
  select(city, tau, Susceptible = S_mean, population, rho_label) %>% 
  mutate(Symptomatic = (population - Susceptible)*tau,
         Asymptomatic = population - Susceptible - Symptomatic
  )




## NYC
nyc_baseline_lower <- s_immune_data %>% 
  filter(city == "NYC", tau >= 0.15, rho_label == "Lowerbound") %>%
  mutate(lower = 0,
         symptomatic = Symptomatic/population,
         asymptomatic = symptomatic + Asymptomatic/population,
         final = 1,
         asymp_frac = 1-tau) %>%
  select(asymp_frac, lower, symptomatic, asymptomatic, final)

nyc_baseline_lower %>% 
  mutate(key = "Symptomatic") %>% 
  select(key, asymp_frac, lower, upper = symptomatic) %>% 
  bind_rows(nyc_baseline_lower %>% 
              mutate(key = "Asymptomatic") %>% 
              select(key, asymp_frac, lower = symptomatic, upper = asymptomatic),
            nyc_baseline_lower %>% 
              mutate(key = "Susceptible") %>% 
              select(key, asymp_frac, lower = asymptomatic, upper = final)
  ) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(x = asymp_frac, ymin = lower, ymax = upper, fill = key)) +
  geom_ribbon() +
  geom_rect(data = nyc_results %>% 
              mutate(city = factor(city, levels = c("NYC", "Austin"))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), alpha=.3, inherit.aes=FALSE) +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic proportion", 
       y = "Percent population", 
       fill = "Infection state") +
  theme(strip.background = element_rect(fill = NA)) +
  scale_fill_brewer(type = "seq", palette = 1, direction = -1) ->  simmune_plot_lower_nyc
simmune_plot_lower_nyc

austin_baseline_lower <- s_immune_data %>% 
  filter(city == "Austin", rho_label == "Lowerbound") %>%
  mutate(lower = 0,
         symptomatic = Symptomatic/population,
         asymptomatic = symptomatic + Asymptomatic/population,
         final = 1,
         asymp_frac = 1-tau) %>%
  select(asymp_frac, lower, symptomatic, asymptomatic, final)

austin_baseline_lower %>% 
  mutate(key = "Symptomatic") %>% 
  select(key, asymp_frac, lower, upper = symptomatic) %>% 
  bind_rows(austin_baseline_lower %>% 
              mutate(key = "Asymptomatic") %>% 
              select(key, asymp_frac, lower = symptomatic, upper = asymptomatic),
            austin_baseline_lower %>% 
              mutate(key = "Susceptible") %>% 
              select(key, asymp_frac, lower = asymptomatic, upper = final)
  ) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(x = asymp_frac, ymin = lower, ymax = upper, fill = key)) +
  geom_ribbon() +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic proportion", 
       y = "Percent population", 
       fill = "Infection state") +
  theme(strip.background = element_rect(fill = NA)) +
  scale_fill_brewer(type = "seq", palette = 1, direction = -1) ->  simmune_plot_lower_austin
simmune_plot_lower_austin

simmune_lower <- plot_grid( (simmune_plot_lower_nyc+theme(legend.position = "none")), 
          simmune_plot_lower_austin+theme(legend.position = "none"),
          get_legend(simmune_plot_lower_nyc + theme(legend.box.margin = margin(50, 12, 50, 12))) ,
          ncol = 3, rel_widths = c(1, 1, .3),  labels = c("A", "B", ""))
save_plot("figures/asymp-fig-s-immune_low.png", simmune_lower, base_height = 4.5, base_asp = 2.5)


## NYC
nyc_baseline_higher <- s_immune_data %>% 
  filter(city == "NYC", tau >= 0.05, rho_label == "Upperbound") %>%
  mutate(lower = 0,
         symptomatic = Symptomatic/population,
         asymptomatic = symptomatic + Asymptomatic/population,
         final = 1,
         asymp_frac = 1-tau) %>%
  select(asymp_frac, lower, symptomatic, asymptomatic, final)

nyc_baseline_higher %>% 
  mutate(key = "Symptomatic") %>% 
  select(key, asymp_frac, lower, upper = symptomatic) %>% 
  bind_rows(nyc_baseline_higher %>% 
              mutate(key = "Asymptomatic") %>% 
              select(key, asymp_frac, lower = symptomatic, upper = asymptomatic),
            nyc_baseline_higher %>% 
              mutate(key = "Susceptible") %>% 
              select(key, asymp_frac, lower = asymptomatic, upper = final)
  ) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(x = asymp_frac, ymin = lower, ymax = upper, fill = key)) +
  geom_ribbon() +
  geom_rect(data = nyc_results %>% 
              mutate(city = factor(city, levels = c("NYC", "Austin"))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), alpha=.3, inherit.aes=FALSE) +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic proportion", 
       y = "Percent population", 
       fill = "Infection state") +
  theme(strip.background = element_rect(fill = NA)) +
  scale_fill_brewer(type = "seq", palette = 1, direction = -1) ->  simmune_plot_higher_nyc
simmune_plot_higher_nyc

austin_baseline_higher <- s_immune_data %>% 
  filter(city == "Austin", rho_label == "Upperbound") %>%
  mutate(lower = 0,
         symptomatic = Symptomatic/population,
         asymptomatic = symptomatic + Asymptomatic/population,
         final = 1,
         asymp_frac = 1-tau) %>%
  select(asymp_frac, lower, symptomatic, asymptomatic, final)

austin_baseline_higher %>% 
  mutate(key = "Symptomatic") %>% 
  select(key, asymp_frac, lower, upper = symptomatic) %>% 
  bind_rows(austin_baseline_higher %>% 
              mutate(key = "Asymptomatic") %>% 
              select(key, asymp_frac, lower = symptomatic, upper = asymptomatic),
            austin_baseline_higher %>% 
              mutate(key = "Susceptible") %>% 
              select(key, asymp_frac, lower = asymptomatic, upper = final)
  ) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(x = asymp_frac, ymin = lower, ymax = upper, fill = key)) +
  geom_ribbon() +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic proportion", 
       y = "Percent population", 
       fill = "Infection state") +
  theme(strip.background = element_rect(fill = NA)) +
  scale_fill_brewer(type = "seq", palette = 1, direction = -1) ->  simmune_plot_higher_austin
simmune_plot_higher_austin

simmune_higher <- plot_grid( (simmune_plot_higher_nyc+theme(legend.position = "none")), 
                            simmune_plot_higher_austin+theme(legend.position = "none"),
                            get_legend(simmune_plot_higher_nyc + theme(legend.box.margin = margin(50, 12, 50, 12))) ,
                            ncol = 3, rel_widths = c(1, 1, .3), labels = c("A", "B", ""))
save_plot("figures/asymp-fig-s-immune_high.png", simmune_higher, base_height = 4.5, base_asp = 2.5)



austin_inset_s1 <- s_immune_data %>%
  filter(city =="Austin", rho_label == "Lowerbound") %>% 
  filter((1-tau) >= nyc_asymp$asymp_min, (1-tau) <= nyc_asymp$asymp_max) %>% 
  gather(key, value, Susceptible,Symptomatic,Asymptomatic) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(1-tau, 
             value/population, 
             fill = key)) +
  geom_col(width = .01) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0,.04), expand = F) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = NULL, 
       y = NULL, 
       fill = "Infection state") +
  scale_fill_brewer(type = "seq", palette = 1) +
  theme(strip.background = element_rect(fill = NA), 
        legend.position = "none",
        plot.background = element_rect(fill = "lightgray",colour = NA),
        axis.text = element_text(size = 6),
        plot.margin = margin(7,7,7,10, unit = "pt")) 
austin_inset_s1

sfig_immune_low <- ggdraw(simmune_plot) + 
  draw_plot(austin_inset_s1, x = .58, y = .45,width =  .3, height =  .45)
save_plot("figures/asymp-fig-s-immune_low.png", sfig_immune_low, base_height = 4.5, base_asp = 1.8)


## Upperbounds
s_immune_data %>%
  filter(rho_label == "Upperbound") %>% 
  gather(key, value, Susceptible,Symptomatic,Asymptomatic) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible"))),
         city = factor(city, levels = c("NYC", "Austin"))) %>%
  ggplot(aes(1-tau, 
             value/population, 
             fill = key)) +
  geom_col(width = .01) +
  facet_wrap(~city) +
  geom_rect(data = nyc_results %>% 
              mutate(city = factor(city, levels = c("NYC", "Austin"))), aes(xmin = -Inf, 
                                                                            xmax = Inf, 
                                                                            ymin = imin, 
                                                                            ymax = imax), alpha=.3, inherit.aes=FALSE) +
  geom_hline(yintercept = 0.6, lty = 2) +
  scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1), expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Asymptomatic proportion", 
       y = "Percent population", 
       fill = "Infection state") +
  theme(strip.background = element_rect(fill = NA), legend.position = "bottom") +
  scale_fill_brewer(type = "seq", palette = 1) -> simmune_plot
simmune_plot

austin_inset_s1 <- s_immune_data %>%
  filter(city =="Austin", rho_label == "Upperbound") %>% 
  filter((1-tau) >= nyc_asymp$asymp_min, (1-tau) <= nyc_asymp$asymp_max) %>% 
  gather(key, value, Susceptible,Symptomatic,Asymptomatic) %>% 
  mutate(key = factor(key, levels = rev(c("Symptomatic", "Asymptomatic", "Susceptible")))) %>% 
  ggplot(aes(1-tau, 
             value/population, 
             fill = key)) +
  geom_col(width = .01) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0,.015), expand = F) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = NULL, 
       y = NULL, 
       fill = "Infection state") +
  scale_fill_brewer(type = "seq", palette = 1) +
  theme(strip.background = element_rect(fill = NA), 
        legend.position = "none",
        plot.background = element_rect(fill = "lightgray",colour = NA),
        axis.text = element_text(size = 6),
        plot.margin = margin(7,7,7,10, unit = "pt")) 
austin_inset_s1

sfig_immune_low <- ggdraw(simmune_plot) + 
  draw_plot(austin_inset_s1, x = .58, y = .45,width =  .3, height =  .45)
save_plot("figures/asymp-fig-s-immune_high.png", sfig_immune_low, base_height = 4.5, base_asp = 1.8)


# Plot build up of immunity through time -----------------------------------------
df <- fit_results %>% 
  left_join(pop_size, by = "city") %>% 
  filter(city == "nyc") %>% 
  select(full_df, population) %>% 
  mutate(id = seq_along(full_df)) %>% 
  unnest() %>% 
  mutate(immune_mean = 1 - S_mean/population)
  
df %>%  
  group_by(id) %>% 
  mutate(fall_ind = ifelse(tail(immune_mean, 1) > nyc_results$imin & 
                             tail(immune_mean, 1) < nyc_results$imax, "in", "out")) %>% 
  ggplot(aes(date, immune_mean, group = id, color = fall_ind)) + 
  geom_rect(data = nyc_results, aes(xmin = min(df$date),
                                    xmax = max(df$date),
                                    ymin = imin,
                                    ymax = imax), alpha=.3,inherit.aes=FALSE) +
  geom_line(alpha = .1) +
  background_grid(major = "xy", minor ="xy") +
  guides(color = FALSE) +
  scale_color_manual(values = c("black", "grey")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Percent immune")

  


fit_results %>% 
  left_join(pop_size, by = "city") %>% 
  filter(city == "austin", tau == 0.45 | tau == 0.92) %>% 
  select(tau, S_mean, population) %>% 
  mutate(immune = 1 - S_mean/population) %>% 
  arrange(immune)

fit_results %>% 
  filter(tau == 0.5) %>% 
  select(city, omega, rho,full_df, fitted_parms) %>% 
  unnest(full_df) %>% 
  group_by(city, omega, rho) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(init_rnot = map2(exp(logbetat_mean), fitted_parms, calc_ar_rnot) %>% unlist()) %>% 
  ggplot(aes(rho, init_rnot)) + 
    geom_col() +
    facet_grid(omega~city)

# # Compare with loess model ------------------------------------------------
# loess(strac_mean ~ tau, weights=1/(std_err_strac_mean^2))
# mod_df <- fit_results %>% 
#   filter(omega == 0.67, city == "nyc") %>% 
#   left_join(pop_size, by = "city") %>% 
#   mutate(immune_hi = 1 - S_lo/population,
#          immune_mean = 1 - S_mean/population,
#          immune_lo = 1 - S_hi/population,
#          asymp = 1-tau) %>% 
#   mutate(immune_sd = (immune_hi - immune_lo)/3.92) %>% 
#   mutate(immune_sd = ifelse(immune_sd == 0, 0.00001, immune_sd))
# 
# 
# loess(formula = immune_mean ~ asymp, data = mod_df, weights = 1/immune_sd^2)
# 
# mod_df %>% 
#   filter(immune_hi - immune_lo < 0.1) %>% 
#   ggplot(aes(asymp, immune_mean)) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = immune_lo, ymax = immune_hi)) 




# # Print dataset for James -------------------------------------------------
# fit_results %>% 
#   left_join(pop_size, by = "city") %>% 
#   mutate(sfrac_lo = S_lo/population,
#          sfrac_mean = S_mean/population,
#          sfrac_hi = S_hi/population) %>% 
#   select(city, population, tau, omega, S_mean, S_lo,S_hi, S_sd) %>%
# write_csv("processed_data/summary_for_james.csv")



fit_results %>% 
  filter(city == "nyc") %>% 
  filter(mle > -450) %>% 
  select(S_sd, logbetat_mean, logbetat_sd, mle) %>% 
  mutate(log_ssd = log(S_sd)) %>% 
  pairs()

fit_results %>% 
  filter(city == "nyc") %>% 
  filter(S_sd < 149479) %>% 
  ggplot(aes(S_sd)) + 
  geom_histogram(bins = 30) 
  # scale_y_log10() +
  # xlim(c(-400,-350))

fit_results %>% 
  filter(city == "nyc") %>% 
  pull(S_sd) %>% 
  quantile(probs=c(0.1,0.5,0.8,0.9,0.95, 0.975))

fit_results %>% 
  filter(tau == 0.5, omega == 0.67) %>% 
  ggplot(aes(day_to_start, mle)) + 
  geom_point() + 
  facet_wrap(~city, scales = "free_y")



vec[1:2]

fit_results %>% 
  filter(tau == 0.5, omega == 0.67) %>% 
  ggplot(aes(day_to_start, S_mean)) + geom_point() + facet_wrap(~city, scales = "free_y")

vec

betas_to_look_at <- c("processed_data/2020-05-05-01_mif-fits/city=austin_tau=0.5_omega=0.67_startdate=0.rda",
                      "processed_data/2020-05-05-01_mif-fits/city=austin_tau=0.5_omega=0.67_startdate=40.rda")


read_fitting_data <- function(file_path){
  load(file_path)
  parms <- fitted_parms %>% 
    enframe() %>% 
    spread(name,value)
  state_df %>% 
    select(date, betat_mean:betat_hi) %>% 
    mutate(start_date = parms$day_to_start)
}

fit_beta<- betas_to_look_at %>% 
  purrr::map(read_fitting_data) %>% 
  bind_rows()

fit_beta %>%   
  ggplot(aes(date, betat_mean, group = start_date, color = as.factor(start_date), fill = as.factor(start_date))) + 
  geom_line() +
  geom_ribbon(aes(ymin = betat_lo, ymax = betat_hi), alpha = .1)







# Old code ----------------------------------------------------------------
# 
# nyc_compare %>% 
#   filter(omega == "0.67") %>% 
#   mutate(norm_hosp_rate = tau*rho) %>%  
#   ggplot(aes(1-tau, 
#              immune_mean, 
#              color = rho_label)) +
#   # fill = as.factor(round(omega, 2)),) + 
#   geom_rect(data = nyc_results, aes(xmin = -Inf, 
#                                     xmax = Inf, 
#                                     ymin = imin, 
#                                     ymax = imax), alpha=.3,inherit.aes=FALSE) +  
#   geom_hline(yintercept = 0.6, lty = 2) +
#   geom_point(size = 1) + 
#   geom_errorbar(aes(ymin = immune_lo, ymax =immune_hi)) +
#   # geom_ribbon(aes(ymin = immune_lo, ymax =immune_hi),  color = NA, alpha = .1) +
#   facet_wrap(~city, ncol = 2) +
#   scale_y_continuous(labels = scales::percent, breaks= seq(0,1,by = 0.1), limits = c(0,1)) +
#   scale_x_continuous(labels = scales::percent) +
#   labs(x = "Asymptomatic proportion", 
#        y = "Percent immune", 
#        color = "Symptomatic hospitalization rate") +
#   # fill = "Relative\nasymptomatic\ninfectiousness") +
#   background_grid(major = "xy", minor="xy") + 
#   theme(strip.background = element_rect(fill = NA), legend.position = "bottom") +
#   scale_color_brewer(type = "seq", palette = 1) -> immune_plot
# fig2 <- ggdraw(immune_plot) + 
#   draw_plot(austin_inset, x = .13,y = .4,width =  .3,height =  .45)



tibble(c(0),
       c(1))


get_single_scam_fit <- function(df, n_samps  = 100){
  scam_fit <- df %>%  
    sample_n(size = n_samps, replace = T) %>%
    filter(immune_mean < 1) %>% 
    scam(immune_mean ~ s(asymp_frac, k = 7, bs = "mpi"), data = .)
  
  mod_preds <- tibble(asymp_frac = seq(.01,1,by = .01))
  mod_preds$predicted <- predict.scam(scam_fit, newdata = mod_preds)
  mod_preds %>% 
    mutate(predicted = ifelse(predicted >1, 1, predicted )) 
}

temp <- rerun(100, get_single_scam_fit(nyc_compare %>% 
                                         filter(city == "NYC", rho == mle_rho))) %>% 
  bind_rows(.id = "id")


# filter(immune_mean<.99) %>% 
scam(immune_mean ~ s(asymp_frac, k = 7, bs = "mpd"), data = .)


mod_preds %>% 
  mutate(predicted = ifelse(predicted >1, 1, predicted )) %>% 
  ggplot(aes(tau, predicted)) + geom_line()
mod$model %>% 
  ggplot(aes(tau,immune_mean)) + geom_line()
ggplot(aes(tau, immune_mean)) + 
  geom_point() +
  geom_errorbar(aes(ymax = immune_hi, ymin = immune_lo)) +
  stat_smooth(method = "scam", formula = y ~ s(x, k = 7, bs = "mpd"), se = TRUE)

library(scam)

fit_results %>% 
  filter(city == "nyc", round(logbeta_stdev, 2) == 0.25 | round(logbeta_stdev, 2) == "2.46") %>% 
  select(logbeta_stdev, full_df, fitted_parms) %>% 
  unnest(full_df) %>% 
  mutate(rnot_mean = map2(exp(logbetat_mean), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_lo = map2(exp(logbetat_lo), fitted_parms, calc_ar_rnot) %>% unlist(),
         rnot_hi = map2(exp(logbetat_hi), fitted_parms, calc_ar_rnot) %>% unlist()) %>% 
  filter(date > "2020-03-03") %>% 
  ggplot(aes(date, rnot_mean, color = as.factor(logbeta_stdev))) + geom_line() +ylim(0,50)


