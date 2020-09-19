###########################################
## Processing Hospitalization Data
###########################################

get_hospitalization_data <- function(city){
  require(tidyverse)
  require(lubridate)
  
  if(city == "austin"){

    df <- data.frame(
      new_admits = c(1L,0L,1L,0L,0L,1L,1L,1L,
                     2L,3L,1L,3L,4L,3L,5L,12L,8L,4L,9L,9L,13L,9L,
                     10L,8L,10L,7L,7L,5L,14L,11L,4L,4L,14L,10L,
                     10L,7L,7L,11L,8L,5L,13L,9L,6L,10L,9L,3L,8L,7L,
                     9L),
      new_discharges = c(0L,0L,0L,0L,0L,0L,0L,0L,
                         0L,1L,0L,1L,1L,0L,1L,6L,6L,1L,3L,4L,5L,3L,
                         7L,12L,3L,8L,4L,5L,10L,11L,7L,6L,7L,8L,9L,8L,
                         6L,12L,5L,4L,12L,5L,9L,13L,16L,0L,3L,5L,7L),
      hospitalized = c(1L,1L,2L,2L,2L,3L,4L,5L,
                       7L,9L,10L,12L,15L,18L,22L,28L,30L,33L,39L,44L,
                       52L,58L,61L,57L,64L,63L,66L,66L,70L,70L,67L,
                       65L,72L,74L,75L,74L,75L,74L,77L,78L,79L,83L,
                       80L,77L,70L,73L,78L,80L,82L),
      date = c("2020-03-10","2020-03-11",
               "2020-03-12","2020-03-13","2020-03-14","2020-03-15",
               "2020-03-16","2020-03-17","2020-03-18","2020-03-19",
               "2020-03-20","2020-03-21","2020-03-22","2020-03-23",
               "2020-03-24","2020-03-25","2020-03-26","2020-03-27",
               "2020-03-28","2020-03-29","2020-03-30","2020-03-31",
               "2020-04-01","2020-04-02","2020-04-03","2020-04-04",
               "2020-04-05","2020-04-06","2020-04-07","2020-04-08",
               "2020-04-09","2020-04-10","2020-04-11","2020-04-12",
               "2020-04-13","2020-04-14","2020-04-15","2020-04-16","2020-04-17",
               "2020-04-18","2020-04-19","2020-04-20","2020-04-21",
               "2020-04-22","2020-04-23","2020-04-24","2020-04-25",
               "2020-04-26","2020-04-27")
    ) %>% 
      as_tibble() %>% 
      mutate(date = lubridate::ymd(date),
             zt = c(0, diff(hospitalized)))
  } else if(city == "nyc") {
    # df <-  data.frame(
    #   stringsAsFactors = FALSE,
    #   DATE_OF_INTEREST = c("3/3/20","3/4/20","3/5/20","3/6/20","3/7/20",
    #                        "3/8/20","3/9/20","3/10/20","3/11/20",
    #                        "3/12/20","3/13/20","3/14/20","3/15/20",
    #                        "3/16/20","3/17/20","3/18/20","3/19/20",
    #                        "3/20/20","3/21/20","3/22/20","3/23/20",
    #                        "3/24/20","3/25/20","3/26/20","3/27/20",
    #                        "3/28/20","3/29/20","3/30/20","3/31/20",
    #                        "4/1/20","4/2/20","4/3/20","4/4/20",
    #                        "4/5/20","4/6/20","4/7/20","4/8/20","4/9/20",
    #                        "4/10/20","4/11/20","4/12/20","4/13/20",
    #                        "4/14/20","4/15/20","4/16/20","4/17/20",
    #                        "4/18/20","4/19/20","4/20/20","4/21/20",
    #                        "4/22/20","4/23/20","4/24/20","4/25/20",
    #                        "4/26/20","4/27/20"),
    #   HOSPITALIZED_COUNT = c(7L,
    #                          8L,13L,12L,10L,14L,36L,46L,70L,73L,
    #                          135L,152L,187L,299L,339L,439L,533L,
    #                          621L,664L,691L,1023L,1135L,1253L,1387L,
    #                          1343L,1272L,1351L,1636L,1479L,1452L,
    #                          1606L,1591L,1346L,1338L,1683L,1512L,1419L,
    #                          1291L,1217L,993L,927L,1100L,964L,
    #                          853L,688L,721L,519L,475L,534L,523L,434L,
    #                          368L,351L,268L,243L,315L)
    # ) %>% 
    #   mutate(date = lubridate::mdy(DATE_OF_INTEREST),
    #          new_admits = HOSPITALIZED_COUNT) %>% 
    #   select(date, new_admits) 
    df <- data.frame(
              stringsAsFactors = FALSE,
              DATE_OF_INTEREST = c("2/29/20","3/1/20","3/2/20","3/3/20","3/4/20",
                                   "3/5/20","3/6/20","3/7/20","3/8/20",
                                   "3/9/20","3/10/20","3/11/20","3/12/20",
                                   "3/13/20","3/14/20","3/15/20","3/16/20","3/17/20",
                                   "3/18/20","3/19/20","3/20/20","3/21/20",
                                   "3/22/20","3/23/20","3/24/20","3/25/20",
                                   "3/26/20","3/27/20","3/28/20","3/29/20",
                                   "3/30/20","3/31/20","4/1/20","4/2/20",
                                   "4/3/20","4/4/20","4/5/20","4/6/20","4/7/20",
                                   "4/8/20","4/9/20","4/10/20","4/11/20",
                                   "4/12/20","4/13/20","4/14/20","4/15/20",
                                   "4/16/20","4/17/20","4/18/20","4/19/20",
                                   "4/20/20","4/21/20","4/22/20","4/23/20",
                                   "4/24/20","4/25/20","4/26/20","4/27/20",
                                   "4/28/20","4/29/20","4/30/20","5/1/20","5/2/20",
                                   "5/3/20","5/4/20","5/5/20","5/6/20",
                                   "5/7/20","5/8/20","5/9/20","5/10/20",
                                   "5/11/20","5/12/20","5/13/20","5/14/20",
                                   "5/15/20","5/16/20","5/17/20","5/18/20","5/19/20",
                                   "5/20/20","5/21/20","5/22/20","5/23/20",
                                   "5/24/20","5/25/20","5/26/20","5/27/20",
                                   "5/28/20","5/29/20","5/30/20","5/31/20",
                                   "6/1/20","6/2/20","6/3/20","6/4/20",
                                   "6/5/20","6/6/20","6/7/20","6/8/20","6/9/20",
                                   "6/10/20","6/11/20","6/12/20","6/13/20",
                                   "6/14/20","6/15/20","6/16/20","6/17/20",
                                   "6/18/20","6/19/20","6/20/20","6/21/20",
                                   "6/22/20","6/23/20","6/24/20","6/25/20",
                                   "6/26/20","6/27/20","6/28/20","6/29/20",
                                   "6/30/20","7/1/20","7/2/20","7/3/20","7/4/20",
                                   "7/5/20","7/6/20","7/7/20","7/8/20",
                                   "7/9/20","7/10/20","7/11/20","7/12/20",
                                   "7/13/20","7/14/20","7/15/20","7/16/20",
                                   "7/17/20","7/18/20","7/19/20","7/20/20","7/21/20",
                                   "7/22/20","7/23/20","7/24/20","7/25/20",
                                   "7/26/20","7/27/20","7/28/20","7/29/20",
                                   "7/30/20","7/31/20","8/1/20","8/2/20",
                                   "8/3/20","8/4/20","8/5/20","8/6/20","8/7/20",
                                   "8/8/20","8/9/20","8/10/20","8/11/20",
                                   "8/12/20","8/13/20","8/14/20","8/15/20",
                                   "8/16/20","8/17/20","8/18/20","8/19/20",
                                   "8/20/20","8/21/20","8/22/20","8/23/20",
                                   "8/24/20","8/25/20","8/26/20","8/27/20",
                                   "8/28/20","8/29/20","8/30/20","8/31/20",
                                   "9/1/20","9/2/20","9/3/20","9/4/20","9/5/20",
                                   "9/6/20","9/7/20","9/8/20","9/9/20",
                                   "9/10/20","9/11/20","9/12/20","9/13/20",
                                   "9/14/20","9/15/20","9/16/20","9/17/20"),
            HOSPITALIZED_COUNT = c(11L,4L,
                                   21L,20L,22L,20L,18L,15L,17L,49L,63L,
                                   90L,92L,156L,159L,189L,317L,357L,
                                   470L,559L,662L,701L,727L,1036L,1159L,
                                   1304L,1453L,1394L,1364L,1407L,1719L,1694L,
                                   1595L,1618L,1602L,1345L,1328L,1721L,
                                   1604L,1540L,1426L,1337L,1099L,995L,1213L,
                                   1088L,981L,881L,901L,665L,592L,725L,
                                   660L,598L,532L,550L,403L,374L,444L,445L,
                                   407L,345L,352L,261L,256L,311L,283L,
                                   262L,259L,257L,185L,157L,246L,211L,230L,
                                   220L,234L,164L,125L,178L,205L,169L,
                                   174L,197L,134L,119L,140L,151L,161L,139L,
                                   170L,98L,91L,124L,123L,115L,90L,65L,
                                   46L,47L,62L,51L,45L,41L,55L,28L,44L,
                                   49L,50L,41L,39L,39L,26L,34L,44L,42L,
                                   37L,37L,34L,37L,32L,39L,37L,29L,30L,
                                   21L,22L,24L,43L,41L,32L,42L,35L,30L,
                                   18L,46L,33L,27L,30L,47L,29L,33L,32L,
                                   22L,31L,34L,34L,30L,18L,43L,35L,22L,
                                   31L,27L,22L,28L,29L,31L,27L,31L,18L,
                                   17L,25L,27L,18L,27L,33L,30L,21L,19L,
                                   26L,35L,27L,29L,28L,20L,18L,26L,36L,
                                   22L,28L,19L,22L,20L,30L,21L,21L,19L,
                                   25L,16L,20L,20L,23L,12L,10L,11L,2L,
                                   6L,7L,4L,1L,0L)) %>% 
                mutate(date = lubridate::mdy(DATE_OF_INTEREST),
                       new_admits = HOSPITALIZED_COUNT) %>%
                select(date, new_admits) %>% 
      filter(date <= '2020-07-01')
          
  } else if(city == "utah"){
    df <- tibble(new_admits = c(1, 0, 0, 0, 0, 3, 1, 1, 2, 0, 3, 2, 0, 10, 3, 3, 8, 3, 10, 13, 5, 5, 11, 13, 8, 9,
                          4, 5, 4, 9, 10, 6, 11, 6, 8, 10, 11, 10, 7 ,11 ,4, 13, 5, 7, 8, 15, 12, 18, 15, 5,
                          14, 10, 8, 6, 17, 13, 11, 11, 9, 7, 10, 10, 11, 16, 9, 10, 13, 11, 9, 11, 12, 8,
                          22, 18, 12, 14, 15, 13, 8, 6, 22, 21, 7, 16, 13, 14, 21, 16, 12, 18, 19, 19, 15,
                          15, 15, 27, 13, 18, 14, 16, 27, 24, 22, 27, 27, 13, 19, 25, 26, 32, 34, 30, 25,
                          34, 26, 25, 30)) %>% 
      mutate(date = ymd("2020-03-06") + days(seq_along(new_admits)))
  }
  return(df)
}



get_data_and_parms <- function(days_to_fill_in, fitting_data, parms){

    data_to_fit <- tibble(date = rev(min(fitting_data$date) - days(1:days_to_fill_in)),
           hospitalized = 0, 
           new_admits = 0,
           new_discharges = 0,
           zt = 0) %>% bind_rows(fitting_data) %>% 
      mutate(day = seq_along(date)-1)
    
    return(list(data = data_to_fit,
                parms = parms))
}

replace_parms <- function(new_vals, parms){
  val_names <- names(new_vals)
  for(i in 1:length(new_vals)){
    parms[val_names[i]] <- new_vals[i]  
  }
  return(parms)
}

# get_init_parms <- function(nyc = FALSE){
#   ## Age distributions: https://censusreporter.org/
#   c(beta0 = 0.42, ## Initial transmission rate
#     beta1 = .001, ## Final transmission rate
#     tau1 = 13, ## timing of intervention in data -- relative to data start date
#     k = 0.1, ## Exponential decay in beta0 due to intervention
#     omega = 1, ## 0-1 percent of beta that asymptomatic individuals transmit
#     sigma = 1 / 2.9, ## Incubation period (no transmission)
#     tau = 0.82, ## 0-1 Symptomatic proportion
#     gamma = 1/7, ## Infectious period
#     # hospitalization_rate = .0453, ## Hospitalization rate for all infections (https://docs.google.com/spreadsheets/d/1g6ciTRpaiC642b3NJ6htO92wQ46qqvdxvTsyo3G36fY/edit?usp=sharing)
#     # rho = .0453/0.82, ## Hospitalization rate for symptomatic infections (based on hosp_rate and symp proportion)
#     rho = .0453, ## Hospitalization rate for symptomatic infections (based on hosp_rate and symp proportion)
#     nu = 1/7, ## Time until hospitalization (https://jamanetwork.com/journals/jama/fullarticle/2761044)
#     alpha = 0.154, ## Proportion of hospitalizations that die
#     mu = 1/11.3, ## Time in hospital before death/recovery
#     S_0 = 2168316, 
#     E_0=0, 
#     IA_0 = 0, 
#     IY_0 = 1, 
#     H_0 =0, 
#     R_0 = 0, 
#     D_0 = 0, 
#     NI_0 = 1, 
#     NH_0 = 0,
#     LH_0 = 0,
#     NIY_0 = 1
#   )
# }



# get_hospitalization_data <- function(nyc = FALSE){
#   require(googlesheets4)
#   require(tidyverse)
#   require(lubridate)
#   # browser()
#   if(!nyc){
#     df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/16QcLWs5kytxSjiZr8Ze6RtILvReLRh1FGysSXAJRncc/edit?usp=sharing",
#                                     sheet=1) %>% 
#       mutate(zt = c(0, diff(hospitalized))) %>% 
#       select(date, hospitalized, zt, admit, discharge)
#     df <- df %>% 
#       filter(ifelse(seq_along(hospitalized) %in% 1:(rle(df$hospitalized)$lengths[1]-1),
#                     FALSE, TRUE))  
#   } else {
#     df <- read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/case-hosp-death.csv") %>%
#       mutate(date = lubridate::mdy(DATE_OF_INTEREST),
#              hospitalized = HOSPITALIZED_CASE_COUNT) %>% 
#       select(date, hospitalized, admit, discharge) 
#     df <- tibble(date = min(df$date)-lubridate::days(1),
#            hospitalized = 0) %>% 
#       bind_rows(df) %>% 
#       mutate(zt = c(0, diff(hospitalized)))
#   }
#   return(df)
# }
