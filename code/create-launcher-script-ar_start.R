base_url <- "asymptomatic"
if(grepl('spencerfox', Sys.info()['user'])) setwd(file.path("~", "projects", base_url))
if(grepl('vagrant', Sys.info()['user'])) setwd( file.path("/vagrant", base_url) )
if(grepl('stampede', Sys.info()['nodename'])) setwd(file.path('/home1/02958/sjf826', base_url))
if(grepl('wrangler', Sys.info()['nodename'])) setwd(file.path('/home/02958/sjf826', base_url))
if(grepl('frontera', Sys.info()['nodename']))  setwd(file.path('/home1/02958/sjf826', base_url))

start_days = 0
taus =  as.numeric(sprintf("%.2f",seq(0.01, 1, by=.01)))
omegas = c(0.29, 0.67, 1.42)
cities = c("austin", "nyc", "utah")
nyc_hosp_rates <- c(0.0513, 0.0305, 0.105)
austin_hosp_rates <- c(0.0443, 0.0264, 0.0905)
utah_hosp_rates <- c(0.0412, 0.0245, 0.0841)
hour_of_run <- format(Sys.time(),'%Y-%m-%d-%H')


sink('launcher/start-day-ar-parm-sweep.txt')
for(city in cities){
  for(tau in taus){
    for(omega in omegas){
      for(start_day in start_days){
        for(hosp_rate_index in 1:length(nyc_hosp_rates)){
          startCmd <- "R CMD BATCH --no-restore --no-save '--args"
          
          rho <- case_when(city == "austin"~ austin_hosp_rates[hosp_rate_index],
                           city == "nyc" ~ nyc_hosp_rates[hosp_rate_index],
                           city == "utah" ~ utah_hosp_rates[hosp_rate_index])
          
          fileCmd <- paste0(' city="', city, 
                            '" start_day="', start_day, 
                            '" omega="', omega,
                            '" tau="', tau,
                            '" rho="', rho,
                            '" hour_of_run="', hour_of_run, '"')
          endCmd <- "' ../code/fit-ar-pomp.R"
          full_cmd <- paste0(startCmd, fileCmd, endCmd)
          # print(full_cmd)
          cat(full_cmd)               # add command
          cat('\n')              # add new line  
        }
      }
    }
  }  
}
sink()
