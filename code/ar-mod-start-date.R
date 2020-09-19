require(pomp)

Csnippet('
        if(t >= day_to_start && started == 0){
          S = S;
          E = E;
          IA = IA;
          IY_tm1 = IY; 
          IY = 1;
          H_tm1 = H;  // must track yesterdays hospitalizations
          H = H;
          R = R;
          D = D;
          NI = 1;
          NH = 0;
          LH = 0;
          NIY = 1;
          started = 1;
        } else if(t < day_to_start) {
          S = S;
          E = E;
          IA = IA;
          IY_tm1 = IY; 
          IY = IY;
          H_tm1 = H;  // must track yesterdays hospitalizations
          H = H;
          R = R;
          D = D;
          NI = 1;
          NH = 0;
          LH = 0;
          NIY = 0;
          logbeta_t = logbeta_t;
        }else {
          double beta, N, pi_var, delta_logbeta;
          N = S+E+IY+IA+H+R+D;
          pi_var = gamma * rho / (nu + gamma*rho - nu*rho);
          
          // Gaussian shocks
          delta_logbeta = rnorm(0,logbeta_stdev*sqrt(dt));
          logbeta_t = logbeta_t + delta_logbeta;
          
          // Student-t shocks with df=3
          // delta_logbeta = logbeta_stdev*rt(3);
          // logbeta_t = logbeta_t + delta_logbeta;
          
          beta = exp(logbeta_t);
          
          // Calculate newly exposed individuals
          double new_E = rbinom(S, 1 - exp(-(beta/N*IY + beta*omega/N*IA)*dt));
          double leaving_E = rbinom(E, 1 - exp(-sigma*dt));
          
          // Allocate newly infectious people into symptomatic and asymptomatic
          double new_IA = rbinom(leaving_E, (1 - tau));
          double new_IY = leaving_E - new_IA;
          
          // Recovering asymptomatics
          double recovering_IA = rbinom(IA, 1 - exp(-gamma*dt));
          
          // Recovering and hospitalizing symptomatics
          double leaving_IY = rbinom(IY, 1 - exp(-((1-pi_var)*gamma + pi_var*nu)*dt));
          double new_H = rbinom(leaving_IY, (pi_var*nu / ((1-pi_var)*gamma + pi_var*nu)));
          double recovering_IY = leaving_IY - new_H;
          
          // Moving hospitalized individuals
          double leaving_H = rbinom(H, 1 - exp(-mu*dt));
          double recovering_H = rbinom(leaving_H, (1-alpha));
          double dying_H = leaving_H - recovering_H;
          
          S = S - new_E;
          E = E + new_E - leaving_E;
          IA = IA + new_IA - recovering_IA;
          IY_tm1 = IY; // must track yesterdays IY for likelihood
          IY = IY + new_IY - leaving_IY;
          H_tm1 = H;  // must track yesterdays hospitalizations
          H = H + new_H - leaving_H;
          R = R + recovering_IA + recovering_IY + recovering_H;
          D = D + dying_H;
          NI = leaving_E;
          NH = new_H;
          LH = leaving_H;
          NIY = new_IY;
        }
        ') -> covid_rprocess
Csnippet("
  S = S_0;
  E = E_0;
  IA = IA_0;
  IY_tm1 = 0;
  IY = IY_0;
  H_tm1 = 0;
  H = H_0;
  R = R_0;
  D = D_0;
  NI = NI_0;
  NH = NH_0;
  LH = LH_0;
  NIY = NIY_0;
  logbeta_t = logbeta_0;
  started = started_0;
  ") -> rinit
c("omega", 
  "sigma", 
  "tau", 
  "gamma", 
  "rho", 
  "nu", 
  "alpha", 
  "mu",
  "day_to_start",
  "S_0", 
  "E_0", 
  "IA_0",
  "IY_0", 
  "H_0", 
  "R_0", 
  "D_0", 
  "NI_0", 
  "NH_0",
  "LH_0",
  "NIY_0",
  "logbeta_0",
  "logbeta_stdev",
  "r",
  "ndata",
  "started_0") -> covid_paramnames
c("S", 
  "E", 
  "IA",
  "IY", 
  "IY_tm1",
  "H",  
  "H_tm1",
  "R", 
  "D", 
  "NI", 
  "NH",
  "LH",
  "NIY",
  "logbeta_t",
  "started"
  ) -> covid_statenames

Csnippet('
          new_admits = rpois(NH);
          new_discharges = rpois(LH);
          ') -> covid_rmeas

Csnippet('double f;
          f = dpois(nearbyint(hospitalized), H, 1);
          lik = (give_log) ? f : exp(f);
          ') -> covid_dmeas

Csnippet('double f;
          int l, l_min, l_max;
          l_min = fmax(0, zt); // min number who could have arrived today
          l_max = hospitalized; // maximum number of people who could have arrived in hosp today
          double likelihood, piece1, piece2;
          likelihood=0.0;
          for(l=l_min; l<=l_max; l++) {
            piece1 = dpois(l, new_H, 0);  // arrivals to hospitals = l
            // piece2 = dbinom(l - zt, H_prev, 1 - exp(-mu), 0);  // departures from hospitals = l-zt
            piece2 = dpois(l-zt, leaving_H, 0);
            likelihood += piece1*piece2;
          }
          lik = (give_log) ? log(likelihood) : likelihood;
          ') -> covid_dmeas_binom


Csnippet('double loglike;
          loglike = dpois(new_admits, NH, 1) + dpois(new_discharges, LH, 1) - 0.1*logbeta_t*logbeta_t;  // arrivals to hospitals
          lik = (give_log) ? loglike : exp(loglike);
          ') -> covid_dmeas_admitdischarge_betapenalty

Csnippet('double loglike;
          ///Rprintf(\"%lg %lg %lg\\n\",new_admits,NH,lik);
          if (ISNAN(NH)) {
            lik = (give_log) ? -10000 : 0;
          } else {
            loglike = dpois(new_admits, NH, 1);  // arrivals to hospitals
            lik = (give_log) ? loglike : exp(loglike);
          }
          
          ///Rprintf(\"%lg %lg %lg\\n\",new_admits,NH,lik);
          ') -> covid_dmeas_admitonly


Csnippet('double loglike;
          ///Rprintf(\"%lg %lg %lg\\n\",new_admits,NH,lik);
          if (ISNAN(NH)) {
            lik = (give_log) ? -10000 : 0;
          } else {
            loglike = dnbinom_mu(new_admits, r, NH, 1);  // arrivals to hospitals
            loglike += dexp(logbeta_stdev, 1.0, 1) / ndata;
            lik = (give_log) ? loglike : exp(loglike);
          }
          
          ///Rprintf(\"%lg %lg %lg\\n\",new_admits,NH,lik);
          ') -> covid_dmeas_nyc_admit

Csnippet('double loglike;
          if (ISNAN(NH) || ISNAN(LH)) {
            lik = (give_log) ? -10000 : 0;
          } else {
            loglike = dpois(new_admits, NH, 1) + dpois(new_discharges, LH, 1);  // arrivals to hospitals
            loglike += dexp(logbeta_stdev, 1.0, 1) / ndata;
            lik = (give_log) ? loglike : exp(loglike);
          }
          ') -> covid_dmeas_admitdischarge

Csnippet('double zt_mu, zt_var, loglik;
          zt_mu = new_H - leaving_H;
          zt_var = new_H + leaving_H;
          if(zt_var==0) {
            loglik = (zt==0) ? 0 : -1e99;
          } else {
            loglik = dnorm(zt, zt_mu, sqrt(zt_var), 1);
          }
          lik = (give_log) ? loglik : exp(loglik);
          ') -> covid_dmeas_skellam

Csnippet('double loglike;
          // calculate expected number of hospital admits at next time step, 
          // given parameters and number infected symptomatic at last time step.
          // this is just iterated expectation over two binomials:
          // E(leaving_IY today | IY yesterday) and and E(new_H today| leaving_IY today)
          double pi_var = gamma * rho / (nu + gamma*rho - nu*rho);
          double leaving_IY_mean = IY_tm1*(1 - exp(-((1-pi_var)*gamma + pi_var*nu)));
          double new_H_mean = leaving_IY_mean*(pi_var*nu / ((1-pi_var)*gamma + pi_var*nu));
          
          // now calculate expected number of discharges, given yesterdays H
          double leaving_H_mean = H_tm1*(1 - exp(-mu));
          
          // likelihood is the product of two Poissons with these means
          loglike = dpois(new_admits, new_H_mean, 1); // admits
          loglike += dpois(new_discharges, leaving_H_mean, 1);  // discharges
          lik = (give_log) ? loglike : exp(loglike);
          ') -> covid_dmeas_admitdischarge_marginalized


get_init_parms_ar_start <- function(city){
  ## Age distributions: https://censusreporter.org/
  if(city == "nyc"){
    c(omega = 0.67, ## 0-1 percent of beta that asymptomatic individuals transmit
      sigma = 1 / 2.9, ## Incubation period (no transmission)
      tau = 0.57, ## 0-1 Symptomatic proportion
      gamma = 1/7, ## Infectious period
      rho = .0513, ## Hospitalization rate for symptomatic infections (based on hosp_rate and symp proportion)
      nu = 1/7, ## Time until hospitalization (https://jamanetwork.com/journals/jama/fullarticle/2761044)
      alpha = 0.21, ## Proportion of hospitalizations that die - https://jamanetwork.com/journals/jama/fullarticle/2761044
      mu = 1/4.1, ##  Time in hospital https://jamanetwork.com/journals/jama/fullarticle/2761044
      day_to_start = 0,
      started = FALSE,
      S_0 = 8398747, 
      E_0=0, 
      IA_0 = 0, 
      IY_0 = 0, 
      H_0 =0, 
      R_0 = 0, 
      D_0 = 0, 
      NI_0 = 0, 
      NH_0 = 0,
      LH_0 = 0,
      NIY_0 = 0,
      logbeta_0 = -0.405, ## R0 = 4
      logbeta_stdev = 0.21,
      r = 5,
      started_0 = 0,
      # ndata = 56
      ndata = 124
    )
  } else if(city == "austin") {
    c(omega = 0.67, ## 0-1 percent of beta that asymptomatic individuals transmit
      sigma = 1 / 2.9, ## Incubation period (no transmission)
      tau = 0.57, ## 0-1 Symptomatic proportion
      gamma = 1/7, ## Infectious period
      rho = .0443, ## Hospitalization rate based on age demographics of location
      nu = 1/7, ## Time until hospitalization (https://jamanetwork.com/journals/jama/fullarticle/2761044)
      alpha = 0.154, ## Proportion of hospitalizations that die
      mu = 1/10.5, ## Time in hospital before death/recovery -- APH personal communication
      day_to_start = 0,
      started = FALSE,
      S_0 = 2168316, 
      E_0=0, 
      IA_0 = 0, 
      IY_0 = 0, 
      H_0 =0, 
      R_0 = 0, 
      D_0 = 0, 
      NI_0 = 0, 
      NH_0 = 0,
      LH_0 = 0,
      NIY_0 = 0,
      logbeta_0 = -0.405, ## R0 = 4
      logbeta_stdev = 0.21,
      started_0 = 0,
      r = 5,
      ndata = 49
    )
  } else if (city == "utah"){
    c(omega = 0.67, ## 0-1 percent of beta that asymptomatic individuals transmit
      sigma = 1 / 2.9, ## Incubation period (no transmission)
      tau = 0.57, ## 0-1 Symptomatic proportion
      gamma = 1/7, ## Infectious period
      rho = .0412, ## Hospitalization rate based on age demographics of location
      nu = 1/7, ## Time until hospitalization (https://jamanetwork.com/journals/jama/fullarticle/2761044)
      alpha = 0.154, ## Proportion of hospitalizations that die
      mu = 1/10.5, ## Time in hospital before death/recovery -- APH personal communication
      day_to_start = 0,
      started = FALSE,
      S_0 = 3205957,  ##Location population size minus 1
      E_0=0, 
      IA_0 = 0, 
      IY_0 = 0, 
      H_0 =0, 
      R_0 = 0, 
      D_0 = 0, 
      NI_0 = 0, 
      NH_0 = 0,
      LH_0 = 0,
      NIY_0 = 0,
      logbeta_0 = -0.405, ## R0 = 4
      logbeta_stdev = 0.21,
      started_0 = 0,
      r = 5,
      ndata = 117
    )
  }
}

calc_ar_rnot <- function(beta, parms){
  with(as.list(parms), {
    pi_var = gamma * rho / (nu + gamma*rho - nu*rho)
    beta*(tau)/(gamma*(1-pi_var) + pi_var*nu) + beta*omega*(1 - tau)/(gamma)
  })
}

