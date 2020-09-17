######################################
## post-processing-fxns
######################################


get_state_posteriors <- function(mf2, maxT, N_smooth_draws = 1000, N_particles_per_draw = 2000){
  # construct the raw filtered states for the smoothed distribution
  smooth_dist = foreach(i=1:N_smooth_draws) %do% {
    mf_filter = mf2 %>%
      pfilter(Np = N_particles_per_draw, save.states=TRUE, filter.mean=TRUE, filter.traj=TRUE)
    list(ll = logLik(mf_filter), states=drop(filter.traj(mf_filter)))
  }
  n_states = nrow(smooth_dist[[1]]$states)
  my_dim_names = dimnames(smooth_dist[[1]]$states)
  my_dim_names$sample = 1:length(smooth_dist)
  
  # extract the weights for each filtered trajectory
  smoothing_weights = smooth_dist %>%
    lapply(function(x) x$ll) %>%
    unlist %>%
    (function(x) {exp(x - max(x))})
  
  # bind up the filtered trajectories for each state in an array
  filtered_states = smooth_dist %>%
    lapply(function(x) x$states) %>%
    unlist %>%
    array(dim=c(n_states, maxT,length(smooth_dist)))
  dimnames(filtered_states) = my_dim_names
  
  # resample indices and get smoothed states
  sample_ind = pomp::systematic_resample(smoothing_weights)
  filtered_states[,,sample_ind]
}


get_state_ci <- function(state, statename, smoothed_states, alpha=0.05) {
  states <- smoothed_states[state,,]
  states_mean = apply(states, 1, mean)
  states_sd = apply(states, 1, sd)
  states_lo = apply(states, 1, function(x){quantile(x,alpha/2)})
  states_hi = apply(states, 1, function(x){quantile(x,1-alpha/2)})
  df <- tibble(states_mean, states_sd, states_lo, states_hi)
  names(df) = gsub("states", statename, names(df))
  df
}


get_state_ci_table <- function(fitting_data, smoothed_states, state_names, clean_state_names){
  bind_cols(fitting_data,
            map2(state_names, 
                 clean_state_names, 
                 get_state_ci, 
                 smoothed_states = smoothed_states))
  
}

