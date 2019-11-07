sim_with_estimated_pars <- function(est_params_table,
                                    time,
                                    n_mainland_species
                                    ) { 
  n_replicates <- nrow(est_params_table)
  pars_table <- est_params_table[1:2,1:5]
  sim = list()
  for(i in 1:2) {
    pars = c(pars_table[i,1],pars_table[i,2],pars_table[i,3],pars_table[i,4],pars_table[i,5])
    sim[[i]] = DAISIE_sim(time = island_age,
                          M = n_mainland_species, 
                          pars = pars,
                          replicates = n_replicates,
                          Tpars = NULL,    ##  DAISIE only considering one trait states, so Tpars = NULL
                          plot_sims = FALSE,
                          verbose = FALSE,
                          Apars = NULL,
                          divdepmodel = "CS")
  }
  return(sim)
}