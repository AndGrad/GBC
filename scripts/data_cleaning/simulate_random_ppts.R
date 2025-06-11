## Simulate random participants

# Simulate counterfact


source("./B_SimulationCode/load_environments_social_experiment.R") # environments
source("./C_modelfittingCode/learning_models.R") # environments
source("./B_SimulationCode/sim_models.R") # modelcode for simulation

#environment files are in generated files
environments <- load_envs_social(path = "./A_GeneratedFiles/experiment/")
social_sims_plot_d <- list()
social_sims_plot_one <- list()
#filter out participants that could not be fit and wierd trials that have na values in environments
#social_fits_d<-social_fits_d%>%filter(!is.na(fit),!is.na(env_number))
sw_simvec<-seq(0,25,length.out=120)
lrs_simvec<-seq(0,0.7,length.out=20)

sim_dat<-expand.grid(
  lr=lrs_simvec,
  sw=sw_simvec
)

future::plan("multisession", workers = 40)
huge<-furrr::future_map_dfr(1:length(sim_dat$lr),~{
  for (i in 1:100) {
    # make sure you have the parameter lookup dataframe in the futures
    sw_simvec<-seq(0,25,length.out=120)
    lrs_simvec<-seq(0,0.7,length.out=20)
    sim_dat<-expand.grid(
      lr=lrs_simvec,
      sw=sw_simvec
    )
    # make sure you have the environment lookup dataframe in the futures
    Xnew <- as.matrix(expand.grid(0:7, 0:7)) 
    
    #make data (arbitrary participants)
    d1 <- social_data %>% filter(uniqueID == 3) %>%
      group_by(round) %>%
      mutate(
        choices = cells
      ) %>% rowwise() %>%
      mutate(social_info = ifelse(social_info == 64, 1, social_info)) %>%
      ungroup()
    
    #### unpack parameters (learning rate, temperature, social weight)
    estimates <- c(sim_dat$lr[.x],1,sim_dat$sw[.x])
    #print(estimates)
    
    ####
    ####
    #SIMULATE
    ####
    ####
    cv <- exploreEnv1lrsw(
      par = estimates,
      learning_model_fun = RW_Q,
      acquisition_fun = NULL,
      data = d1,
      envs = environments
    )
    
    # Collect
    cv$player = i
    cv$sw=sim_dat$sw[.x]
    cv$lr<-sim_dat$lr[.x]
    social_sims_plot_one[[i]] <- cv
  }
  social_sims_plot_d <- do.call("rbind", social_sims_plot_one)
  return(social_sims_plot_d)
})

saveRDS(huge,file = "A_GeneratedFiles/bootstrapped_rewards.rds")
