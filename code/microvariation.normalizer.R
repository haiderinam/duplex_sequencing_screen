microvariation.normalizer=function(netgr_raw,
                                   net_gr_wodrug,
                                   hill_standard, #Hill coefficient for the standard.
                                   ic50_standard, #IC50 for the standard
                                   dose_model, #Basically the dose of the model experiment (M3) for now. "Apparent dose" could be the mean of the apparrent doses form all experiments too
                                   time,#hours, probably the amount of time for ic50 rather than duration of experiment
                                   standard_name,
                                   mcmc=FALSE,#Set to true if you want to use the mcmc-derived apparent doses
                                   dose_app_mcmc=0
                                   ){
  ####### Hardcoded Inputs########
  # mcmc=FALSE
  # netgr_raw=twinstrand_simple_melt_merge%>%
  #   filter(experiment=="M4",duration=="d3d6")%>%
  #   dplyr::select(experiment,mutant,netgr_obs)
  # net_gr_wodrug=0.057
  # time=72
  # hill_standard=2.83 #E255K Hill
  # ic50_standard=1.205 #E255K IC50
  # standard_name="E255K"
  # dose_model=1.9147 #calculated using the net growth rate of the standard for the model experiment
  ####### Hardcoded Inputs for MCMC Apparent doses########
  # netgr_raw=twinstrand_simple_melt_merge%>%
  #   filter(experiment=="M3",duration=="d3d6")%>%
  #   dplyr::select(experiment,mutant,netgr_obs)
  # twinstrand_simple_melt_merge=twinstrand_simple_melt_merge%>%filter(!experiment%in%c("Enu_3","Enu_4"))
  # net_gr_wodrug=0.055
  # time=72
  # hill_standard=2.83 #E255K Hill
  # ic50_standard=1.205 #E255K IC50
  # standard_name="E255K"
  # dose_model=1.56 #M3 dose
  # mcmc=TRUE
  # dose_app_mcmc=as.numeric(mcmc_apparent_doses%>%filter(experiment%in%"M3")%>%dplyr::select(dose_app))
  ########Corrections code########
  #First filter: if netgr_obs for a mutant is greater than netgr wo drug, change it to netgr wo drug and include a warning.
  #Essentially, if something is growing faster with drug than without, change its growth rate to the without drug growth rate.
  netgr_raw=netgr_raw%>%mutate(netgr_obs=case_when(netgr_obs>net_gr_wodrug~net_gr_wodrug-.0001,
                                                   netgr_obs<=net_gr_wodrug~netgr_obs))
  #Calculate alpha t (drug effect*time) for standard
  netgr_standard=as.numeric(netgr_raw%>%filter(mutant%in%standard_name)%>%dplyr::select(netgr_obs))
  alpha_t_standard=(net_gr_wodrug-netgr_standard)*time

  #Calculate apparrent dose based on the e255k growth rate
  #if MCMC flag is true, apparent dose is dose_apparent_mcmc
  if(mcmc==TRUE){
    dose_app=dose_app_mcmc
  } else{
    dose_app=((1-exp(alpha_t_standard))*-(ic50_standard^hill_standard))^(1/hill_standard)
  }


  #Calculate IC50apparrent for all mutants using their net growth rates and the same hill coefficient as what was observed for E255K
  netgr_new=netgr_raw%>%mutate(ic50_apparent=dose_app*exp(-(net_gr_wodrug-netgr_obs)*time)^(1/hill_standard)/(1-exp(-(net_gr_wodrug-netgr_obs)*time))^(1/hill_standard))

  #Calculate correction factors
  netgr_new=netgr_new%>%mutate(correction_factor=log(1/(1+(dose_app/ic50_apparent)^hill_standard))/log(1/(1+(dose_model/ic50_apparent)^hill_standard)))

  #Normalize the net growth rate by the correction factor
  netgr_new=netgr_new%>%mutate(netgr_obs_corrected=net_gr_wodrug-(net_gr_wodrug-netgr_obs)/correction_factor)

  #Take out the ic50_apparent and correction factor columns
  netgr_new=netgr_new%>%dplyr::select(-ic50_apparent,-correction_factor)

  ######Outputs
  #Dataframe with corrected growth rates
  # netgr_new
  #Dose_apparent
  # dose_app
  return(list(netgr_new,dose_app))
}
