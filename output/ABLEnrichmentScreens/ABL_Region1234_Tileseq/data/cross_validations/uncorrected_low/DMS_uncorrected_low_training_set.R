### Find drug concentration that best identifies clinical resistance mutations from pooled mutational scanning screens ###

## Set up
rm(list=ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dr4pl)
library(dplyr)
library(pROC)
library(ggplot2)

## Import and parse data
ngr.df = read.csv("merged_table_imatinib_full_kinase_ngr_uncorr_low.csv",
                  header=T,stringsAsFactors=F)

# Known non-resistant and resistant mutations
non_resistant_muts = c('A337V', 'P465S', 'V468F', 'I502L', 'V299L')

resistant_muts <- c("E255V", "Y253H", "T315I", "H396P", "F486S", "G250E", "E255K", "Y253F", 
                    "Q252H", "L248V", "F359C", "F359I", "M351T", "F317L", "D276G", "M244V", "E459K")


ngr.df <- ngr.df %>%
  filter(species %in% c(non_resistant_muts, resistant_muts))

screen.df = ngr.df

# Add resistance ground truth as a logical vector, explicitly handling non-resistant cases
screen.df$resmuts = screen.df$species %in% resistant_muts & !(screen.df$species %in% non_resistant_muts)


## For each variant, fit dose response curve

# Two replicates at three doses
doses = c(rep(screen.df$dose.low[1],2),
          rep(screen.df$dose.medium[1],2),
          rep(screen.df$dose.high[1],2))

# Initiate dr4pl fit columns
screen.df$IC50 = NA
screen.df$Hill.slope = NA
screen.df$convergence = NA
screen.df$RSS = NA

for (i in 1:nrow(screen.df)) {
  
  species.i = screen.df$species[i]
  
  ## Convert net growth rates into expected relative viability estimates for a 3 day IC50 assay
  mean.netgr.nodrug = mean(screen.df$netgr_obs_rep1.il3[i],
                           screen.df$netgr_obs_rep2.il3[i])
  
  
  netgrs = c(screen.df$netgr_obs_rep1.low[i],
             screen.df$netgr_obs_rep2.low[i],
             screen.df$netgr_corr_rep1.medium[i],
             screen.df$netgr_corr_rep2.medium[i],
             screen.df$netgr_corr_rep1.high[i],
             screen.df$netgr_corr_rep2.high[i])
  
  
  
  t.assay = 72 # in hours
  
  
  #rel.viabs = exp(netgrs*t.assay)/exp(mean.netgr.nodrug*t.assay)
  rel.viabs = exp(netgrs*t.assay)/exp(0.055*t.assay)
  #rel.viabs = exp(netgrs*t.assay)/exp(0.045*t.assay)
  ## Fit dose response
  
  # Force max and min values to be (near) 1 and 0, respectively
  fit = dr4pl(rel.viabs~doses,
              method.init="logistic",
              init.parm = dr4pl_theta(.99,100,-2,0.01),
              upperl=c(1,Inf,Inf,0.02),lowerl=c(0.98,-Inf,-Inf,0))
  
  rsdls = residuals(fit)
  
  plot(fit)
  
  ## Record parameters
  screen.df$IC50[i] = coef(fit)[2]
  screen.df$Hill.slope[i] = coef(fit)[3]
  screen.df$convergence[i] = fit$convergence
  screen.df$RSS[i] = sum(rsdls^2)
  
}




## Refine data

# Filter for any instances of failed convergence
screen.df = screen.df %>%
  filter(convergence==T)


## Go through range of drug doses spanning low to high dose


dose.rnge <- sort(unique(c(seq(10, 10000, by = 10), 444, 760, 916)))

#Calculate rel.viab and alpha for Each Variant and Dose
for (dose in dose.rnge) {
  for (i in 1:nrow(screen.df)) {
    IC50.i <- screen.df$IC50[i]
    hill.i <- screen.df$Hill.slope[i]
    
    pred.resp <- MeanResponse(theta = c(1, IC50.i, hill.i, 0), x = dose)
    screen.df[i, paste0(dose, '.rel.viab')] <- pred.resp
    screen.df[i, paste0(dose, '.alpha')] <- -log(pred.resp) / 72  # t.assay
  }
}
