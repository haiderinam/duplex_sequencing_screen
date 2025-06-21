dr4pl_df <- function(data, n_boot = 100, ci_level = 0.95) {
  ic50data_species_specific <- data

  # Fit model on original data
  fit <- dr4pl(y ~ conc,
               method.init = "logistic",
               data = data,
               init.parm = dr4pl_theta(.99,100,-2,0.01),
               upperl=c(1,Inf,Inf,0.02),
               lowerl=c(0.98,-Inf,-Inf,0))

  rsdls <- residuals(fit)
  theta_hat <- coef(fit)
  IC50.i <- theta_hat[2]
  hill.i <- theta_hat[3]

  ic50data_species_specific$IC50 <- IC50.i
  ic50data_species_specific$Hill.slope <- hill.i
  ic50data_species_specific$convergence <- fit$convergence
  ic50data_species_specific$RSS <- sum(rsdls^2)

  # Concentrations to predict
  conc_grid <- sort(unique(data$conc))

  # Bootstrap loop
  boot_preds <- replicate(n_boot, {
    boot_data <- data[sample(nrow(data), replace = TRUE), ]

    boot_fit <- try(dr4pl(y ~ conc,
                          method.init = "logistic",
                          data = boot_data,
                          init.parm = dr4pl_theta(.99,100,-2,0.01),
                          upperl=c(1,Inf,Inf,0.02),
                          lowerl=c(0.98,-Inf,-Inf,0)), silent = TRUE)

    if (inherits(boot_fit, "try-error")) {
      return(rep(NA, length(conc_grid)))
    } else {
      MeanResponse(coef(boot_fit), x = conc_grid)
    }
  }, simplify = "matrix")

  # CI quantiles
  alpha <- 1 - ci_level
  y_lower <- apply(boot_preds, 1, quantile, probs = alpha / 2, na.rm = TRUE)
  y_upper <- apply(boot_preds, 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE)

  # Final modeled predictions
  y_model <- MeanResponse(theta = c(1, IC50.i, hill.i, 0), x = conc_grid)

  # Merge back to main data
  model_df <- data.frame(conc = conc_grid, y_model, y_model_lower = y_lower, y_model_upper = y_upper)

  ic50data_species_specific <- ic50data_species_specific %>%
    left_join(model_df, by = "conc")

  return(ic50data_species_specific)
}
