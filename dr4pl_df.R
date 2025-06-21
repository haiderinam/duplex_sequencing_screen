dr4pl_df <- function(data, n_boot = 10, ci_level = 0.95, rss_threshold = 0.5) {
  ic50data_species_specific <- data

  # Initial constrained fit
  fit <- try(dr4pl(y ~ conc,
                   method.init = "logistic",
                   data = data,
                   init.parm = dr4pl_theta(.99,100,-2,0.01),
                   upperl=c(1,Inf,Inf,0.02),
                   lowerl=c(0.98,-Inf,-Inf,0)), silent = TRUE)

  if (inherits(fit, "try-error")) {
    warning("Initial fit failed.")
    return(NULL)
  }

  rsdls <- residuals(fit)
  rss <- sum(rsdls^2)

  # If RSS is high (flatline), try an unconstrained fit
  if (rss > rss_threshold) {
    message("High RSS detected (", round(rss, 3), "). Trying fallback (unconstrained) fit.")
    fit <- try(dr4pl(y ~ conc,
                     method.init = "logistic",
                     data = data), silent = TRUE)

    if (inherits(fit, "try-error")) {
      warning("Fallback fit also failed.")
      return(NULL)
    }

    rsdls <- residuals(fit)
    rss <- sum(rsdls^2)
  }

  theta_hat <- coef(fit)
  IC50.i <- theta_hat[2]
  hill.i <- theta_hat[3]

  ic50data_species_specific$IC50 <- IC50.i
  ic50data_species_specific$Hill.slope <- hill.i
  ic50data_species_specific$convergence <- fit$convergence
  ic50data_species_specific$RSS <- rss

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

  alpha <- 1 - ci_level
  y_lower <- apply(boot_preds, 1, quantile, probs = alpha / 2, na.rm = TRUE)
  y_upper <- apply(boot_preds, 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
  y_model <- MeanResponse(theta_hat, x = conc_grid)

  model_df <- data.frame(conc = conc_grid, y_model, y_model_lower = y_lower, y_model_upper = y_upper)

  ic50data_species_specific <- ic50data_species_specific %>%
    left_join(model_df, by = "conc")

  return(ic50data_species_specific)
}
