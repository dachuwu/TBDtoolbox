#' @md
#' @name HIfit
#' @title Hierarchical Imputation Model
#' @description Fit a Bayesian Hierarchical model to make 'shrinkage' estimation and impute missing values in aggregated data.
#'
#' @param X_fix (numeric matrix) a model matrix (or design matrix) for fixed, non-hierarchical effects. We recommend using [stats::model.matrix] to generate this matrix.
#' @param X_hier (integer/character/factor matrix) a indicator matrix of hierarchical effects.
#' @param Y (numeric vector) an outcome vector. For `model = "binomial"`, this will be a vector of observed success probability (e.g. prevalence, incidence).
#' For `model = "normal"`, this will be a vector of observed sample means. **Note that `NA`'s have to be replaced by any numeric values**.
#' @param Ysd (numeric vector) standard deviation for each observation. (required for `model = "normal"`)
#' @param Nsam (integer vector) population, or sample size for each observation.
#' @param isDat (integer/logical vector) indicators which rows are observed (`1`, or `TRUE`) and which are missing values (`0`, or `FALSE`).
#' @param model (character) the model type, one of "binomial" or "normal".
#' @param ... parameters for the sampling algorithm, such as
#'  * `chains`: number of the Markov chains (defaults to 4).
#'  * `cores`: number of cores to use when executing the chains in parallel (defaults to 1).
#'  * `iter`: number of iterations for each chain including warmup (defaults to 2000).
#'  * `warmup`: number of  warmup (aka burnin) iterations for each chain (defaults to `iter/2`).
#'  * `control `: a named list of parameters to control the sampler's behavior, such as
#'    * `adapt_delta`: between 0 and 1, defaults to 0.8.
#'    * `max_treedepth`: positive integer, defaults to 10.
#'    * ...: for other parameters, see the details in the documentation for the control argument in [rstan:stan()].
#'
#' @return A `HIfit` object containing the following components.
#' * `fit`: an object of S4 class [rstan::stanfit].
#' * `dat`: a list of data passed [rstan::sampling].
#' * `Y_obs`: a vector of observed outcomes. For `model = "binomial"`, this will equal to `Y/pop`. For For `model = "normal"`, this will equal to `Y`.
#' * `Y_est`: a data.frame of predicted outcomes, corresponding to `obs`.
#' * `model`: model family, one of "binomial" or "normal".
#'
#' @export
#'
HIfit <- function(
  X_fix, X_hier, Y, Ysd, Nsam, isDat,
  model = c("binomial", "normal"), ...
){


  if(!model %in% c("binomial", "normal")) stop("Invalid model argument. Must be one of 'binomial', 'normal'. ")
  if(missing(Nsam)) stop("Missing required argument: Nsam")
  if(model == "binomial"){
    Y <- as.integer(round(Y*Nsam))
    Nsam <- as.integer(Nsam)
    Ysd <- NULL
    exp_name <- c("pos_pd")

  }
  if(model == "normal"){
    if(missing(Ysd)) stop("Missing required argument: Ysd")
    Y <- as.numeric(Y)
    Nsam <- as.numeric(Nsam)
    Ysd <- as.numeric(Ysd)
    exp_name <- c("eta", "sig")
  }

  D <- list(
    n_obs = length(Y),
    isDat = isDat,
    n_x = NCOL(X_fix),
    n_h = NCOL(X_hier),
    k_vect = apply(X_hier, 2, max),

    X_fix = X_fix,
    X_hier = X_hier,
    Y = Y,
    pop = Nsam,
    sdv = Ysd,
    pop_0 = as.integer(mean(Nsam))
  )

  #
  cat("Fitting a", model, "model for aggregated data ... \n")
  cat("Data: ", sum(D$isDat>0), "observations/", sum(D$isDat==0), "missings \n")
  cat("Parameters: ", NCOL(D$X_fix), "fix effects/", sum(D$k_vect), "varying intercepts/", NCOL(D$X_hier),"hierarchy \n")


  #
  cat("Compiling ... \n")
  if(model == "binomial"){
    Mod <- rstan::stan_model(model_code = TBDstan_models$HIfit_binomial)
  } else if (model == "normal"){
    Mod <- rstan::stan_model(model_code = TBDstan_models$HIfit_normal)
    #Mod <- rstan::stan_model("prep/stan/HIfit_normal.stan")
  }

  cat("Sampling ... \n")
  res <- rstan::sampling(object = Mod, data = D,
                         chains = 4, core= 4,
                         iter = 1500, warmup = 1000,
                         control=list(adapt_delta = .8, max_treedepth = 8))

  cat("Exporting ... \n ")
  Y_est <- lapply(exp_name, function(xv){
    rstan::summary(res, xv)$summary %>%
      as.data.frame() %>%
      mutate(est = mean, est_l = `2.5%`, est_u = `97.5%`)%>%
      select(starts_with("est"))
  })
  names(Y_est) <- exp_name



  structure(
    list(
      fit = res,
      dat = D,
      model = model,
      Y_obs = Y,
      Y_est = Y_est

    ), class = "HIfit")
}


#' @describeIn HIfit Plot the observations versus the predictions of HIfit
#' @export
plot.HIfit <- function(x){
  ind <- x$D$isDat > 0
  y <- x$obs[ind]
  yp <- x$est[[1]]$est[ind]
  yp_l <- x$est[[1]]$est_l[ind]
  yp_u <- x$est[[1]]$est_u[ind]
  plot(y, yp, type = "n",
       xlab = "Observations",ylab = "Predictions",
       main = paste0("Imputation by ", x$model, " model"))
  segments(y, yp_l, y, yp_u, col = "grey85")
  points(y, yp, pch= 16, col = "grey45")
  abline(0, 1, col = "orange4")
}


