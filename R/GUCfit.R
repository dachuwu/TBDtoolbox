#' @md
#' @name GUCfit
#' @title Predicting the Underlying Causes of Death
#' @description Fit a redistribution model to predict the Underlying Causes (UCs) from Garbage Codes (GCs). **NHIRC-Usable**
#' \loadmathjax
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' This should contain an outcome variable (as the underlying causes) and predictor variables if any.
#' Predictor variables put inside `multi()` are recognized as multiple causes. See *Details &sect;1* also.
#' @param data a data.frame or a list (with equal-length vectors) containing all the variables.
#' @param gc_to_uc a named matrix specifying the *a priori* constraints for GC-UC mapping.
#' The row names are used as the Garbage Code (GC) levels and the column names are used as the Underlying Cause (UC) levels.
#' This is a required argument. See *Details &sect;2* also.
#' @param nm_id variable name of the identity key for the individual record.
#' @param method one of the following redistribution model, See *Details &sect;3* also.
#' * "NB": Naive Bayes Classifier (default).
#' * "MLR": Multinomial Logistic Regression implemented by [nnet::multinom()].
#' @param prop_valid proportion of data used in validation (default to 0.2). See *Details &sect;4* also.
#' @param ... the following optional arguments are passed to redistribution methods.
#'
#' * `alp = 0.1` (default): The smoothing parameter in "NB" method , i.e. the additional counts added to all strata of the conditional probabilities.
#' * `maxit = 100` (default): Maximum number of iterations in "MLR" method. Additional arguments for \code{nnet} can also be specified here. See [nnet::nnet()] for details.
#'
#'
#' @details
#' ## &sect;1. Specify the model formula
#' The `formula` argument takes the general model form as in `lm()` or `glm()`. The form should be like `GUC ~ x1 + x2 + multi(MC1, MC2)`,
#' where `GUC` is the name of the outcome variable (the underlying causes of death , UCs). The RHS of `~` contains the names of predictors used by the model. Here,
#' `x1` and `x2` represent the normal predictor variables as in common regression models. `multi(...)` is used to specify the multiple causes of death (here, `MC1` and `MC2`).
#' `multi(...)` is treated differently in different methods. In "NB", `multi(...)` seen as item-sets to calculate the conditional probabilities. See the reference paper for details.
#' In "MLR", `multi(...)` is transformed into many binary variables indicating whether one cause of death item exist. **There should be only one `multi(...)` term in the formula**.
#' Also, **only factor or character variables are accepted**.
#'
#' ## &sect;2. Specify the GC-UC mapping constraints
#' The GC-UC mapping constraints `gc_to_uc` should be a named matrix. **The row names and column names are required** as the row names define the GC categories,
#' and the column names define the UC categories. The entries of this matrix (\mjseqn{A}) should be binary, so that \mjseqn{A_{ij} = 1} denotes
#' the permission to redistribute \mjseqn{i}-th GC category to \mjseqn{j}-th UC category, otherwise, \mjseqn{A_{ij} = 0}.
#'
#' ## &sect;3. Redistributing GCs to UCs
#' Records with UCs (defined in `gc_to_uc`) in the outcome variable are used to train the "NB" or "MLR" model. Then, the trained model is used to
#' predict the UCs for those having GC outcomes. Generally, "NB" is recommended as it better handles missing data and large number of UC categories (more accurate and efficient) .
#' However, "MLR" can perform better with more complete data and small number of UC categories. We recommend using validation procedure to compare the two methods
#' before full implementation.
#'
#' ## &sect;4. Validation and the error measures
#' When the proportion of validation (`prop_valid` = 0.2 by default) is greater than zero, a random proportion of records with UCs is erserved for validation.
#' Binary and cross entropy error measures are used to evaluate the model performance.
#' Use `summary()` to the returned `GUCfit` object to see the average errors in the training and validation partition.
#'
#' @return A `GUCfit` object containing the following components.
#'
#' * `formula`: The formula same as the input
#' * `pred_GUC`: The the predicted UC probabilities for each GC record. A data.frame where
#'    the row identifying the individual records, and the column identifying the UC categories.
#'    The  key `nm_id` is preserved to identify individual predictions.
#' * `dat_info`: A data.frame summarizing the no. of records used for training, validation, and prediction.
#' * `error_info`: A data.frame summarizing the error measures
#' * `fit`: The fitted model.
#' * `gcs`: A character vector listing the GC levels, same as the rownames of `gc_to_uc`.
#' * `ucs`: A character vector listing the UC levels, same as the colnames of `gc_to_uc`.
#' * `method`: The modeling method.
#'
#' @examples
#' \dontrun{
#' # load demo dataset
#' data("multideath")
#'
#' # create a full gc_to_uc matrix
#' gucs <- sort(unique(multideath$GUC))
#' gc_to_uc = matrix(1, 10, 97, dimnames = list(gucs[98:107], gucs[1:97]))
#'
#' # predictors have to be factors or characters
#' d <- multideath
#' d$x1 <- factor(d$x1)
#' d$x2 <- factor(d$x2)
#' d$x3 <- factor(d$x3)
#'
#' # fit a NB model
#' fit1 <- GUCfit(
#'   formula = GUC ~ age + x1  + x2  + x3 + multi(MC1, MC2, MC3),
#'   data = d, gc_to_uc = gc_to_uc,
#'   nm_id = "id", method = "NB", prop_valid = 0.2)
#'
#' # summarizing the results
#' summary(fit1)
#' }
#'
#' @seealso [multideath] for the demo dataset, [nnet::multinom] for the underlying MLR method.
#' @references Ng, T. C., Lo, W. C., Ku, C. C., Lu, T. H., & Lin, H. H. (2020). Improving the use of mortality data in public health: A comparison of garbage code redistribution models. American journal of public health, 110(2), 222-229.
#'
#' @export
GUCfit.formula <- function(formula, data, gc_to_uc, nm_id = "id",
                   method = c("NB", "MLR"), prop_valid = 0.2, ...){

  err_binary <- function(ref, lpmat){
    yhat <- apply(lpmat, 1, function(lp) names(which.max(lp)))
    return(as.integer(yhat != as.character(ref)))
  }
  err_ce <- function(ref, lpmat){
    ref <- as.character(ref)
    ces <- sapply(1:NROW(ref), function(i) -lpmat[i,ref[i]] )
    return(ces)
  }

  func_mod <- switch(method, NB = gucfit_nb, MLR = gucfit_mlr)
  lab_mod <- switch(method, NB = "Naive Bayes classifier",
                    MLR = "Multinomial Logistic Regression")

  # levels of uc/gc
  if(is.null(rownames(gc_to_uc))) stop("gc_to_uc must have the GC levels as its row names.")
  gcs <- rownames(gc_to_uc)
  if(is.null(colnames(gc_to_uc))) stop("gc_to_uc must have the UC levels as its column names.")
  ucs <- colnames(gc_to_uc)
  if(!all(gc_to_uc %in% c(0, 1))) stop("The entries of gc_to_uc must be binary.")

  # variable names
  nm_y <- deparse(formula[[2]])
  avs <- rownames(attr(terms(formula),"factors"))
  avs <- avs[!avs %in% nm_y]
  ptn <- "^multi\\(([0-9A-Za-z, ]+)\\)$"
  nm_mc <- avs[grepl(ptn, avs)]
  if(length(nm_mc) > 1) stop("More than 1 multi(...) predictor detected.")
  nm_x <- avs[!grepl(ptn, avs)]
  nm_mc <- gsub(ptn, "\\1", nm_mc)
  nm_mc <- unlist(strsplit(nm_mc, ", "))

  # process data
  Dat <- gucproc_dat(nm_y, nm_x, nm_mc, nm_id,
                     as.data.frame(data),
                     ucs, gcs, prop_valid)

  dat_info <- data.frame(
    n.total = NROW(data), n.train = NROW(Dat$train),
    n.val = NROW(Dat$val), n.pred = NROW(Dat$pred)
  )

  # fit model
  cat("Method:", lab_mod, "\n")
  fit <- func_mod(nm_y, nm_x, nm_mc, dat = Dat$train, ucs, gcs, ...)
  cat("Training completed. \n")

  # in-sample error
  plp_train <- predict(fit, newdata = Dat$train, log.p = T)
  er_train_b <- err_binary(ref = Dat$train[,nm_y], lpmat = plp_train)
  er_train_c <- err_ce(ref = Dat$train[,nm_y], lpmat = plp_train)

  # out-sample error
  if(!is.null(Dat$val)){
    plp_val <- predict(fit, newdata = Dat$val, log.p = T)
    er_val_b <- err_binary(ref = Dat$val[,nm_y], lpmat = plp_val)
    er_val_c <- err_ce(ref = Dat$val[,nm_y], lpmat = plp_val)
    cat("Validation completed. \n")
  } else {
    er_val_b <- er_val_c <- NA
  }

  error_info <- data.frame(
    partition = c("training", "validation"),
    binary = round(c(mean(er_train_b), mean(er_val_b)), 3),
    cross_entropy = round(c(mean(er_train_c), mean(er_val_c)), 3)
  )

  # prediction
  pp_pred <- predict(fit, newdata = Dat$pred, log.p = F)

  tmp <- t(sapply(1:NROW(pp_pred), function(i){
    pbs <- pp_pred[i, ] * gc_to_uc[as.character(Dat$pred[i, nm_y]),]
    pbs/sum(pbs)
  }))

  pred_GUC <- cbind(Dat$pred[nm_id], as.data.frame(tmp))
  cat("Prediction completed. \n")

  # output check
  if(!any(grepl("gucfit_", class(fit)))) stop("Invalid model fit.")
  if(!all(round(rowSums(pred_GUC[,-1]), 3)==1)) stop("Invalid pred_GUC.")

  structure(
    list(
      formula = formula,
      pred_GUC = pred_GUC,
      dat_info = dat_info,
      error_info = error_info,
      fit = fit,
      gcs = gcs, ucs = ucs,
      method = lab_mod
    )
    , class = "GUCfit")
  }

gucproc_dat <- function(nm_y, nm_x, nm_mc, nm_id, data,
                        ucs, gcs, prop_valid){

  add_na_fac <- function(x, xnm, lvs){
    x <- as.character(x)
    ind <- is.na(x)
    if(any(ind)){
      message("NA_ category created for variable '", i, "'.")
      x[ind] <- "NA_"
    }

    if(missing(lvs)) lvs <- sort(unique(x))
    factor(x, levels = lvs)
  }

  # check
  for(i in c(nm_x, nm_mc)){
    if(!class(data[,i]) %in% c("factor", "character"))
      stop("Variable '", i, "' is not a factor or character.")
  }
  for(i in nm_mc){
    if(!all(as.character(data[,i][!is.na(data[,i])]) %in% gucs))
      warning("'", i, "' contains unknown UC/GC items.
              \nPlease make sure the multiple causes are specified correctly.")
  }

  # manage level
  gucs <- c(ucs, gcs)
  for(i in nm_x) data[,i] <- add_na_fac(data[,i], i)
  for(i in nm_mc) data[,i] <- factor(as.character(data[,i]), levels = gucs)
  data[,nm_y] <- add_na_fac(data[,nm_y], gucs)

  # data split
  data <- data[, c(nm_id, nm_y, nm_x, nm_mc)]
  df_pred <- data[data[, nm_y] %in% gcs,]
  df_train <- data[data$GUC %in% ucs,]

  if(prop_valid == 0){
    cat("Garbage code redistribution without validation \n")
    df_val <- NULL
  }else if (prop_valid < 1){
    cat("Garbage code redistribution with validation on",round(prop_valid*100),"% data. \n")
    ind <- sample.int(NROW(df_train), round(NROW(df_train)*prop_valid))
    df_val <- df_train[ind, ]
    df_train <- df_train[-ind, ]
  }else if (prop_valid >= 1){
    stop("prop_valid must be < 1.")
  }

  out <- list(train = df_train, val = df_val, pred = df_pred)
  return(out)
}


#' @rdname GUCfit
#' @export
GUCfit <- function(formula, ...){
  UseMethod("GUCfit")
}


#' @describeIn GUCfit Print the basics (GC/UC levels, redistribution method) of GUCfit
#' @export
print.GUCfit  <- function(x){
  cat("Garbage code redistribution model fitted\n------\n")
  cat("Formula:\n\t", deparse(x$formula), "\n")
  cat("Method:\n\t", x$method, "\n\n")
  cat("Data info:\n")
  cat("\tTotal records:", x$dat_info$n.total, "\n")
  cat("\tNo. training records:", x$dat_info$n.train, "\n")
  cat("\tNo. validation records:", x$dat_info$n.val, "\n")
  cat("\tNo. prediction records:", x$dat_info$n.pred, "\n\n")
}


#' @describeIn GUCfit Print the details (variable importance, errors) of GUCfit
#' @export
summary.GUCfit <- function(x){

  txt_gc <- ifelse(
    length(x$gcs>6),
    paste0(paste0(head(x$gcs, 3), collapse = ", "), ", ..., ", paste0(tail(x$gcs, 3), collapse = ", ")),
    paste0(x$gcs, collapse = ", ")
  )
  txt_uc <- ifelse(
    length(x$ucs>6),
    paste0(paste0(head(x$ucs, 3), collapse = ", "), ", ..., ", paste0(tail(x$ucs, 3), collapse = ", ")),
    paste0(x$ucs, collapse = ", ")
  )
  err_b <- formatC(x$error_info$binary, width = 13)
  err_c <- formatC(x$error_info$cross_entropy, width = 13)

  print(x)
  cat("GUC levels:\n")
  cat("\t",length(x$gcs), "GCs :", txt_gc, "\n")
  cat("\t",length(x$ucs), "UCs :", txt_uc, "\n\n")
  summary(x$fit)
  cat("\nError info:\n")
  cat("\tPartition \t        Binary \t Cross entropy \n")
  cat("\t--------- \t        ------ \t ------------- \n")
  cat("\tTraining \t", err_b[1],"\t", err_c[1]," \n")
  cat("\tValidation \t", err_b[2],"\t", err_c[2]," \n")
}

