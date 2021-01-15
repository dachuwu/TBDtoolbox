#' @md
#' @title  Simulated dataset: Death registry with multiple causes
#'
#' @description
#' A dataset containing 250,000 records of multiple cause of death.
#'
#'
#' @details # Variables
#' * `id`: identity key
#' * `age`: age
#' * `sex`: sex
#' * `x1`-`x2`: true predictor variable (binary, can be NA)
#' * `x3`-`x4`: noisy predictor variable (binary, can be NA)
#' * `GUC`: main cause of death
#' * `MC1`-`MC5`: supplemental cause of death
#'
"multideath"
