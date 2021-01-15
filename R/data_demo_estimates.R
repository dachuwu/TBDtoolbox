#' @md
#' @title  Demo dataset: Deaths attributable to PM.2.5 (Taiwan 2013)
#'
#' @description
#' A dataset containing the number of attributable deaths to PM2.5 exposure by townships. The values are simulated
#' from the original dataset.
#'
#' @format A data frame with `r NROW(attri_death_pm25)` rows and  `r NCOL(attri_death_pm25)` variables:
#'
#' @details # Variables
#' * `town`: town code, MOI format (preferred)
#' * `towncode_new`: town code, MOH format
#' * `IHD_att_death`: IHD deaths attributable to PM2.5
#' * `stroke_att_death`: stroke deaths attributable to PM2.5
#' * `lung_att_death`: lung cancer deaths attributable to PM2.5
#' * `copd_att_death`: COPD deaths attributable to PM2.5
#' * `all_att_death`: Total deaths attributable to PM2.5
#'
#' @family demo datasets
#' @source TBDC
"attri_death_pm25"



#' @md
#' @title Demo dataset: DALY estimates of cancers (level-3)
#'
#' @description
#' A dataset containing the DALY, YLD and YLL of level-3 cancers (Taiwan 2015).
#'
#' @format A data frame with `r NROW(daly_B01)` rows and `r NCOL(daly_B01)` variables:
#'
#' @details # Variables
#' * `sex`: sex (0 = female and 1 = male)
#' * `town`: town code, MOI format.
#' * `year`: 2015
#' * `cause_l3`: level-3 cause code
#' * `daly_count`: DALY estimates (person-year)
#' * `yld_count`: YLD estimates (person-year)
#' * `yll_count`: YLL estimates (person-year)
#' * `population`: population size
#'
#' @family demo datasets
#' @source TBDC
"daly_B01"
