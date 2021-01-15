#' @md
#' @title  Demo dataset: Estimates for Risk factors  (Smoking)
#'
#' @details # Variables
#' * `age`: age group
#' * `sex`: sex
#' * `year`: year
#' * `city`: city, MOI format (refer to [TWpop_city])
#' * `region`: region (refer to [TWpop_region])
#' * `nsam`: no. of the samples surveyed
#' * `csam`: no. of cases from the surveyed
#' * `prev` (`prev_l`-`prev_u`): prevalence (95% CI)
#' * `mean_x`/`sd_x`: mean and standard deviation of cigarettes per day (日根)
#' * `x_pth0`-`x_pth100`: minimum and maximum of cigarettes per day (日根)
#' * `mean_z`/`sd_z`: mean and standard deviation of packs per year (年包)
#' * `z_pth0`-`z_pth100`: minimum and maximum of packs per year (年包)
#'
#' @family risk factor estimates
#' @source TBDC
"risk_smoke"


#' @md
#' @title  Demo dataset: Estimates for Risk factors  (Drinking)
#'
#' @details # Variables
#' * `age`: age group
#' * `sex`: sex
#' * `year`: year
#' * `city`: city, MOI format (refer to [TWpop_city])
#' * `region`: region (refer to [TWpop_region])
#' * `nsam`: no. of the samples surveyed
#' * `csam`: no. of cases from the surveyed
#' * `prev` (`prev_l`-`prev_u`): prevalence (95% CI)
#' * `mean_x`/`sd_x`: mean and standard deviation of gram alcohol per day (日克)
#' * `x_pth0`-`x_pth100`: minimum and maximum of gram alcohol per day (日克)
#'
#' @family risk factor estimates
#' @source TBDC
"risk_drink"


#' @md
#' @title  Demo dataset: Estimates for 3H Risk factors  (blood pressure, glucose, LDL)
#'
#' @details # Variables
#' * `risk`: type of risk, one of "BP" (systolic blood pressure), "Glucose" (blood sugar), and "LDL" (low-density lipoprotein)
#' * `age`: age group
#' * `sex`: sex
#' * `year`: year
#' * `city`: city, MOI format (refer to [TWpop_city])
#' * `region`: region (refer to [TWpop_region])
#' * `mean_x`/`sd_x`: mean and standard deviation of risk exposure
#' * `nsam`: _ad hoc_ sample size estimates (proportional to population, sum to ~7000)
#'
#' @family risk factor estimates
#' @source TBDC
"risk_3H"
