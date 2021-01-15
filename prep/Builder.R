# build package
require(devtools)
require(usethis)
require(testthat)

use_package("mathjaxr", "suggests")

## license
use_mit_license(name  = "Taiwan Burden of Disease Center")

## Git
use_git()



## function
use_r("GUCfit")
use_r("gucfit_")
use_r("data_risk_factors")

use_r("HIfit")


## documentation
document()
use_vignette("GUCfit", "Predicting the Underlying Causes of Death")


## test
use_testthat()
