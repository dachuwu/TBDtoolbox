## Demo: RedistGUC
require(devtools)
require(usethis)
#document()
load_all()

# load demo dataset
data("multideath")
require(TBDtoolbox)
# create a full gc_to_uc matrix
gucs <- sort(unique(multideath$GUC))
gcs <- gucs[98:107]
ucs <- gucs[1:97]
gc_to_uc <- matrix(1, 10, 97, dimnames = list(gcs, ucs))


# predictors have to be factors or characters
d <- multideath
d$x1 <- factor(d$x1)
d$x2 <- factor(d$x2)
d$x3 <- factor(d$x3)

# fit a NB model
fit1 <- GUCfit(
  formula = GUC ~ age + x1 + multi(GUC, MC1, MC2,MC3, MC4, MC5), #
  data = d, gc_to_uc = gc_to_uc,
  nm_id = "id", method = "MLR",
  prop_valid = 0.25, maxit = 100)

# summarizing the results
summary(fit1)

