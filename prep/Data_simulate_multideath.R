## Simulate death record data with multiple causes of death
require(tidyverse)

icd2cause <- read_csv("./prep/raw_data/icd10toyllcause.csv") %>%
  filter(!duplicated(cause_outline) & cause_outline != "GC")

ucs <- sort(unique(icd2cause$GUC))
gcs <- rep(
  paste0("GC", formatC(1:10, width = 2, flag = 0)), times = c(4, 12, 20, 14, 12, 5, 7, 5, 4, 14)
) %>%
  setNames(nm = ucs)

tmp <- by(ucs, gcs, function(x){
  k <- length(x)
  a <- diag(0.6, k, k)
  dimnames(a) <- list(x, x)
  for(i in 1:k){
    for(j in 1:k){
    if(abs(i-j) == 1) a[i,j] = 0.15
    if(abs(i-j) == 2) a[i,j] = 0.05
    }}
  a
}, simplify = F)

K <- length(ucs)
pmat <- diag(0, K, K)
dimnames(pmat) <- list(ucs, ucs)
for(m in 1:length(tmp)){
  pmat[rownames(tmp[[m]]), colnames(tmp[[m]])] = tmp [[m]]
}



draw_guc <- function(n){

  sdf <- icd2cause[sample.int(NROW(icd2cause),size = n, replace = T),] %>%
    mutate(
       id = 1:n(),
       tCause = cause_outline,
       tUC = GUC,
       yll_amax = pmin(95, yll_amax),
       sex = case_when(
         male + female == 2 ~ sample(c("m","f"), size = n, replace = T),
         male == 1 ~ "m",
         T ~ "f"),
       x1 = ifelse(substr(tUC, 1, 1) == "C", "1", "0"),
       x2 = ifelse(substr(tUC, 1, 1) == "A", "1", "0"),
       x3 = sample(c("0", "1"), n, T),
       x4 = sample(c("0", "1"), n, T)
           )%>%
    mutate(
      age = map2_dbl(yll_amin, yll_amax, function(x, y) runif(1, x, y)) %>% round,
      age = cut(age, breaks = c(0, 5, 25, 40, 60, 85, 100), include.lowest = T, right = F),
      x1 = ifelse(runif(n) < 0.03, NA, x1),
      x2 = ifelse(runif(n) < 0.03, NA, x2),
      x3 = ifelse(runif(n) < 0.03, NA, x3),
      x4 = ifelse(runif(n) < 0.03, NA, x4),
    )%>%
    select(id, tCause, tUC, age, sex, x1, x2, x3, x4)

  mcs <- lapply(1:n, function(i){
    ps <- pmat[sdf$tUC[i], ]
    nn <- rpois(1, 2.5) + 1
    tmp <- rmultinom(1, nn, ps)
    out_uc <- rep(names(ps), times = tmp[,1])
    out_gc <- gcs[out_uc]
    out <- if_else(runif(nn) < 0.2, out_gc, out_uc)
    out <- unique(out)
    names(out) <- NULL
    c(out, rep(NA, 10))
  })

  sdf$GUC <- map_chr(mcs, 1)
  sdf$MC1 <- map_chr(mcs, 2)
  sdf$MC2 <- map_chr(mcs, 3)
  sdf$MC3 <- map_chr(mcs, 4)
  sdf$MC4 <- map_chr(mcs, 5)
  sdf$MC5 <- map_chr(mcs, 6)
  sdf%>%
    select(-tCause, -tUC)
}



multideath <- draw_guc(25e4)


usethis::use_data(multideath, overwrite = T)











