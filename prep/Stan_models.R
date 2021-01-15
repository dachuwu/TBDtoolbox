# prepare stan models
require(tidyverse)
fps <- dir("./prep/stan")

TBDstan_models <- lapply(1:length(fps), function(i){
  paste0(readLines(paste0("./prep/stan/", fps[i])), collapse = "\n" )
})
names(TBDstan_models) <- str_replace_all(fps, ".stan", "")


usethis::use_data(TBDstan_models, overwrite = T)

