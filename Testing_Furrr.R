library(future)
library(purrr)
library(furrr)

x <-  1:10
y <-  1:10

dummyfunction <- function(x){
  Sys.sleep(.5)
  x+x
}

ptm <- proc.time()

map(x, dummyfunction)

proc.time() - ptm


plan(multisession, workers = 4)


ptm <- proc.time()

future_map(x, dummyfunction)

proc.time() - ptm