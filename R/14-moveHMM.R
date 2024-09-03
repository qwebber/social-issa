



## load libraries
libs <- c('data.table', 'moveHMM')
lapply(libs, require, character.only = TRUE)

devtools::install_github("TheoMichelot/moveHMM", build_vignettes = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN-N20.RDS")

DT[, .N, by = c("IDYr")]

### moveHMM ----

# TODO: randomize + compare m
stepPar0 = c(100, 1000, 100, 1000, 0.01, 0.001)
anglePar0 = c(pi, 0, 0.1, 2)

m <- fitHMM(
  data = DT,
  nbStates = 2,
  stepPar0 = stepPar0,
  anglePar0 = anglePar0
)

# TODO: Some of the parameter estimates seem to lie close to the boundaries of their parameter space.
#       The associated CIs are probably unreliable (or might not be computable).

DT[, state := viterbi(m)]

