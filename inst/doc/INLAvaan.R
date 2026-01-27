## -----------------------------------------------------------------------------
#| label: setup
#| include: false
library(INLAvaan)
library(lavaan)
library(dplyr)


## -----------------------------------------------------------------------------
#| label: install-instructions
#| eval: false
# # Run this if you have not yet installed INLAvaan
# install.packages("pak")
# pak::pak("haziqj/INLAvaan")
# 
# # Load all libraries for this example
# library(INLAvaan)
# library(tidyverse)
# library(lavaan)


## -----------------------------------------------------------------------------
#| label: simulate-data
pop_mod <- "
  eta1 =~ 1*y1 + 0.8*y2 + 0.6*y3
  eta2 =~ 1*y4 + 0.8*y5 + 0.6*y6
  eta2 ~ 0.3*eta1
  
  # Variances
  y1 ~~ 0.5*y1
  y2 ~~ 0.5*y2
  y3 ~~ 0.5*y3
  y4 ~~ 0.5*y4
  y5 ~~ 0.5*y5
  y6 ~~ 0.5*y6
  eta1 ~~ 1*eta1
  eta2 ~~ 1*eta2
"
set.seed(123)
dat <- lavaan::simulateData(pop_mod, sample.nobs = 1000)
glimpse(dat)


## -----------------------------------------------------------------------------
#| label: fit-model
mod <- "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  eta2 ~ eta1
"
fit <- asem(mod, dat)


## -----------------------------------------------------------------------------
#| label: results-str
str(fit, 1)
fit


## -----------------------------------------------------------------------------
#| label: results-coef-summary
# Inspect coefficients
coef(fit)

# Summary of results
summary(fit)


## -----------------------------------------------------------------------------
#| label: results-pred
eta_preds <- predict(fit, nsamp = 100)
length(eta_preds)
head(eta_preds[[1]])


## -----------------------------------------------------------------------------
#| label: results-pred-summary
summ_eta <- summary(eta_preds)
str(summ_eta)
head(summ_eta$Mean)


## -----------------------------------------------------------------------------
#| label: results-plot
plot(fit)


## -----------------------------------------------------------------------------
mod2 <- "
  # A model with uncorrelated factors
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  eta1 ~~ 0*eta2
"
fit2 <- asem(mod2, dat)
compare(fit, fit2)


## -----------------------------------------------------------------------------
#| label: priors-default
blavaan::dpriors()


## -----------------------------------------------------------------------------
#| label: priors-global
DP <- blavaan::dpriors(theta = "gamma(1,1)", psi = "gamma(1,1)")
DP
## fit <- asem(mod, dat, dpriors = DP)  # not run


## -----------------------------------------------------------------------------
mod <- "
  eta1 =~ y1 + y2 + prior('normal(1,3)')*y3
  eta2 =~ y4 + y5 + y6
  eta2 ~ prior('normal(0,.5)')*eta1
"
## fit <- asem(mod, dat)  # not run

