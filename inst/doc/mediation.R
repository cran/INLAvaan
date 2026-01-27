## graph LR
##     X((X)) -->|a| M((M))
##     M -->|b| Y((Y))
##     X -->|c| Y

## -----------------------------------------------------------------------------
set.seed(11)
n <- 100  # sample size

# 1. Predictor
X <- rnorm(n)

# 2. Mediator (Path a = 0.5)
M <- 0.5 * X + rnorm(n)

# 3. Outcome (Path b = 0.7, Path c = 0)
Y <- 0.7 * M + rnorm(n) 

dat <- data.frame(X = X, Y = Y, M = M)


## -----------------------------------------------------------------------------
mod <- "
  # Direct effect (path c)
  Y ~ c*X

  # Mediator paths (path a and b)
  M ~ a*X
  Y ~ b*M

  # Define Indirect effect (a*b)
  ab := a*b

  # Define Total effect
  total := c + (a*b)
"


## -----------------------------------------------------------------------------
library(INLAvaan)
fit <- asem(mod, dat, meanstructure = TRUE)


## -----------------------------------------------------------------------------
summary(fit)


## -----------------------------------------------------------------------------
#| include: false
summ <- INLAvaan:::get_inlavaan_internal(fit)$summary
fmt <- function(x) sprintf("%.3f", x)

a     <- fmt(summ["a", "Mean"])
b     <- fmt(summ["b", "Mean"])
c     <- fmt(summ["c", "Mean"])
c_lo  <- fmt(summ["c", "2.5%"])
c_hi  <- fmt(summ["c", "97.5%"])
ab    <- fmt(summ["ab", "Mean"])
ab_lo <- fmt(summ["ab", "2.5%"])
ab_hi <- fmt(summ["ab", "97.5%"])
tot   <- fmt(summ["total", "Mean"])

