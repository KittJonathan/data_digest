# How to use the 4 different norm() functions in R
# The Data Digest
# https://www.youtube.com/watch?v=X5NXDK6AVtU

# rnorm(): random number generator
# dnorm(): probability density function (PDF)
# pnorm(): cumulative probability distribution
# qnorm(): quantile distribution (reverse pnorm)

# rnorm() ----

rnorm(n = 1)
rnorm(1)
rnorm(1)

set.seed(42)
rnorm(1)

set.seed(42)
rnorm(1)

rnorm(30)

options(digits = 4)
rnorm(30)
sort(rnorm(30))

sort(rnorm(n = 50, mean = 178, sd = 6))

dnorm(x = seq(from = -3, to = 3, by = 0.1))

# pnorm() ----

pnorm(mean = 178, sd = 6, q = 180)
# prob of measuring 180cm or less for a population of mean height 178cm and sd 6cm

pnorm(q = c(160, 170, 178, 180, 190, 200), mean = 178, sd = 6)

pnorm(q = seq(from = -3, to = 3, by = 0.1))

pnorm(q = c(0.67, 1, 2, 2.5, 3))

# qnorm() ----

qnorm(p = 0.99, mean = 178, sd = 6)
# what height to be in the top 99th percentile?

pnorm(q = 190, mean = 178, sd = 6) - pnorm(q = 170, mean = 178, sd = 6)
# 88.6% of the population falls between 170cm & 190cm