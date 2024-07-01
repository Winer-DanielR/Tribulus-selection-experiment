# Mixed effects Cox regression
# By Daniel Reyes
# May 2024

# Mixed effects cox regression models are used
# to model survival data when there are
# **repeated measures on an individual (same mericarps over time)**,
# **nested within some hierarchy ** (Spine nested within Size)
# **include random effects** (Color and Mark position)
# 
# Using the survival and coxme packages.
# 
# Examples of mixed cox models ####
## Simulated data ####
set.seed(10)
N <- 250
dat <- data.frame(ID = factor(1:N),
                  age = rnorm(N, mean = 45,
                              sd = 5),
                  sex = sample(0:1, N, T),
                  basemort = rnorm(N, sd = 3))
interval <- matrix(sample(2:14, N*3, replace = T), N)
windows <- t(apply(cbind(0, interval), 1, cumsum))
windows <- rbind(windows[,1:2], windows[,2:3], windows[,3:4])

colnames(windows) <- c("time1", "time2")
dat <- cbind(do.call(rbind, rep(list(dat), 3)), windows)
dat <- dat[order(dat$ID), ]
dat$assessment <- rep(1:3, N)
rownames(dat) <- NULL
head(dat)

## Simulate survival (mortality) data
transplant <- with(dat, {
  mu <- (0.05 * age) + (0.3 * time2)
  lp <- rnorm(N*3, mean = mu, sd = 1)
  as.integer(lp > quantile(lp, probs = 0.655))
})

# Ensure transplants do not revert
transplant <- as.integer(ave(transplant, dat$ID, FUN = cumsum) >= 1)

# Simulate survival (mortality data)
mortality <- with(dat, {
  mu <- basemort + (0.05 * age) - (2.5 * sex) + (0.3 * time2)
  lp <- rnorm(N*3, mean = mu, sd = 1)
  as.integer(lp > median(lp))
})

# Ensure that when someone dies, he or she stays dead
mortality <- as.integer(ave(mortality, dat$ID, FUN = cumsum) >= 1)

# Ensure no one dead at baseline
mortality[dat$assessment == 1] <- 0

# Ensure no post mortem change in transplant status
transplant <- unlist(by(data.frame(mortality, transplant),
                        dat$ID, FUN = function(x){
                          i <- cumsum(x$mortality)
                          tstat <- x$transplant[i == 1]
                          x$transplant[i >= 1] <- tstat
                          return(x$transplant)
                        }))

dat$transplant <- transplant
dat$mortality <- mortality

# Print few rows
head(dat)

# Mixed cox regression ####
## Basic models - simple model ####
m <- coxph(Surv(time1, time2, mortality) ~ 
             age + sex + transplant, data = dat)
# Summary
summary(m)

# Model with robust SE via clustering
m2 <- coxph(Surv(time1, time2, mortality) ~ 
              age + sex + transplant + cluster(ID), #clusted mericarp
            data = dat)
# Summary
summary(m2)

## model with a frailty term (slower)
m3 <- coxph(Surv(time1, time2, mortality) ~ age + sex + transplant + frailty(ID, 
                                                                             distribution = "gaussian", sparse = FALSE, method = "reml"), data = dat)
## show model results
m3

# Cox model with random effects ####
# Observations on ind. each individual
# likely has their own baseline so adjust with a mixed
# effects cox model using the coxme package

m4 <- coxme(Surv(time1, time2, mortality)
            ~ age + sex + transplant
            + (1|ID), data = dat)
## Just print, no summary
m4

## Get profile likelihood for variance values to examine,
## based on observed .486

s <- seq(0.01, 0.4, length.out = 200)^2
res <- sapply(s, function(i) {
  mod <- coxme(Surv(time1, time2, mortality) ~ age + sex + transplant + 
                 (1 | ID), data = dat, vfixed = i)
  2 * diff(mod$loglik)[1]
})


## profile likelihood, horizontal line is 95% CI lower blound
## includes 0, upper bound lools a little under .8

plot(sqrt(s), res, type = "l", xlab = expression(SD[i]), ylab = "2 * LL")
abline(h = 2 * diff(m4$loglik)[1] - qchisq(0.95, 1), lty = 2)

## compare models
summary(m)  # regular cox no adjust for repeated measures
m4
