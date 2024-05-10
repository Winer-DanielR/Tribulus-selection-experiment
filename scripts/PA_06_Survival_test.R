# Test of survival package using island dataset


library(survival)
data(lung2)

lungsurvival <- Surv(lung$time, lung$status)
head(lungsurvival)

lungfit <- survfit(formula = lungsurvival ~ 1)
summary(lungfit, times = c(0,100,200,500,1000))

plot(survfit(formula = lungsurvival ~ 1, data=lung), 
     ylab = "Survival Proportion", 
     xlab = "Time",
     main = "Overall Lung Dataset Survival")

# Test using Santa Cruz 2018

Santa_Cruz <- read.csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2018.csv")
str(Santa_Cruz)

Santa_Cruz_survival <- Surv(Santa_Cruz$time, Santa_Cruz$Present)
head(Santa_Cruz_survival)

Santa_Cruz_fit <- survfit((formula = Santa_Cruz_survival ~ 1))
summary(Santa_Cruz_fit)

plot(Santa_Cruz_fit,
     ylab = "Survival Proportion",
     xlab = "Monitoring times",
     main = "Santa Cruz Survival")

survfit(formula = Santa_Cruz_survival ~ treatment + size, data = Santa_Cruz)

plot(survfit(formula = Santa_Cruz_survival ~ treatment + size, data = Santa_Cruz), 
     ylab = "Survival Proportion", 
     xlab = "Time", 
     main = "Santa Cruz Dataset Split by Variable",
     mark.time = T,
     lwd = 2,
     col = c("darkorange","darkorange2","darkorange3","darkorange4"),
     bty = "n",
     las = 1)
legend("topright",
       legend = names(Santa_Cruz$treatment))
       
