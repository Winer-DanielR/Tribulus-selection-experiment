# Description -------------------------------------------------------------
##########################################################################################
# Created by Marc-Olivier Beausoleil
# McGill University 
# Created November 7, 2022
# Why:
# What is the fitness function of different mericarp populations 
# Requires 
# NOTES: 
##########################################################################################

# Source scripts ----------------------------------------------------------
invlogit <- function(x) {exp(x)/(1+exp(x))}


# Reading the data  -------------------------------------------------------
trib.data = read.csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
# Making the data ready for the model 
trib.data$island = as.factor(trib.data$island)
isl = levels(trib.data$island)

# Summary of data 
head(trib.data)
summary(trib.data)
ftable(trib.data$year~trib.data$island)


# levels(trib.data$island)
table(trib.data$island)

# remove NAs 
trib.data = trib.data[-which(is.na(trib.data$length)),]

# Making data for polynomial 
trib.data$x.l = trib.data$length
trib.data$x.l2 = trib.data$length^2

# Model GLM

# intercept model 
glmint = glm(eaten ~ 1 , data = trib.data, family = binomial(link = "logit"))
# Model only linear length 
glmout.l = glm(eaten ~ length + island, data = trib.data, family = binomial(link = "logit"))
# Model with polynomial 
glmout = glm(eaten ~ x.l+ x.l2 + island, data = trib.data, family = binomial(link = "logit"))
# glmout = glm(eaten ~ poly(x = length, 2), data = trib.data, family = binomial(link = "logit"))
# glmout = glm(eaten ~ poly(x = length, 2) + island, data = trib.data, family = binomial(link = "logit"))
# glmout = glm(eaten ~ length + island-1, data = trib.data, family = binomial(link = "logit"))
summary(glmout)

# Compare models 
anova(glmint, glmout.l,glmout, test = "Chisq")
AIC(glmint,glmout,glmout.l)

# Showing the correlation of the data 
plot(trib.data[,c("width","length","depth")])

# Printing the image 
png(filename = "output/fitness.surface.png", width = 7, height = 7 , units = "in", res = 300)

# Empty plot 
plot(trib.data$length, 
     trib.data$eaten, 
     type="n", 
     ylim = c(0,1), 
     xlab = "Mericarp length",
     main = "GLM all data")

# setting parameters 
w = 1
v = .01

# Looping through all islands 
for (j in isl) {
  print(j)
  
  # Subset Islands 
  tmp = trib.data[trib.data$island %in% j,]
  
  # Adding phenotypic means 
  points(x = mean(tmp$length), y = .7, 
         pch = 19, col = w, cex = 3)
  
  # Model per island 
  glmout = glm(eaten ~ x.l+ x.l2, data = tmp, family = binomial(link = "logit"))
  
  # Predict the islands 
  new.seq = seq(min(trib.data$length, na.rm = TRUE), max(trib.data$length, na.rm = TRUE), length.out = 100)
  newdata = data.frame(x.l = new.seq,  
                       x.l2 = new.seq^2)
  z1 <- predict(glmout, newdata = newdata, se.fit = TRUE)
  
  yhat <-  invlogit(z1$fit)
  upper <- invlogit(z1$fit + z1$se.fit)
  lower <- invlogit(z1$fit - z1$se.fit)
  
  # Add fitness functions 
  lines(newdata$x.l, yhat, lty = 1, col = w , lwd = 5)
  
  # Adding the points 
  points(tmp$x.l,ifelse(tmp$eaten == 1, tmp$eaten-v, tmp$eaten+v), col = w, pch = 19) 
  
  # Error around the lines 
  lines(newdata$x.l, upper, lty = 2,col = w, lwd = .5)
  lines(newdata$x.l, lower, lty = 2,col = w, lwd = .5)
  
  # Make the parameters increment 
  w  = w  + 1
  v  = v  + .01
}

# Add legend of islands 
legend("topleft", 
       bg = scales::alpha("white", alpha = .5),
       legend = isl, lwd = 5,
       col = 1:length(isl))

# Clear the image 
dev.off()
