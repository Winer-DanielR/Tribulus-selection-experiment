################Tribulus Selection experiments Dataset##############

####### Goal: Plot the S estimates show comparisons between methods, traits and islands ####


S_estimates <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus Selection experiment/Data/Processed/S_estimates.csv")
str(S_estimates)
S_estimates <- S_estimates %>% mutate_at(vars(island, trait), list(factor))

point_in_time <- S_estimates$S_PT
mark_recapture <- S_estimates$S_MR

color_easy <- c("red", "blue", "black")[S_estimates$island]
pcheasy <- c(21,22,23,24)[S_estimates$trait]

plot_estimates <- plot(x=point_in_time, y=mark_recapture, main = "S* Estimates (mean non eaten - mean eaten)",
      xlab = "Point in Time S* estimates", ylab =  "Mark Recapture S* estimates", 
      col = color_easy, pch = pcheasy, bg = color_easy, cex = 1.5)

legend("topleft", legend = c("depth", "length", "longest spine", "width"), pch = c(16,15,18,17), cex = 1.1)
legend("topright", legend = c("Floreana", "Isabela", "Santa Cruz"), pch = c(15),
       col = color_easy, cex = 1.2)
abline(v=0, h=0, col = "black")
abline(a=0, b=1, col = "red")


