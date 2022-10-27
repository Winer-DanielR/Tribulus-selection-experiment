################Tribulus Selection experiments Dataset##############

####### Goal: Plot the effect of time by relating measurements of the start and the end of the experiment ####
# This was done using mericarps recovered from Santa Cruz island. I missed the information on the other two.

mericarp_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Time effect summary.csv")
str(mericarp_time)
# Changed variables to factors
mericarp_time <- mericarp_time %>% mutate_at(vars(year, trait), list(factor))

#Separated 2018 and 2019 values
# mericarp_time_final <- pivot_wider(mericarp_time, names_from = year, values_from = c(mean, sd, se))

time_plot <- ggplot(mericarp_time) +
 aes(x = year, y = mean, group = trait, colour = trait) + geom_point(size = 4) + geom_line(size = 1.5) + 
    labs(x = "Year", y = "Size (mm)", title = "Size differences of mericarps", subtitle = "Selection experiment Santa Cruz") + theme(legend.key = element_rect(fill = "gray100", 
    size = 0, linetype = "solid")) + theme(axis.title = element_text(size = 15), 
    axis.text = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text.y = element_text(size = 15), 
    plot.title = element_text(size = 15))




