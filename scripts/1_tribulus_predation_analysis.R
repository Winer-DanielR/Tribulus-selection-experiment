##########################################################################################
# Created by Marc-Olivier Beausoleil
# McGill University 
# Created February 19, 2020 

# Why: 
# Arrange the Excel data 
# Requires 
# NOTES: 
##########################################################################################

tribulus_data = read.csv("selection experiment/point in time data/processed/tribulus_combined.csv")
head(tribulus_data)
tribulus_data[which(is.na(tribulus_data$eaten.insects)),"eaten.insects"]<-0

table(tribulus_data$eaten.insects)

table(tribulus_data$eaten)
length(which(!is.na(tribulus_data$eaten.insects)))
length(which(!is.na(tribulus_data$eaten)))


tables.eat= table(tribulus_data$eaten.insects,tribulus_data$eaten)
lgth_insc_eaten =length(which(!is.na(tribulus_data$eaten.insects)))

png("selection experiment/output/Proportion_of_seeds_predation.png", height = 5, width = 5,res = 300, units = "in")
barplot(round(tables.eat/lgth_insc_eaten,3)*100, 
        main = "Barplot birds and insect predation", 
        ylim = c(0,100),
        xlab = "Bird predation",ylab = "Proportion of predation", 
        col = c("black","white"))
dev.off()


dput(names(tribulus_data))

# PCA of Tribulus traits 
pca.trib= prcomp(tribulus_data[,c("Length", "Widht", "Depth")])
biplot(pca.trib)