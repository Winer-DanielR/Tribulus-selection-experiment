# This script is to arrange and create the figure for question 4
# The figure would be a bar plot with the proportios of eaten mericarps
# per treatments.
# 
# First I will upload and merge the datasets
# 
# Data loading ####

Floreana_2018 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2018.csv")
Floreana_2019 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2019.csv")

str(Floreana_2018)
str(Floreana_2019)

Floreana_MR <- bind_rows(Floreana_2018, Floreana_2019)

# Data preparation ####
# Select columns for analysis
Floreana_MR <- select(Floreana_MR, c(2:10, 18:21))

