##########################################################################################
# Created by Marc-Olivier Beausoleil
# McGill University 
# Created February 19, 2020 

# Why: 
  # Arrange the Excel data 
# Requires 
# NOTES: 
##########################################################################################

library(openxlsx)
trib.df3 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx", sheet = 3, startRow = 2)
trib.df4 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx", sheet = 4, startRow = 2)
trib.df5 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx", sheet = 5, startRow = 2)
trib.df6 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx", sheet = 6, startRow = 2)
trib.df7 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx", sheet = 7, startRow = 2)
trib.df8 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx", sheet = 8, startRow = 2)
trib.df9 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx", sheet = 9, startRow = 2)
trib.df10 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 10, startRow = 2)
trib.df11 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 11, startRow = 2)
trib.df12 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 12, startRow = 2)
trib.df13 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 13, startRow = 2)
trib.df14 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 14, startRow = 2)
trib.df15 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 15, startRow = 2)
trib.df16 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 16, startRow = 2)
trib.df17 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 17, startRow = 2)
trib.df18 = read.xlsx("selection experiment/point in time data/raw/majo.xlsx",sheet = 18, startRow = 2)


trib.df3$pop = 2
trib.df4$pop = 5
trib.df5$pop = 4
trib.df6$pop = 1
trib.df7$pop = 0
trib.df8$pop = 4
trib.df9$pop = 1
trib.df10$pop = 5
trib.df11$pop = 3
trib.df12$pop = 2
trib.df13$pop = 6
trib.df14$pop = 3
trib.df15$pop = 1
trib.df16$pop = 4
trib.df17$pop = 5
trib.df18$pop = 7

trib.df3$isl = "Santa Cruz"
trib.df4$isl = "San Cristobal"
trib.df5$isl = "San Cristobal"
trib.df6$isl = "San Cristobal"
trib.df7$isl = "San Cristobal"
trib.df8$isl = "Isabela"
trib.df9$isl = "Isabela"
trib.df10$isl = "Isabela"
trib.df11$isl = "Isabela"
trib.df12$isl = "Isabela"
trib.df13$isl = "Isabela"
trib.df14$isl = "Floreana"
trib.df15$isl = "Floreana"
trib.df16$isl = "Floreana"
trib.df17$isl = "Floreana"
trib.df18$isl = "Floreana"


trib.df3$site = "Garrapatero Alto"
trib.df4$site = "Parque"
trib.df5$site = "Playa de Oro"
trib.df6$site = "USFQ"
trib.df7$site = "Loberia"
trib.df8$site = "Aeropuerto"
trib.df9$site = "Flamingos"
trib.df10$site = ""
trib.df11$site = ""
trib.df12$site = "Bifurcacion"
trib.df13$site = "Balkan"
trib.df14$site = "Basurero"
trib.df15$site = "Playa Negra"
trib.df16$site = "Mina"
trib.df17$site = "Cementerio"
trib.df18$site = "Cementerio entrada"









names(trib.df3)[9]<-"total.seed.predated"
names(trib.df8)[2]<-"Widht"
names(trib.df9)[2]<-"Widht"
names(trib.df10)[2]<-"Widht"
names(trib.df11)[2]<-"Widht"
names(trib.df12)[2]<-"Widht"
names(trib.df13)[2]<-"Widht"
names(trib.df14)[2]<-"Widht"
names(trib.df15)[2]<-"Widht"
names(trib.df16)[2]<-"Widht"
names(trib.df17)[2]<-"Widht"
names(trib.df18)[2]<-"Widht"

trib.df8 = trib.df8[,-10]
trib.df9 = trib.df9[,-10]
trib.df10 = trib.df10[,-10]
trib.df11 = trib.df11[,-10]
trib.df12 = trib.df12[,-10]
trib.df13 = trib.df13[,-10]
trib.df14 = trib.df14[,-10]
trib.df15 = trib.df15[,-10]
trib.df16 = trib.df16[,-10]
trib.df17 = trib.df17[,-10]
trib.df18 = trib.df18[,-10]

cbind(names(trib.df3),names(trib.df8))

names(trib.df3)==names(trib.df8)
data.final= rbind(trib.df3,
                  trib.df4,
                  trib.df5,
                  trib.df6,
                  trib.df7,
                  trib.df8,
                  trib.df9,
                  trib.df10,
                  trib.df11,
                  trib.df12,
                  trib.df13,
                  trib.df14,
                  trib.df15,
                  trib.df16,
                  trib.df17,
                  trib.df18
)

write.csv(data.final,"selection experiment/point in time data/processed/tribulus_combined.csv")
