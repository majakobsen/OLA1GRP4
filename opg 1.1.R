library("ggplot2")
library("dplyr")

newhomes <- read.csv("https://gist.githubusercontent.com/ayki1000/03cdebe8d2d50a16ef6c2fcd4802c6fd/raw/a2f8319a3277cfb822af5eb0962b8e486818866d/boligsiden.csv")
View(newhomes)

#Vester Altan er opført i 1963 efter kontrol på boligsiden (OUTLIER KONTROlL

newhomes[1817,12] <- 1963
newhomes[1817,12]

#Opgave 1

#Opgave 1.1

subset(newhomes, vej == "tousvej" & vejnr =="106")
subset(newhomes, vej == "egevej" & vejnr =="20")

