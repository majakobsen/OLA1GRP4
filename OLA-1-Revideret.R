library("ggplot2")
library("dplyr")

newhomes <- read.csv("https://gist.githubusercontent.com/ayki1000/03cdebe8d2d50a16ef6c2fcd4802c6fd/raw/a2f8319a3277cfb822af5eb0962b8e486818866d/boligsiden.csv")
View(newhomes)
#Opgave 1

#Opgave 1.1

subset(newhomes, vej == "tousvej" & vejnr =="106")
subset(newhomes, vej == "egevej" & vejnr =="20")

#Opgave 2

#Opgave 2.1

newhomesny <- newhomes %>% filter(complete.cases(.))
newhomesny$pris <- as.numeric(gsub("[^0-9]", "", newhomesny$pris))
newhomesny$størrelse <- as.numeric(gsub("[^0-9]", "", newhomesny$størrelse))
newhomesny$kvmpris <- as.numeric(gsub("[^0-9]", "", newhomesny$kvmpris))
newhomesny$mdudg <- as.numeric(gsub("[^0-9]", "", newhomesny$mdudg))
newhomesny$grund <- as.numeric(gsub("[^0-9]", "", newhomesny$grund))
newhomesny$liggetid <- as.numeric(gsub("[^0-9]", "", newhomesny$liggetid))

newhomesny$vej <- as.factor(newhomesny$vej)
newhomesny$vejnr <- as.factor(newhomesny$vejnr)
newhomesny$postnr <- as.factor(newhomesny$postnr)
newhomesny$by <- as.factor(newhomesny$by)

View(newhomesny)

summary(newhomesny)
View(newhomesny)

#Opgave 2.2

cor(newhomesny$pris, newhomesny$størrelse)

#Opgave 2.3 

newhomesny$alder <- 2026 - newhomesny$opført

model1 <- lm(kvmpris ~ liggetid, data = newhomesny)
model2 <- lm(kvmpris ~ størrelse, data = newhomesny)
model3 <- lm(kvmpris ~ grund, data = newhomesny)
model4 <- lm(kvmpris ~ værelser, data = newhomesny)
model5 <- lm(kvmpris ~ alder, data = newhomesny)



kor_data <- newhomesny[, c("liggetid",
                           "pris",
                           "størrelse",
                           "grund",
                           "alder",
                           "værelser")]
cor(kor_data)

#Opgave 3

#Opgave 3.1

#Funtkion som kaster 25.000 terninger og beregner andelen som bliver fem.

Femere <- function() {
  Terninger25000 <- sample(1:6, 25000, replace = TRUE)
  Andel <- sum(Terninger25000==5) / 25000
  Andel
}

#Opgave 3.2

#Definer en funktion som kaster med seks terninger.

SeksTerninger <- function () {
  die <- 1:6
  dice <- sample(die, size = 6, replace = TRUE)
  sum(dice)
}

#Definer en funktion som kaster de 6 terninger 10000 gange, og giver et output i barplot

TiTusind <- replicate(10000, SeksTerninger())

BarPlot1 <- function() {
  qplot(TiTusind, geom = "histogram", 
        binwidth = 1,
        main = "Sum af seks terninger", 
        xlab = "Sum", 
        ylab = "Antal")
}

#Opgave 3.3

#Kopi af 3.2, dog med 1 mio, replikationer

BarPlot2 <- function() {
  EnMillion <- replicate(1000000, SeksTerninger())
  qplot(EnMillion, geom = "histogram", 
        binwidth = 1,
        main = "Sum af seks terninger", 
        xlab = "Sum", 
        ylab = "Antal")
}

#Opgave 3.4 
FastRække <- 2:6
TilfældigRække <- sample(c(1, 2, 3, 5, 6))
matrix_data <- cbind(FastRække, TilfældigRække)


Matrix1 <- function() {
  FastRække <- 2:6
  TilfældigRække <- sample(c(1, 2, 3, 5, 6))
  matrix_data <- cbind(FastRække, TilfældigRække)
  matrix_data
}

#Overstående er opdelt, understående er en funktion så man får et nyt hver gang.


#Opgave 4

#4.1
#vi laver en karakter sample med størrelse svarende til vores dataframe
#her er replace sat til true og vi antager lige fordeling af karakter.
k<-sample(c(-3,0,2,4,7,10,12), size = 9, replace = TRUE,
          prob = c(1/14, 1/14, 1/14, 1/14, 1/14, 1/14,8/14))
k1<-sample(c(-3,0,2,4,7,10,12), size = 9, replace = TRUE,
           prob = c(1/14, 1/14, 2/14, 4/14, 3/14, 2/14,1/14))
k2<-sample(c(-3,0,2,4,7,10,12), size = 9, replace = TRUE,
           prob = c(1/14, 1/14, 1/14, 4/14, 2/14, 1/14,4/14))
k3<-sample(c(-3,0,2,4,7,10,12), size = 9, replace = TRUE,
           prob = c(1/14, 1/14, 8/14, 1/14, 1/14, 1/14,1/14))

dfklasse <- data.frame(klasse = rep(c("a","b","c","d"), each =9),
                       uge= rep(c(seq(1,9,by=1))),
                       score = c(k,k1,k2,k3)
)
View(dfklasse)
summary(dfklasse)

#tom dataframe
nydf <- data.frame()
#loop tid! vi sender 12 rækker ind i vores tomme datasæt en for hver 3. uge
for (i in 1:12) {
  rows <- dfklasse[((i-1)*3+1:3),3] #finder der relevante rækker til til gennemsnit
  nydf<- rbind(nydf, data.frame(
    klasse = dfklasse[3*i,1],uge= dfklasse[3*i,2], # tager den rigtige klasse og uge
    snitscore = sum(rows)/3)) # laver gennemsnit
}

View(nydf)

snitdf <- data.frame()
#loop tid! vi sender 12 rækker ind i vores tomme datasæt en for hver 3. uge
for (i in 1:9) {
  rows <- dfklasse[((i-1)*4+1:4),3] #finder der relevante rækker til til gennemsnit
  snitdf<- rbind(snitdf, data.frame(
    klasse = dfklasse[4*i,1],uge= dfklasse[4*i,2], # tager den rigtige klasse og uge
    snitscore = sum(rows)/4)) # laver gennemsnit
}

View(nydf)

install.packages("tidyr")
library(tidyr)

nydfbred <- pivot_wider(nydf,names_from=klasse,values_from=snitscore)
dim(nydfbred)
