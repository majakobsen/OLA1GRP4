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

#Opgave 2

#Opgave 2.1
newhomesny <- newhomes
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

library(ggplot2)

# Simulér summer af seks terninger

TiTusind <- replicate(10000, SeksTerninger())
EnMillion <- replicate(1000000, SeksTerninger())

# Fælles design gør figurerne nemme at sammenligne
lav_terningeplot <- function(resultater, undertitel, farve) {
  ggplot(data.frame(Sum = resultater), aes(x = Sum)) +
    geom_histogram(
      aes(y = after_stat(count / sum(count))),
      binwidth = 1,
      boundary = 0.5,
      fill = farve,
      colour = "white",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = 21,
      colour = "#253447",
      linetype = "dashed",
      linewidth = 0.7
    ) +
    annotate(
      "text",
      x = 21.5,
      y = 0.111,
      label = "Forventet sum: 21",
      hjust = 0,
      colour = "#253447",
      size = 3.8
    ) +
    scale_x_continuous(
      breaks = seq(6, 36, by = 3),
      limits = c(5.5, 36.5),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 0.12, by = 0.02),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(ylim = c(0, 0.12)) +
    labs(
      title = "Summer omkring 21 forekommer oftest",
      subtitle = undertitel,
      x = "Summen af seks terninger",
      y = "Andel af forsøg (%)",
      caption = "Kilde: egen simulation i R · Seks fair terninger pr. forsøg"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 19,
        colour = "#253447",
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 12,
        colour = "#586779",
        margin = margin(b = 18)
      ),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        colour = "#E4E9EF",
        linewidth = 0.4
      ),
      axis.title = element_text(colour = "#253447"),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.text = element_text(colour = "#586779"),
      plot.caption = element_text(
        hjust = 0,
        size = 9,
        colour = "#687789",
        margin = margin(t = 16)
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = margin(22, 25, 18, 18)
    )
}

# Opgave 3.2
BarPlot1 <- function() {
  lav_terningeplot(
    TiTusind,
    undertitel = "10.000 gentagelser",
    farve = "#4386B5"
  )
}

# Opgave 3.3
BarPlot2 <- function() {
  lav_terningeplot(
    EnMillion,
    undertitel = "1.000.000 gentagelser",
    farve = "#258C85"
  )
}

# Vis figurerne
BarPlot1()
BarPlot2()

# Gem begge figurer
r_mappe <- "C:/Users/Lasse/Desktop/R/OLA-1"

ggsave(
  filename = file.path(r_mappe, "BarPlot1.png"),
  plot = BarPlot1(),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(r_mappe, "BarPlot2.png"),
  plot = BarPlot2(),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

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
