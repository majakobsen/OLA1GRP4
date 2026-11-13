#Opgave 3

#Opgave 3.1

#Funtkion som kaster 25.000 terninger og beregner andelen som bliver fem.

Femere <- function() {
  Terninger25000 <- sample(1:6, 25000, replace = TRUE)
  Andel <- sum(Terninger25000==5) / 25000
  Andel
}

