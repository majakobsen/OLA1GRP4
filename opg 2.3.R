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

