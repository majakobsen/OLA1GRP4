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

