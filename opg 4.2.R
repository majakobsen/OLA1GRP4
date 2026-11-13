#Opg 4.2

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

