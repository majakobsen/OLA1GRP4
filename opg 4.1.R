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

