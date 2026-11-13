#Opg 4.3

install.packages("tidyr")
library(tidyr)

nydfbred <- pivot_wider(nydf,names_from=klasse,values_from=snitscore)
dim(nydfbred)
