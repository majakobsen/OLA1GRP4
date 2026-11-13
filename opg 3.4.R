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


