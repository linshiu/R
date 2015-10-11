random.imp <- function (a){
  missing <- is.na(a)           ## T/F matrix
  n.missing <- sum(missing)     ## number of observations with missing values
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}
