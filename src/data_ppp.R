library(spatstat)
data_ppp <- function(data,win){
  data_ppp <- ppp(x = data$longitud,
                            y = data$latitud,
                            win)
  return(data_ppp)
}