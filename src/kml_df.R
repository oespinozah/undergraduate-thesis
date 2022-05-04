kml_df <- function(household){
  household <- as.data.frame(cbind(t(t(household[[1]])),
                                   sf::st_coordinates(household)))
  names(household) <- c("ncasa","longitud","latitud")
  return(household)
}