library(spatstat)
win_ppp <- function(datos,household_df){
  ppc_pueblo <- as.data.frame(cbind(t(t(names(table(datos$ncasa)))),
                                    t(t(table(datos$ncasa)))))
  names(ppc_pueblo) <- c("ncasa","cantidad")
  ppc_pueblo <- merge(ppc_pueblo,household_df, by="ncasa", all.x=TRUE)
  ppc_pueblo$ncasa <- as.numeric(ppc_pueblo$ncasa)
  ppc_pueblo$cantidad <- as.numeric(ppc_pueblo$cantidad)
  ppc_pueblo$longitud <- as.numeric(ppc_pueblo$longitud)
  ppc_pueblo$latitud <- as.numeric(ppc_pueblo$latitud)
  win <- sf::st_polygon(list(as.matrix(ppc_pueblo[
    c(chull(ppc_pueblo$longitud,ppc_pueblo$latitud),
      chull(ppc_pueblo$longitud,ppc_pueblo$latitud)[1]), c(3,4)])))
  win <- as.owin(win)
  return(win)
}