inference <- function(modelo_funcion,data_train){
  precision <- c()
  for (i in predict(modelo_funcion, type = "response")) {
    precision <- c(precision,
                   sum(data_train$virit_resultado_wb ==
                         factor(ifelse(predict(modelo_funcion,type = "response") > i,1,0),
                                labels = c("negativo","positivo"),levels = c(0,1)))/
                     nrow(data_train))
  }
  n_1input <- sum(
    factor(ifelse(
      predict(modelo_funcion,
              newdata = datos_consolidados[datos_consolidados$virit_wb=="No tiene resultado de WB",],
              type = "response") > predict(modelo_funcion, type = "response")[which.max(precision)],1,0),
      labels = c("negativo","positivo"),levels = c(0,1)) == "positivo")
  n_0input <- nrow(datos_consolidados[datos_consolidados$virit_wb=="No tiene resultado de WB",]) - 
    n_1input
  n_1 <- sum(datos_virsel$virit_resultado_wb=="positivo") 
  n_0 <- sum(datos_virsel$virit_resultado_wb=="negativo")
  N_1 <- n_1 + n_1input
  N_0 <- n_0 + n_0input
  n_1/ (n_1 + n_0)
  N_1/ (N_1 + N_0)
  rho <- (n_1*N_0)/(N_1*n_0)
  p_tilde <- n_1/ (n_1 + n_0*rho)
  lista_inferencia <- list(p = n_1/(n_1 + n_0),
                           rho = rho,
                           p_tilde = p_tilde)
  return(lista_inferencia)
}
