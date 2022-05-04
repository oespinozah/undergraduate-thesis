library(pROC)
evaluate <- function(modelo_funcion){
  precision <- c()
  for (i in predict(modelo_funcion, type = "response")) {
    precision <- c(precision,
                   sum(datos_virsel$virit_resultado_wb ==
                         factor(ifelse(predict(modelo_funcion,type = "response") > i,1,0),
                                labels = c("negativo","positivo"),levels = c(0,1)))/
                     nrow(datos_virsel))
  }
  valores_table <- table(t(t(datos_virsel$virit_resultado_wb)),
                         t(t(factor(ifelse(predict(modelo_funcion,
                                                   type = "response") >
                                             predict(modelo_funcion, type = "response")
                                           [which.max(precision)],1,0),
                                    labels = c("negativo*","positivo*"),
                                    levels = c(0,1)))))
  lista_evaluacion <- list(resumen = summary(modelo_funcion),
                           tabla = valores_table,
                           especificidad = valores_table[1,1]/sum(valores_table[1,]),
                           sensibilidad = valores_table[2,2]/sum(valores_table[2,]),
                           AUC = pROC::roc(datos_virsel$virit_resultado_wb,
                                           predict(modelo_funcion))$auc)
  return(lista_evaluacion)
}
