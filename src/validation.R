library(pROC)
validation <- function(modelo_funcion, data_train, data_test){
  precision <- c()
  for (i in predict(modelo_funcion, type = "response")) {
    precision <- c(precision,
                   sum(data_train$virit_resultado_wb ==
                         factor(ifelse(predict(modelo_funcion,type = "response") > i,1,0),
                                labels = c("negativo","positivo"),levels = c(0,1)))/
                     nrow(data_train))
  }
  valores_table_train <- table(t(t(data_train$virit_resultado_wb)),
                               t(t(factor(ifelse(predict(modelo_funcion,
                                                         type = "response") >
                                                   predict(modelo_funcion, type = "response")
                                                 [which.max(precision)],1,0),
                                          labels = c("negativo*","positivo*"),
                                          levels = c(0,1)))))
  valores_table_test <- table(t(t(test$virit_resultado_wb)),
                              t(t(factor(ifelse(predict(modelo_funcion,
                                                        newdata = data_test,
                                                        type = "response") >
                                                  predict(modelo_funcion,
                                                          newdata = data_test,
                                                          type = "response")
                                                [which.max(precision)],1,0),
                                         labels = c("negativo*","positivo*"),
                                         levels = c(0,1)))))
  lista_evaluacion <- list(resumen = summary(modelo_funcion),
                           tabla_train = valores_table_train,
                           especificidad_train = valores_table_train[1,1]/sum(valores_table_train[1,]),
                           sensibilidad_train = valores_table_train[2,2]/sum(valores_table_train[2,]),
                           AUC_train = pROC::roc(data_train$virit_resultado_wb,
                                                 predict(modelo_funcion))$auc,
                           tabla_test = valores_table_test,
                           especificidad_test = valores_table_test[1,1]/sum(valores_table_test[1,]),
                           sensibilidad_test = valores_table_test[2,2]/sum(valores_table_test[2,]),
                           AUC_test = pROC::roc(data_test$virit_resultado_wb,
                                                predict(modelo_funcion, newdata = data_test))$auc,
                           diff = pROC::roc(data_train$virit_resultado_wb,
                                            predict(modelo_funcion))$auc -
                             pROC::roc(data_test$virit_resultado_wb,
                                       predict(modelo_funcion, newdata = data_test))$auc)
  return(lista_evaluacion)
}
