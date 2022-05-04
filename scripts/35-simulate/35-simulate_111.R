git_path <- function() {
  system('git rev-parse --show-toplevel', intern = TRUE)
}
proj_path <- git_path()
source(file.path(proj_path,"src","split_quadrant.R"))

library(sf)
library(spatstat)
library(fields)
library(mgcv)
library(psych)
library(splines)
library(caret)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

h_lambda <- 5
k_lambda <- 5
a_lambda <- 1/4
b_lambda <- 12
c_lambda <- 6.95
h_theta <- 7.5
k_theta <- 7.5
a_theta <- -1/16
b_theta <- -1/8
c_theta <- 0.45
h_pi <- 2.5
k_pi <- 7.5
a_pi <- -1/8
b_pi <- -1/8
c_pi <- 2.35

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

lambda_fun <- function(x,y){
  return(c_lambda-c_lambda*(a_lambda*(x-h_lambda)^2+b_lambda*(y-k_lambda)^2)^0.5/
           ((a_lambda*(h_lambda)^2+b_lambda*(k_lambda)^2)^0.5))}
theta_fun <- function(x,y){
  return(1/(1+exp(-(c_theta+a_theta* (x-h_theta)^2+b_theta*(y-k_theta)^2))))}
pi_fun <- function(x,y){
  return(1/(1+exp(-(c_pi+a_pi * (x-h_pi)^2+b_pi*(y-k_pi)^2))))}

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

lambda_theta_fun <- function(x,y){
  return( lambda_fun(x,y) * theta_fun(x,y))}
lambda_pi_fun <- function(x,y){
  return( lambda_fun(x,y) * pi_fun(x,y))}
lambda_theta_pi_fun <- function(x,y){
  return( lambda_fun(x,y) * theta_fun(x,y) * pi_fun(x,y))}

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

set.seed(2020)
win <- owin(xrange = c(0, 10), yrange = c(0, 10))
spp <- rpoispp(lambda = lambda_fun, win = win)
props <- theta_fun(spp$x,spp$y)
marks_spp <- factor(sapply(props, function(x) rbinom(1, 1, x)),
                    labels = c("negativo","positivo"),levels = c(0,1))
props <- pi_fun(spp$x,spp$y)
props_sample <- sapply(props, function(x) rbinom(1, 1, x)) == 1
spp_sample <- spp[props_sample]
spp$n
spp_sample$n/spp$n

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

m <- 510
resultados <- matrix(nrow = m,ncol = 18)
set.seed(2020)
for (i in 1:m) {
  win <- owin(xrange = c(0, 10), yrange = c(0, 10))
  spp <- rpoispp(lambda = lambda_fun, win = win)
  props <- theta_fun(spp$x,spp$y)
  marks_spp <- factor(sapply(props, function(x) rbinom(1, 1, x)),
                      labels = c("negativo","positivo"),levels = c(0,1))
  props <- pi_fun(spp$x,spp$y)
  props_sample <- sapply(props, function(x) rbinom(1, 1, x)) == 1
  spp_sample <- spp[props_sample]
  sq <- sample(1:25,10,replace=F)
  train_spp <- split_quadrant(spp,sq)
  qua <- quadrats(spp, 5, 5)
  k_base <- c()
  ecm_nB_train <- c()
  ecm_nB_test <- c()
  for (j in 2:50) {
    train_fit <- ppm(train_spp~ s(x,y,k=j), use.gam=TRUE)
    res2e <- residuals(train_fit)
    resQ <- integral(res2e, qua)
    k_base <- cbind(k_base,j)
    ecm_nB_train <- cbind(ecm_nB_train,mean(resQ[-sq]^2))
    ecm_nB_test <- cbind(ecm_nB_test,
                         mean((quadratcount(spp,
                                            nx=5, ny=5)[sq] + resQ[sq])^2))
  }
  k_base <- k_base[which.min(ecm_nB_test)]
  lambda_p_fit <- ppm(train_spp~s(x,y,k= k_base),
                      use.gam=TRUE)
  loglambda_p <- log(predict(lambda_p_fit,
                             locations=data.frame(x=spp_sample$x,
                                                  y=spp_sample$y)))
  sq <- sample(1:25,10,replace=F)
  train_spp <- split_quadrant(spp_sample,sq)
  qua <- quadrats(spp_sample, 5, 5)
  k_base <- c()
  ecm_nB_train <- c()
  ecm_nB_test <- c()
  for (j in 2:50) {
    train_fit <- ppm(train_spp~ s(x,y,k=j),
                     offset=loglambda_p,
                     use.gam=TRUE)
    res2e <- residuals(train_fit)
    resQ <- integral(res2e, qua)
    k_base <- cbind(k_base,j)
    ecm_nB_train <- cbind(ecm_nB_train,mean(resQ[-sq]^2))
    ecm_nB_test <- cbind(ecm_nB_test,
                         mean((quadratcount(train_spp,
                                            nx=5, ny=5)[sq] + resQ[sq])^2))
  }
  k_base <- k_base[which.min(ecm_nB_test)]
  lambda_fit <- ppm(train_spp~s(x,y,k= k_base),use.gam=TRUE)
  loglambda <- log(predict(lambda_fit,
                           locations=data.frame(x=spp_sample$x,
                                                y=spp_sample$y)))
  w <- exp(loglambda_p-loglambda)
  marks(spp) <- marks_spp
  spp_sample <- spp[props_sample]
  resultados[i,1] <- spp$n
  resultados[i,2] <- spp_sample$n
  inTrain <- createDataPartition(y = spp_sample$marks,p=0.7,list = FALSE)
  train <- spp_sample[inTrain,]
  test <- spp_sample[-inTrain,]
  w_train <- w[inTrain]
 #model <-  try(mgcv::gam(marks~s(x,y),family = binomial(link="logit"), data = train))
 #if(inherits(model, "try-error")) { resultados[i,c(3:8)] <- NA}
 #else{
 #  resultados[i,c(3:8)] <- cbind(as.numeric(pROC::roc(train$marks,predict(model))$auc),
 #                                as.numeric(pROC::coords(pROC::roc(train$marks,predict(model)),
 #                                                        "best", best.method="youden")$specificity[1]),
 #                                as.numeric(pROC::coords(pROC::roc(train$marks,predict(model)),
 #                                                        "best", best.method="youden")$sensitivity[1]),
 #                                as.numeric(pROC::roc(test$marks,predict(model, newdata = test))$auc),
 #                                as.numeric(pROC::coords(pROC::roc(test$marks,predict(model, newdata = test)),
 #                                                        "best", best.method="youden")$specificity[1]),
 #                                as.numeric(pROC::coords(pROC::roc(test$marks,predict(model, newdata = test)),
 #                                                        "best", best.method="youden")$sensitivity[1]))
 #  }
  model_w <- try(mgcv::gam(marks~s(x,y,k=8),family = binomial(link="logit"), data = train, weights =  w_train))
  if(inherits(model_w, "try-error")) { resultados[i,c(9:14)] <- NA}
  else{
    resultados[i,c(9:14)] <- cbind(as.numeric(pROC::roc(train$marks,predict(model_w))$auc),
                                   as.numeric(pROC::coords(pROC::roc(train$marks,predict(model_w)),
                                                           "best", best.method="youden")$specificity[1]),
                                   as.numeric(pROC::coords(pROC::roc(train$marks,predict(model_w)),
                                                           "best", best.method="youden")$sensitivity[1]),
                                   as.numeric(pROC::roc(test$marks,predict(model_w, newdata = test))$auc),
                                   as.numeric(pROC::coords(pROC::roc(test$marks,predict(model_w, newdata = test)),
                                                           "best", best.method="youden")$specificity[1]),
                                   as.numeric(pROC::coords(pROC::roc(test$marks,predict(model_w, newdata = test)),
                                                           "best", best.method="youden")$sensitivity[1]))
    #best.coords <- pROC::coords(pROC::roc(train$marks,predict(model, newdata = train)), "best", best.method="youden")
    #resultados[i,17] <- (sum(predict(model, newdata = spp[!props_sample]) >= best.coords$threshold) +
    #                       sum(spp_sample$marks == "positivo"))/spp$n 
    best.coords_w <- pROC::coords(pROC::roc(train$marks,predict(model_w, newdata = train)),"best", best.method="youden")
    resultados[i,18] <- (sum(predict(model_w, newdata = spp[!props_sample]) >= best.coords_w$threshold) +
                           sum(spp_sample$marks == "positivo"))/spp$n
  }
  resultados[i,15] <- sum(spp$marks == "positivo")/spp$n
  resultados[i,16] <- sum(spp_sample$marks == "positivo")/spp_sample$n
  print(paste(round(i*100/m,2),"%",sep=""))
}
resultados <- as.data.frame(resultados)
names(resultados) <- c("n","n_sample",
                       "train_auc","train_spec","train_sens",
                       "test_auc","test_spec","test_sens",
                       "train_w_auc","train_w_spec","train_w_sens",
                       "test_w_auc","test_w_spec","test_w_sens",
                       "p","p_sample","p_fix","p_w")
simulacion_19 <- list(caracteristicas = matrix(c("poblacion homogenea", "seleccion homogenea",
                                                 "riesgo constante"), ncol = 1),
                      resultados = resultados)
#saveRDS(simulacion_19,file.path(proj_path,"scripts","35-simulate", "simulacion_19.rds"))


#Limpieza de valores nulos
simulacion_10$resultados <- simulacion_10$resultados[!is.na(simulacion_10$resultados$p_w),]
simulacion_11$resultados <- simulacion_11$resultados[!is.na(simulacion_11$resultados$p_w),]
simulacion_12$resultados <- simulacion_12$resultados[!is.na(simulacion_12$resultados$p_w),]
simulacion_13$resultados <- simulacion_13$resultados[!is.na(simulacion_13$resultados$p_w),]
simulacion_14$resultados <- simulacion_14$resultados[!is.na(simulacion_14$resultados$p_w),]
simulacion_15$resultados <- simulacion_15$resultados[!is.na(simulacion_15$resultados$p_w),]
simulacion_16$resultados <- simulacion_16$resultados[!is.na(simulacion_16$resultados$p_w),]
simulacion_17$resultados <- simulacion_17$resultados[!is.na(simulacion_17$resultados$p_w),]
simulacion_18$resultados <- simulacion_18$resultados[!is.na(simulacion_18$resultados$p_w),]
simulacion_19$resultados <- simulacion_19$resultados[!is.na(simulacion_19$resultados$p_w),]
#simulacion_19$resultados <- simulacion_19$resultados[!is.na(simulacion_19$resultados$train_w_auc),]
