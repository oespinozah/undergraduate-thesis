#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Librerias a utilizar en la simulación
library(spatstat)
library(fields)
library(mgcv)
library(psych)
library(pracma)
library(splines)
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Parametros para las simulaciones
lambda_parametros <- c(5,5,1/4,10,4,0) #(h,k,a,b,c,const)
theta_parametros <- c(7.5,7.5,-1/16,-1/8,0,0) #(h,k,a,b,c,const)
pi_parametros <- c(2.5,7.5,-1/4,-1/16,2,0,0,0) #(h,k,a,b,c,const,caso,control)
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Creamos las funciones
#Intensidad poblacional
lambda_fun <- function(x,y){
  if (lambda_parametros[6] == 0) {
    return(lambda_parametros[5]-lambda_parametros[5]*
             (lambda_parametros[3]*(x-lambda_parametros[1])^2+
                lambda_parametros[4]*(y-lambda_parametros[2])^2)^0.5/
             ((lambda_parametros[3]*(lambda_parametros[1])^2+
                 lambda_parametros[4]*(lambda_parametros[2])^2)^0.5))
    } else {
    return(lambda_parametros[6])
  }
}
#Riesgo
theta_fun <- function(x,y){
  if (theta_parametros[6] == 0) {
    return(1/(1+exp(-(theta_parametros[5]+
                        theta_parametros[3]*(x-theta_parametros[1])^2+
                        theta_parametros[4]*(y-theta_parametros[2])^2))))
    } else {
    return(rep,(theta_parametros[6]),length(x))
  }
}
#Muestreo
pi_fun <- function(x,y){
  if (pi_parametros[6] == 0) {
    return(1/(1+exp(-(pi_parametros[5]+
                        pi_parametros[3]*(x-pi_parametros[1])^2+
                        pi_parametros[4]*(y-pi_parametros[2])^2))))
    } else {
    return(rep,(pi_parametros[6]),length(x))
    }
}
#Intensidad de positivos poblacional
lambda_theta_fun <- function(x,y){
  return( lambda_fun(x,y) * theta_fun(x,y))}
#Intensidad de muestral
lambda_pi_fun <- function(x,y){
  return( lambda_fun(x,y) * pi_fun(x,y))}
#Intensidad de positivos muestral
lambda_theta_pi_fun <- function(x,y){
  return( lambda_fun(x,y) * theta_fun(x,y) * pi_fun(x,y))}
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Funciones para graficar
grid <- expand.grid(seq(0, 10, length.out = 100),
                    seq(0, 10, length.out = 100))
imagen_plot <- function(z) {
  fields::image.plot(seq(0, 10, length.out = 100),
                     seq(0, 10, length.out = 100),
                     matrix(z, 100, 100),
                     xlab = "x", ylab = "y", main = "")
}
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Empezamos a Simular
set.seed(123) #Semilla de aleatoriedad
win <- owin(xrange = c(0, 10), yrange = c(0, 10))
spp <- rpoispp(lambda = lambda_fun, win = win)
props <- theta_fun(spp$x,spp$y)
marks(spp) <- factor(sapply(props, function(x) rbinom(1, 1, x)),
                     labels = c("negativo","positivo"),levels = c(0,1))
if (pi_parametros[7]==0 & pi_parametros[8]==0) {
  props <- pi_fun(spp$x,spp$y)
  spp_sample <- spp[sapply(props, function(x) rbinom(1, 1, x)) == 1]
} else {
  props <- c(positivo = pi_parametros[7],
             negativo = pi_parametros[8])[spp$marks]
}
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Proporcion poblacional y muestral
N_1 <- sum(spp$marks=="positivo") 
N_0 <- sum(spp$marks=="negativo")
n_1 <- sum(spp_sample$marks=="positivo") 
n_0 <- sum(spp_sample$marks=="negativo")
N_1/ (N_1 + N_0)
n_1/ (n_1 + n_0)
rho <- (n_1*N_0)/(N_1*n_0)
n_1/ (n_1 + n_0*rho)
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Inferencia sobre positivo y negativo en muestra
spp_sample_positvo <- spp_sample[spp_sample$marks=="positivo"]
spp_sample_negativo <- spp_sample[spp_sample$marks=="negativo"]
marks(spp_sample_positvo) <- NULL
marks(spp_sample_negativo) <- NULL
spp_lambda_fit_positvo <- ppm(spp_sample_positvo ~ s(x,y,k=6), use.gam=TRUE)
spp_lambda_fit_negativo <- ppm(spp_sample_negativo ~ s(x,y,k=6), use.gam=TRUE)
spp_lambda_fit_predict_positvo <- predict(spp_lambda_fit_positvo)
spp_lambda_fit_predict_negativo <- predict(spp_lambda_fit_negativo)
risk_estimated <- spp_lambda_fit_predict_positvo/
  (spp_lambda_fit_predict_positvo+rho*spp_lambda_fit_predict_negativo)
z_1 <-predict(spp_lambda_fit_positvo,
              locations=data.frame(x=grid[,1], y=grid[,2]))
z_0 <-predict(spp_lambda_fit_negativo,
              locations=data.frame(x=grid[,1], y=grid[,2]))
z_1/(z_1+rho*z_0)
w <- c(negativo = rho, positivo = 1)[spp_sample$marks]
theta_fit_gam <- mgcv::gam(marks~s(x,y,k=20),family = binomial(link="logit"),
                           data=spp_sample, weights = w,
                           gamma = 1)
summary(theta_fit_gam)
z <- psych::logistic(predict.gam(theta_fit_gam,
                                 newdata=data.frame(x=grid[,1], y=grid[,2])))
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#Intensidad poblacional
temporal <- marks(spp)
marks(spp) <- NULL
spp_lambda_fit <- ppm(spp ~ s(x,y,k=6), use.gam=TRUE)
spp_lambda_predict <- predict(spp_lambda_fit)
marks(spp) <- temporal
#Intensidad muestral
temporal <- marks(spp_sample)
marks(spp_sample) <- NULL
spp_sample_lambda_fit <- ppm(spp_sample ~ s(x,y,k=6), use.gam=TRUE)
spp_sample$loglambda_p <- log(predict(spp_lambda_fit,
                                      locations=data.frame(x=spp_sample$x,
                                                           y=spp_sample$y)))

Z_fun <- function(x,y){
  return( sqrt((x - 2.5)^2 + (y - 7.5)^2) )}


spp_sample_lambda_fit <- ppm(spp_sample ~ s(x,y,k=6) + Z_fun,
                             offset=loglambda_p,
                             use.gam=TRUE)
#pp_sample_lambda_predict <- predict(spp_sample_lambda_fit)
marks(spp_sample) <- temporal
summary(spp_sample_lambda_fit)

str(spp_sample)
#Predict
loglambda_p <- log(predict(spp_lambda_fit,
                       locations=data.frame(x=spp_sample$x,
                                            y=spp_sample$y)))
loglambda <- log(predict(spp_sample_lambda_fit,
                     locations=data.frame(x=spp_sample$x,
                                          y=spp_sample$y)))
#
Z <-sqrt((spp_sample$x - 2.5)^2 + (spp_sample$y - 7.5)^2)
beta_0 <- log(spp_sample$n/spp$n)
data_Z <- as.data.frame(cbind(loglambda,loglambda_p,beta_0,Z))
#Sin intercepto en el BETA
modelo_base <- lm(loglambda~Z-1,
                  data_Z,
                  offset = loglambda_p + beta_0)
summary(modelo_base)
#Con intercepto
modelo_base <- lm(loglambda~Z,
                  data_Z,
                  offset = loglambda_p + beta_0)
summary(modelo_base)
#Sin beta_0 como offset
modelo_base <- lm(loglambda~Z,
                  data_Z,
                  offset = loglambda_p)
summary(modelo_base)
