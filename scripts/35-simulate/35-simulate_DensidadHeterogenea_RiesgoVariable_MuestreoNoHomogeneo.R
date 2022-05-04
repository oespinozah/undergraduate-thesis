library(spatstat)
library(fields)
library(mgcv)
library(psych)
library(pracma)
library(splines)
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=# Densidad poblacional heterogénea, riesgo espacial variable, muestreo aleatorio no homogeneo
h_lambda <- 5
k_lambda <- 5
a_lambda <- 1/4
b_lambda <- 10
c_lambda <- 4
h_theta <- 7.5
k_theta <- 7.5
a_theta <- -1/16
b_theta <- -1/8
c_theta <- 0
h_pi <- 2.5
k_pi <- 7.5
a_pi <- -1/4
b_pi <- -1/16
c_pi <- 2

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

grid <- expand.grid(seq(0, 10, length.out = 100),
                    seq(0, 10, length.out = 100))

imagen_plot <- function(z) {
  fields::image.plot(seq(0, 10, length.out = 100),
                     seq(0, 10, length.out = 100),
                     matrix(z, 100, 100),
                     xlab = "x", ylab = "y", main = "")
}

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

x11()
par(mfrow = c(2, 2))
z <- lambda_fun(grid[,1],grid[,2])
imagen_plot(z)
z <- lambda_theta_fun(grid[,1],grid[,2])
imagen_plot(z)
z <- lambda_pi_fun(grid[,1],grid[,2])
imagen_plot(z)
z <- lambda_theta_pi_fun(grid[,1],grid[,2])
imagen_plot(z)
x11()
z <- lambda_fun(grid[,1],grid[,2])
imagen_plot(z)
x11()
z <- theta_fun(grid[,1],grid[,2])
imagen_plot(z)
x11()
z <- pi_fun(grid[,1],grid[,2])
imagen_plot(z)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

set.seed(123)
win <- owin(xrange = c(0, 10), yrange = c(0, 10))
spp <- rpoispp(lambda = lambda_fun, win = win)
props <- theta_fun(spp$x,spp$y)
marks(spp) <- factor(sapply(props, function(x) rbinom(1, 1, x)),
                     labels = c("negativo","positivo"),levels = c(0,1))
props <- pi_fun(spp$x,spp$y)
spp_sample <- spp[sapply(props, function(x) rbinom(1, 1, x)) == 1]

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

x11()
par(mfrow = c(1, 2))
plot(spp, cols=c(1:2), pch = 19, cex=0.5, main="")
plot(spp_sample, cols=c(1:2), pch = 19, cex=0.5, main="")
x11()
par(mfrow = c(2, 2))
plot(density(spp), main = "")
plot(density(spp[spp$marks=="positivo"]), main = "")
plot(density(spp_sample), main = "")
plot(density(spp_sample[spp_sample$marks=="positivo"]), main = "")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#Número estimado de puntos
pracma::integral2(lambda_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q
#Número estimado de puntos marcados
pracma::integral2(lambda_theta_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q
#Número estimado de puntos en la muestra
pracma::integral2(lambda_pi_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q
#Número estimado de puntos marcados en la muestra
pracma::integral2(lambda_theta_pi_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

n_pos <- pracma::integral2(lambda_theta_pi_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q
n_neg <- pracma::integral2(lambda_pi_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q -
         pracma::integral2(lambda_theta_pi_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q
N_pos <- pracma::integral2(lambda_theta_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q
N_neg <- pracma::integral2(lambda_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q-
         pracma::integral2(lambda_theta_fun,xmin = 0,xmax=10,ymin = 0,ymax=10)$Q
N_pos/ (N_pos + N_neg)
n_pos/ (n_pos + n_neg)
rho <- (n_pos*N_neg)/(N_pos*n_neg)
n_pos/ (n_pos + n_neg*rho)
rm(n_pos, n_neg, N_pos, N_neg)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

N_1 <- sum(spp$marks=="positivo") 
N_0 <- sum(spp$marks=="negativo")
n_1 <- sum(spp_sample$marks=="positivo") 
n_0 <- sum(spp_sample$marks=="negativo")
N_1/ (N_1 + N_0)
n_1/ (n_1 + n_0)
rho <- (n_1*N_0)/(N_1*n_0)
alfa <- (n_1/n_0)*(1+N_0/N_1)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

temporal <- marks(spp)
marks(spp) <- NULL
spp_lambda_fit <- ppm(spp ~ s(x,y,k=6), use.gam=TRUE)
summary(spp_lambda_fit)
spp_lambda_fit_predict <- predict(spp_lambda_fit)
x11()
plot(spp_lambda_fit_predict,xlab = "x", ylab = "y", zlab="", main = "")
marks(spp) <- temporal
rm(temporal)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

spp_positvo <- spp[spp$marks=="positivo"]
spp_negativo <- spp[spp$marks=="negativo"]
marks(spp_positvo) <- NULL
marks(spp_negativo) <- NULL
spp_lambda_fit_positvo <- ppm(spp_positvo ~ s(x,y,k=6), use.gam=TRUE)
spp_lambda_fit_negativo <- ppm(spp_negativo ~ s(x,y,k=6), use.gam=TRUE)
spp_lambda_fit_predict_positvo <- predict(spp_lambda_fit_positvo)
spp_lambda_fit_predict_negativo <- predict(spp_lambda_fit_negativo)
risk_estimated <- spp_lambda_fit_predict_positvo/(spp_lambda_fit_predict_positvo +
                                                    spp_lambda_fit_predict_negativo)
plot(spp_lambda_fit_predict_positvo)
plot(spp_lambda_fit_predict_negativo)
plot(risk_estimated)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

spp_sample_positvo <- spp_sample[spp_sample$marks=="positivo"]
spp_sample_negativo <- spp_sample[spp_sample$marks=="negativo"]
marks(spp_sample_positvo) <- NULL
marks(spp_sample_negativo) <- NULL
spp_lambda_fit_positvo <- ppm(spp_sample_positvo ~ s(x,y,k=6), use.gam=TRUE)
spp_lambda_fit_negativo <- ppm(spp_sample_negativo ~ s(x,y,k=6), use.gam=TRUE)
spp_lambda_fit_predict_positvo <- predict(spp_lambda_fit_positvo)
spp_lambda_fit_predict_negativo <- predict(spp_lambda_fit_negativo)
risk_estimated <- spp_lambda_fit_predict_positvo/(spp_lambda_fit_predict_positvo+
                                                    rho*spp_lambda_fit_predict_negativo)
x11()
plot(risk_estimated)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

z_1 <-predict(spp_lambda_fit_positvo, locations=data.frame(x=grid[,1], y=grid[,2]))
z_0 <-predict(spp_lambda_fit_negativo, locations=data.frame(x=grid[,1], y=grid[,2]))
imagen_plot(z_1)
imagen_plot(z_0)
imagen_plot(z_1/(z_1+z_0))





#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#eEsto aún no correr
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#spp_sample$marks <- factor(spp_sample$marks,levels(spp$marks)[c(2,1)])
theta_fit_gam <- mgcv::gam(marks~s(x,y,k=6), family = binomial(link="logit"), data=spp_sample)
summary(theta_fit_gam)
gam.check(theta_fit_gam)
plot(theta_fit_gam)
z <- psych::logistic(predict(theta_fit_gam, newdata=data.frame(x=grid[,1], y=grid[,2])))
x11()
imagen_plot(z)
select <- c()
for (i in 1:spp$n) {
  select <- rbind(select,sum((spp$x[i] == spp_sample$x) & (spp$y[i] == spp_sample$y)))
}
spp_selection <- as.data.frame(cbind(select,spp$x,spp$y))
names(spp_selection) <- c("select","x","y")
spp_selection$select <- factor(spp_selection$select)
spp_selection$select <- factor(spp_selection$select,labels = c("no","si"),levels = c(0,1))
pi_fit_gam <- mgcv::gam(select~s(x,y,bs="ad",k=7), family = binomial(link="logit"), data=spp_selection)
summary(pi_fit_gam)
gam.check(pi_fit_gam)
plot(pi_fit_gam)
z <- psych::logistic(predict(pi_fit_gam, newdata=data.frame(x=grid[,1], y=grid[,2])))
x11()
imagen_plot(z)


