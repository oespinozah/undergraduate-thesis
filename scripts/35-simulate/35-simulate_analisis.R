
git_path <- function() {
  system('git rev-parse --show-toplevel', intern = TRUE)
}
proj_path <- git_path()

#cargando los resultados
resultados01 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_01.rds"))$resultados
resultados02 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_02.rds"))$resultados 
resultados03 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_03.rds"))$resultados 
resultados04 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_04.rds"))$resultados 
resultados05 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_05.rds"))$resultados 
resultados06 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_06.rds"))$resultados 
resultados07 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_07.rds"))$resultados 
resultados08 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_08.rds"))$resultados 
resultados09 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_09.rds"))$resultados 
resultados10 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_10.rds"))$resultados 
resultados11 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_11.rds"))$resultados 
resultados12 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_12.rds"))$resultados 
resultados13 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_13.rds"))$resultados 
resultados14 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_14.rds"))$resultados 
resultados15 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_15.rds"))$resultados 
resultados16 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_16.rds"))$resultados 
resultados17 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_17.rds"))$resultados 
resultados18 <- readRDS(file.path(proj_path,"scripts","35-simulate","simulacion_18.rds"))$resultados 


resultados01 <- resultados01[!is.na(resultados01$train_w_auc),]
resultados02 <- resultados02[!is.na(resultados02$train_w_auc),]
resultados03 <- resultados03[!is.na(resultados03$train_w_auc),]
resultados04 <- resultados04[!is.na(resultados04$train_w_auc),]
resultados05 <- resultados05[!is.na(resultados05$train_w_auc),]
resultados06 <- resultados06[!is.na(resultados06$train_w_auc),]
resultados07 <- resultados07[!is.na(resultados07$train_w_auc),]
resultados08 <- resultados08[!is.na(resultados08$train_w_auc),]
resultados09 <- resultados09[!is.na(resultados09$train_w_auc),]
resultados10 <- resultados10[!is.na(resultados10$train_w_auc),]
resultados11 <- resultados11[!is.na(resultados11$train_w_auc),]
resultados12 <- resultados12[!is.na(resultados12$train_w_auc),]
resultados13 <- resultados13[!is.na(resultados13$train_w_auc),]
resultados14 <- resultados14[!is.na(resultados14$train_w_auc),]
resultados15 <- resultados15[!is.na(resultados15$train_w_auc),]
resultados16 <- resultados16[!is.na(resultados16$train_w_auc),]
resultados17 <- resultados17[!is.na(resultados17$train_w_auc),]
resultados18 <- resultados18[!is.na(resultados18$train_w_auc),]



x11()
par(mfrow=c(3,3))
plot(density(resultados01$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 01", col="green",ylab="Densidad",xlab="")
lines(density(resultados01$p_sample),col="red")
lines(density(resultados01$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1.,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados02$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 02", col="green",ylab="Densidad",xlab="")
lines(density(resultados02$p_sample),col="red")
lines(density(resultados02$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados03$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 03", col="green",ylab="Densidad",xlab="")
lines(density(resultados03$p_sample),col="red")
lines(density(resultados03$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados04$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 04", col="green",ylab="Densidad",xlab="")
lines(density(resultados04$p_sample),col="red")
lines(density(resultados04$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados05$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 05", col="green",ylab="Densidad",xlab="")
lines(density(resultados05$p_sample),col="red")
lines(density(resultados05$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados06$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 06", col="green",ylab="Densidad",xlab="")
lines(density(resultados06$p_sample),col="red")
lines(density(resultados06$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados07$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 07", col="green",ylab="Densidad",xlab="")
lines(density(resultados07$p_sample),col="red")
lines(density(resultados07$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados08$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 08", col="green",ylab="Densidad",xlab="")
lines(density(resultados08$p_sample),col="red")
lines(density(resultados08$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados09$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 09", col="green",ylab="Densidad",xlab="")
lines(density(resultados09$p_sample),col="red")
lines(density(resultados09$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
dev.copy(pdf,file.path(proj_path,"reports","Tesis", "graficos", "simu_01-09.pdf"))
dev.off()

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

x11()
par(mfrow=c(3,3))
plot(density(resultados10$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 10", col="green",ylab="Densidad",xlab="")
lines(density(resultados10$p_sample),col="red")
lines(density(resultados10$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1.,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados11$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 11", col="green",ylab="Densidad",xlab="")
lines(density(resultados11$p_sample),col="red")
lines(density(resultados11$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados12$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 12", col="green",ylab="Densidad",xlab="")
lines(density(resultados12$p_sample),col="red")
lines(density(resultados12$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados13$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 13", col="green",ylab="Densidad",xlab="")
lines(density(resultados13$p_sample),col="red")
lines(density(resultados13$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados14$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 14", col="green",ylab="Densidad",xlab="")
lines(density(resultados14$p_sample),col="red")
lines(density(resultados14$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados15$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 15", col="green",ylab="Densidad",xlab="")
lines(density(resultados15$p_sample),col="red")
lines(density(resultados15$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados16$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 16", col="green",ylab="Densidad",xlab="")
lines(density(resultados16$p_sample),col="red")
lines(density(resultados16$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados17$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 17", col="green",ylab="Densidad",xlab="")
lines(density(resultados17$p_sample),col="red")
lines(density(resultados17$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados18$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 18", col="green",ylab="Densidad",xlab="")
lines(density(resultados18$p_sample),col="red")
lines(density(resultados18$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
dev.copy(pdf,file.path(proj_path,"reports","Tesis", "graficos", "simu_10-18.pdf"))
dev.off()



resultados01$p
resultados01$p_sample
resultados01$p_w





compa01 <- table(abs(resultados01$p - resultados01$p_sample)  >= abs(resultados01$p -resultados01$p_w))
compa02 <- table(abs(resultados02$p - resultados02$p_sample)  >= abs(resultados02$p -resultados02$p_w))
compa03 <- table(abs(resultados03$p - resultados03$p_sample)  >= abs(resultados03$p -resultados03$p_w))
compa04 <- table(abs(resultados04$p - resultados04$p_sample)  >= abs(resultados04$p -resultados04$p_w))
compa05 <- table(abs(resultados05$p - resultados05$p_sample)  >= abs(resultados05$p -resultados05$p_w))
compa06 <- table(abs(resultados06$p - resultados06$p_sample)  >= abs(resultados06$p -resultados06$p_w))
compa07 <- table(abs(resultados07$p - resultados07$p_sample)  >= abs(resultados07$p -resultados07$p_w))
compa08 <- table(abs(resultados08$p - resultados08$p_sample)  >= abs(resultados08$p -resultados08$p_w))
compa09 <- table(abs(resultados09$p - resultados09$p_sample)  >= abs(resultados09$p -resultados09$p_w))
compa10 <- table(abs(resultados10$p - resultados10$p_sample)  >= abs(resultados10$p -resultados10$p_w))
compa11 <- table(abs(resultados11$p - resultados11$p_sample)  >= abs(resultados11$p -resultados11$p_w))
compa12 <- table(abs(resultados12$p - resultados12$p_sample)  >= abs(resultados12$p -resultados12$p_w))
compa13 <- table(abs(resultados13$p - resultados13$p_sample)  >= abs(resultados13$p -resultados13$p_w))
compa14 <- table(abs(resultados14$p - resultados14$p_sample)  >= abs(resultados14$p -resultados14$p_w))
compa15 <- table(abs(resultados15$p - resultados15$p_sample)  >= abs(resultados15$p -resultados15$p_w))
compa16 <- table(abs(resultados16$p - resultados16$p_sample)  >= abs(resultados16$p -resultados16$p_w))
compa17 <- table(abs(resultados17$p - resultados17$p_sample)  >= abs(resultados17$p -resultados17$p_w))
compa18 <- table(abs(resultados18$p - resultados18$p_sample)  >= abs(resultados18$p -resultados18$p_w))

matriz_comparacion <- matrix(ncol = 7,nrow = 18)


matriz_comparacion[01,1] <- nrow(resultados01)
matriz_comparacion[02,1] <- nrow(resultados02)
matriz_comparacion[03,1] <- nrow(resultados03)
matriz_comparacion[04,1] <- nrow(resultados04)
matriz_comparacion[05,1] <- nrow(resultados05)
matriz_comparacion[06,1] <- nrow(resultados06)
matriz_comparacion[07,1] <- nrow(resultados07)
matriz_comparacion[08,1] <- nrow(resultados08)
matriz_comparacion[09,1] <- nrow(resultados09)
matriz_comparacion[10,1] <- nrow(resultados10)
matriz_comparacion[11,1] <- nrow(resultados11)
matriz_comparacion[12,1] <- nrow(resultados12)
matriz_comparacion[13,1] <- nrow(resultados13)
matriz_comparacion[14,1] <- nrow(resultados14)
matriz_comparacion[15,1] <- nrow(resultados15)
matriz_comparacion[16,1] <- nrow(resultados16)
matriz_comparacion[17,1] <- nrow(resultados17)
matriz_comparacion[18,1] <- nrow(resultados18)


matriz_comparacion[01,2] <- round(compa01[2]/sum(compa01)*100,2)
matriz_comparacion[02,2] <- round(compa02[2]/sum(compa02)*100,2)
matriz_comparacion[03,2] <- round(compa03[2]/sum(compa03)*100,2)
matriz_comparacion[04,2] <- round(compa04[2]/sum(compa04)*100,2)
matriz_comparacion[05,2] <- round(compa05[2]/sum(compa05)*100,2)
matriz_comparacion[06,2] <- round(compa06[2]/sum(compa06)*100,2)
matriz_comparacion[07,2] <- round(compa07[2]/sum(compa07)*100,2)
matriz_comparacion[08,2] <- round(compa08[2]/sum(compa08)*100,2)
matriz_comparacion[09,2] <- round(compa09[2]/sum(compa09)*100,2)
matriz_comparacion[10,2] <- round(compa10[2]/sum(compa10)*100,2)
matriz_comparacion[11,2] <- round(compa11[2]/sum(compa11)*100,2)
matriz_comparacion[12,2] <- round(compa12[2]/sum(compa12)*100,2)
matriz_comparacion[13,2] <- round(compa13[2]/sum(compa13)*100,2)
matriz_comparacion[14,2] <- round(compa14[2]/sum(compa14)*100,2)
matriz_comparacion[15,2] <- round(compa15[2]/sum(compa15)*100,2)
matriz_comparacion[16,2] <- round(compa16[2]/sum(compa16)*100,2)
matriz_comparacion[17,2] <- round(compa17[2]/sum(compa17)*100,2)
matriz_comparacion[18,2] <- round(compa18[2]/sum(compa18)*100,2)


matriz_comparacion[01,3] <- mean(resultados01$n)
matriz_comparacion[02,3] <- mean(resultados02$n)
matriz_comparacion[03,3] <- mean(resultados03$n)
matriz_comparacion[04,3] <- mean(resultados04$n)
matriz_comparacion[05,3] <- mean(resultados05$n)
matriz_comparacion[06,3] <- mean(resultados06$n)
matriz_comparacion[07,3] <- mean(resultados07$n)
matriz_comparacion[08,3] <- mean(resultados08$n)
matriz_comparacion[09,3] <- mean(resultados09$n)
matriz_comparacion[10,3] <- mean(resultados10$n)
matriz_comparacion[11,3] <- mean(resultados11$n)
matriz_comparacion[12,3] <- mean(resultados12$n)
matriz_comparacion[13,3] <- mean(resultados13$n)
matriz_comparacion[14,3] <- mean(resultados14$n)
matriz_comparacion[15,3] <- mean(resultados15$n)
matriz_comparacion[16,3] <- mean(resultados16$n)
matriz_comparacion[17,3] <- mean(resultados17$n)
matriz_comparacion[18,3] <- mean(resultados18$n)


matriz_comparacion[01,4] <- round(mean(resultados01$n_sample/resultados01$n)*100,2)
matriz_comparacion[02,4] <- round(mean(resultados02$n_sample/resultados02$n)*100,2)
matriz_comparacion[03,4] <- round(mean(resultados03$n_sample/resultados03$n)*100,2)
matriz_comparacion[04,4] <- round(mean(resultados04$n_sample/resultados04$n)*100,2)
matriz_comparacion[05,4] <- round(mean(resultados05$n_sample/resultados05$n)*100,2)
matriz_comparacion[06,4] <- round(mean(resultados06$n_sample/resultados06$n)*100,2)
matriz_comparacion[07,4] <- round(mean(resultados07$n_sample/resultados07$n)*100,2)
matriz_comparacion[08,4] <- round(mean(resultados08$n_sample/resultados08$n)*100,2)
matriz_comparacion[09,4] <- round(mean(resultados09$n_sample/resultados09$n)*100,2)
matriz_comparacion[10,4] <- round(mean(resultados10$n_sample/resultados10$n)*100,2)
matriz_comparacion[11,4] <- round(mean(resultados11$n_sample/resultados11$n)*100,2)
matriz_comparacion[12,4] <- round(mean(resultados12$n_sample/resultados12$n)*100,2)
matriz_comparacion[13,4] <- round(mean(resultados13$n_sample/resultados13$n)*100,2)
matriz_comparacion[14,4] <- round(mean(resultados14$n_sample/resultados14$n)*100,2)
matriz_comparacion[15,4] <- round(mean(resultados15$n_sample/resultados15$n)*100,2)
matriz_comparacion[16,4] <- round(mean(resultados16$n_sample/resultados16$n)*100,2)
matriz_comparacion[17,4] <- round(mean(resultados17$n_sample/resultados17$n)*100,2)
matriz_comparacion[18,4] <- round(mean(resultados18$n_sample/resultados18$n)*100,2)


matriz_comparacion[01,5] <- mean(resultados01$p)
matriz_comparacion[02,5] <- mean(resultados02$p)
matriz_comparacion[03,5] <- mean(resultados03$p)
matriz_comparacion[04,5] <- mean(resultados04$p)
matriz_comparacion[05,5] <- mean(resultados05$p)
matriz_comparacion[06,5] <- mean(resultados06$p)
matriz_comparacion[07,5] <- mean(resultados07$p)
matriz_comparacion[08,5] <- mean(resultados08$p)
matriz_comparacion[09,5] <- mean(resultados09$p)
matriz_comparacion[10,5] <- mean(resultados10$p)
matriz_comparacion[11,5] <- mean(resultados11$p)
matriz_comparacion[12,5] <- mean(resultados12$p)
matriz_comparacion[13,5] <- mean(resultados13$p)
matriz_comparacion[14,5] <- mean(resultados14$p)
matriz_comparacion[15,5] <- mean(resultados15$p)
matriz_comparacion[16,5] <- mean(resultados16$p)
matriz_comparacion[17,5] <- mean(resultados17$p)
matriz_comparacion[18,5] <- mean(resultados18$p)


matriz_comparacion[01,6] <- mean((resultados01$p-resultados01$p_sample)^2)
matriz_comparacion[02,6] <- mean((resultados02$p-resultados02$p_sample)^2)
matriz_comparacion[03,6] <- mean((resultados03$p-resultados03$p_sample)^2)
matriz_comparacion[04,6] <- mean((resultados04$p-resultados04$p_sample)^2)
matriz_comparacion[05,6] <- mean((resultados05$p-resultados05$p_sample)^2)
matriz_comparacion[06,6] <- mean((resultados06$p-resultados06$p_sample)^2)
matriz_comparacion[07,6] <- mean((resultados07$p-resultados07$p_sample)^2)
matriz_comparacion[08,6] <- mean((resultados08$p-resultados08$p_sample)^2)
matriz_comparacion[09,6] <- mean((resultados09$p-resultados09$p_sample)^2)
matriz_comparacion[10,6] <- mean((resultados10$p-resultados10$p_sample)^2)
matriz_comparacion[11,6] <- mean((resultados11$p-resultados11$p_sample)^2)
matriz_comparacion[12,6] <- mean((resultados12$p-resultados12$p_sample)^2)
matriz_comparacion[13,6] <- mean((resultados13$p-resultados13$p_sample)^2)
matriz_comparacion[14,6] <- mean((resultados14$p-resultados14$p_sample)^2)
matriz_comparacion[15,6] <- mean((resultados15$p-resultados15$p_sample)^2)
matriz_comparacion[16,6] <- mean((resultados16$p-resultados16$p_sample)^2)
matriz_comparacion[17,6] <- mean((resultados17$p-resultados17$p_sample)^2)
matriz_comparacion[18,6] <- mean((resultados18$p-resultados18$p_sample)^2)


matriz_comparacion[01,7] <- mean((resultados01$p-resultados01$p_w)^2)
matriz_comparacion[02,7] <- mean((resultados02$p-resultados02$p_w)^2)
matriz_comparacion[03,7] <- mean((resultados03$p-resultados03$p_w)^2)
matriz_comparacion[04,7] <- mean((resultados04$p-resultados04$p_w)^2)
matriz_comparacion[05,7] <- mean((resultados05$p-resultados05$p_w)^2)
matriz_comparacion[06,7] <- mean((resultados06$p-resultados06$p_w)^2)
matriz_comparacion[07,7] <- mean((resultados07$p-resultados07$p_w)^2)
matriz_comparacion[08,7] <- mean((resultados08$p-resultados08$p_w)^2)
matriz_comparacion[09,7] <- mean((resultados09$p-resultados09$p_w)^2)
matriz_comparacion[10,7] <- mean((resultados10$p-resultados10$p_w)^2)
matriz_comparacion[11,7] <- mean((resultados11$p-resultados11$p_w)^2)
matriz_comparacion[12,7] <- mean((resultados12$p-resultados12$p_w)^2)
matriz_comparacion[13,7] <- mean((resultados13$p-resultados13$p_w)^2)
matriz_comparacion[14,7] <- mean((resultados14$p-resultados14$p_w)^2)
matriz_comparacion[15,7] <- mean((resultados15$p-resultados15$p_w)^2)
matriz_comparacion[16,7] <- mean((resultados16$p-resultados16$p_w)^2)
matriz_comparacion[17,7] <- mean((resultados17$p-resultados17$p_w)^2)
matriz_comparacion[18,7] <- mean((resultados18$p-resultados18$p_w)^2)





#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#


resultados01 <- resultados01[resultados01$train_w_auc > 0.5 &
                               resultados01$train_w_spec > 0.5 & 
                               resultados01$train_w_sens > 0.5 & 
                               resultados01$test_w_auc > 0.5 & 
                               resultados01$test_w_spec > 0.5 & 
                               resultados01$test_w_sens > 0.5 & 
                               abs(resultados01$train_w_auc - resultados01$test_w_auc) < 0.2, ]

resultados02 <- resultados02[resultados02$train_w_auc > 0.5 &
                               resultados02$train_w_spec > 0.5 & 
                               resultados02$train_w_sens > 0.5 & 
                               resultados02$test_w_auc > 0.5 & 
                               resultados02$test_w_spec > 0.5 & 
                               resultados02$test_w_sens > 0.5 & 
                               abs(resultados02$train_w_auc - resultados02$test_w_auc) < 0.2, ]

resultados03 <- resultados03[resultados03$train_w_auc > 0.5 &
                               resultados03$train_w_spec > 0.5 & 
                               resultados03$train_w_sens > 0.5 & 
                               resultados03$test_w_auc > 0.5 & 
                               resultados03$test_w_spec > 0.5 & 
                               resultados03$test_w_sens > 0.5 & 
                               abs(resultados03$train_w_auc - resultados03$test_w_auc) < 0.2, ]

resultados04 <- resultados04[resultados04$train_w_auc > 0.5 &
                               resultados04$train_w_spec > 0.5 & 
                               resultados04$train_w_sens > 0.5 & 
                               resultados04$test_w_auc > 0.5 & 
                               resultados04$test_w_spec > 0.5 & 
                               resultados04$test_w_sens > 0.5 & 
                               abs(resultados04$train_w_auc - resultados04$test_w_auc) < 0.2, ]

resultados05 <- resultados05[resultados05$train_w_auc > 0.5 &
                               resultados05$train_w_spec > 0.5 & 
                               resultados05$train_w_sens > 0.5 & 
                               resultados05$test_w_auc > 0.5 & 
                               resultados05$test_w_spec > 0.5 & 
                               resultados05$test_w_sens > 0.5 & 
                               abs(resultados05$train_w_auc - resultados05$test_w_auc) < 0.2, ]

resultados06 <- resultados06[resultados06$train_w_auc > 0.5 &
                               resultados06$train_w_spec > 0.5 & 
                               resultados06$train_w_sens > 0.5 & 
                               resultados06$test_w_auc > 0.5 & 
                               resultados06$test_w_spec > 0.5 & 
                               resultados06$test_w_sens > 0.5 & 
                               abs(resultados06$train_w_auc - resultados06$test_w_auc) < 0.2, ]

resultados07 <- resultados07[resultados07$train_w_auc > 0.5 &
                               resultados07$train_w_spec > 0.5 & 
                               resultados07$train_w_sens > 0.5 & 
                               resultados07$test_w_auc > 0.5 & 
                               resultados07$test_w_spec > 0.5 & 
                               resultados07$test_w_sens > 0.5 & 
                               abs(resultados07$train_w_auc - resultados07$test_w_auc) < 0.2, ]

resultados08 <- resultados08[resultados08$train_w_auc > 0.5 &
                               resultados08$train_w_spec > 0.5 & 
                               resultados08$train_w_sens > 0.5 & 
                               resultados08$test_w_auc > 0.5 & 
                               resultados08$test_w_spec > 0.5 & 
                               resultados08$test_w_sens > 0.5 & 
                               abs(resultados08$train_w_auc - resultados08$test_w_auc) < 0.2, ]

resultados09 <- resultados09[resultados09$train_w_auc > 0.5 &
                               resultados09$train_w_spec > 0.5 & 
                               resultados09$train_w_sens > 0.5 & 
                               resultados09$test_w_auc > 0.5 & 
                               resultados09$test_w_spec > 0.5 & 
                               resultados09$test_w_sens > 0.5 & 
                               abs(resultados09$train_w_auc - resultados09$test_w_auc) < 0.2, ]

resultados10 <- resultados10[resultados10$train_w_auc > 0.5 &
                               resultados10$train_w_spec > 0.5 & 
                               resultados10$train_w_sens > 0.5 & 
                               resultados10$test_w_auc > 0.5 & 
                               resultados10$test_w_spec > 0.5 & 
                               resultados10$test_w_sens > 0.5 & 
                               abs(resultados10$train_w_auc - resultados10$test_w_auc) < 0.2, ]

resultados11 <- resultados11[resultados11$train_w_auc > 0.5 &
                               resultados11$train_w_spec > 0.5 & 
                               resultados11$train_w_sens > 0.5 & 
                               resultados11$test_w_auc > 0.5 & 
                               resultados11$test_w_spec > 0.5 & 
                               resultados11$test_w_sens > 0.5 & 
                               abs(resultados11$train_w_auc - resultados11$test_w_auc) < 0.2, ]

resultados12 <- resultados12[resultados12$train_w_auc > 0.5 &
                               resultados12$train_w_spec > 0.5 & 
                               resultados12$train_w_sens > 0.5 & 
                               resultados12$test_w_auc > 0.5 & 
                               resultados12$test_w_spec > 0.5 & 
                               resultados12$test_w_sens > 0.5 & 
                               abs(resultados12$train_w_auc - resultados12$test_w_auc) < 0.2, ]

resultados13 <- resultados13[resultados13$train_w_auc > 0.5 &
                               resultados13$train_w_spec > 0.5 & 
                               resultados13$train_w_sens > 0.5 & 
                               resultados13$test_w_auc > 0.5 & 
                               resultados13$test_w_spec > 0.5 & 
                               resultados13$test_w_sens > 0.5 & 
                               abs(resultados13$train_w_auc - resultados13$test_w_auc) < 0.2, ]

resultados14 <- resultados14[resultados14$train_w_auc > 0.5 &
                               resultados14$train_w_spec > 0.5 & 
                               resultados14$train_w_sens > 0.5 & 
                               resultados14$test_w_auc > 0.5 & 
                               resultados14$test_w_spec > 0.5 & 
                               resultados14$test_w_sens > 0.5 & 
                               abs(resultados14$train_w_auc - resultados14$test_w_auc) < 0.2, ]

resultados15 <- resultados15[resultados15$train_w_auc > 0.5 &
                               resultados15$train_w_spec > 0.5 & 
                               resultados15$train_w_sens > 0.5 & 
                               resultados15$test_w_auc > 0.5 & 
                               resultados15$test_w_spec > 0.5 & 
                               resultados15$test_w_sens > 0.5 & 
                               abs(resultados15$train_w_auc - resultados15$test_w_auc) < 0.2, ]

resultados16 <- resultados16[resultados16$train_w_auc > 0.5 &
                               resultados16$train_w_spec > 0.5 & 
                               resultados16$train_w_sens > 0.5 & 
                               resultados16$test_w_auc > 0.5 & 
                               resultados16$test_w_spec > 0.5 & 
                               resultados16$test_w_sens > 0.5 & 
                               abs(resultados16$train_w_auc - resultados16$test_w_auc) < 0.2, ]

resultados17 <- resultados17[resultados17$train_w_auc > 0.5 &
                               resultados17$train_w_spec > 0.5 & 
                               resultados17$train_w_sens > 0.5 & 
                               resultados17$test_w_auc > 0.5 & 
                               resultados17$test_w_spec > 0.5 & 
                               resultados17$test_w_sens > 0.5 & 
                               abs(resultados17$train_w_auc - resultados17$test_w_auc) < 0.2, ]

resultados18 <- resultados18[resultados18$train_w_auc > 0.5 &
                               resultados18$train_w_spec > 0.5 & 
                               resultados18$train_w_sens > 0.5 & 
                               resultados18$test_w_auc > 0.5 & 
                               resultados18$test_w_spec > 0.5 & 
                               resultados18$test_w_sens > 0.5 & 
                               abs(resultados18$train_w_auc - resultados18$test_w_auc) < 0.2, ]


x11()
par(mfrow=c(3,3))
plot(density(resultados01$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 01", col="green",ylab="Densidad",xlab="")
lines(density(resultados01$p_sample),col="red")
lines(density(resultados01$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1.,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados02$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 02", col="green",ylab="Densidad",xlab="")
lines(density(resultados02$p_sample),col="red")
lines(density(resultados02$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados03$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 03", col="green",ylab="Densidad",xlab="")
lines(density(resultados03$p_sample),col="red")
lines(density(resultados03$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados04$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 04", col="green",ylab="Densidad",xlab="")
lines(density(resultados04$p_sample),col="red")
lines(density(resultados04$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados05$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 05", col="green",ylab="Densidad",xlab="")
lines(density(resultados05$p_sample),col="red")
lines(density(resultados05$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados06$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 06", col="green",ylab="Densidad",xlab="")
lines(density(resultados06$p_sample),col="red")
lines(density(resultados06$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados07$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 07", col="green",ylab="Densidad",xlab="")
lines(density(resultados07$p_sample),col="red")
lines(density(resultados07$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados08$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 08", col="green",ylab="Densidad",xlab="")
lines(density(resultados08$p_sample),col="red")
lines(density(resultados08$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados09$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 09", col="green",ylab="Densidad",xlab="")
lines(density(resultados09$p_sample),col="red")
lines(density(resultados09$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
dev.copy(pdf,file.path(proj_path,"reports","Tesis", "graficos", "simu_01-09.pdf"))
dev.off()

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

x11()
par(mfrow=c(3,3))
plot(density(resultados10$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 10", col="green",ylab="Densidad",xlab="")
lines(density(resultados10$p_sample),col="red")
lines(density(resultados10$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1.,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados11$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 11", col="green",ylab="Densidad",xlab="")
lines(density(resultados11$p_sample),col="red")
lines(density(resultados11$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados12$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 12", col="green",ylab="Densidad",xlab="")
lines(density(resultados12$p_sample),col="red")
lines(density(resultados12$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados13$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 13", col="green",ylab="Densidad",xlab="")
lines(density(resultados13$p_sample),col="red")
lines(density(resultados13$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados14$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 14", col="green",ylab="Densidad",xlab="")
lines(density(resultados14$p_sample),col="red")
lines(density(resultados14$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados15$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 15", col="green",ylab="Densidad",xlab="")
lines(density(resultados15$p_sample),col="red")
lines(density(resultados15$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados16$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 16", col="green",ylab="Densidad",xlab="")
lines(density(resultados16$p_sample),col="red")
lines(density(resultados16$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados17$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 17", col="green",ylab="Densidad",xlab="")
lines(density(resultados17$p_sample),col="red")
lines(density(resultados17$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
plot(density(resultados18$p),xlim=c(0.05,0.65), ylim=c(0,20),
     main = "Escenario 18", col="green",ylab="Densidad",xlab="")
lines(density(resultados18$p_sample),col="red")
lines(density(resultados18$p_w),col="blue")
grid()
legend(x= "topright", lty = c(1,1,1), col = c(3,2,4), lwd = 1,  cex = 0.5,  
       legend = c("Prevalencia","Prevalencia estimada","Prevalencia corregida"),
       text.col = "black",  horiz = F , 
)
dev.copy(pdf,file.path(proj_path,"reports","Tesis", "graficos", "simu_10-18.pdf"))
dev.off()



resultados01[,c("train_w_auc","test_w_auc","p","p_sample","p_w")]
resultados01[abs(resultados01$p - resultados01$p_w) < abs(resultados01$p - resultados01$p_sample),c("train_w_auc","test_w_auc","p","p_sample","p_w")]




