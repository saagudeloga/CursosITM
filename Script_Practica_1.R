#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           PRACTICA 1 - CEP/C 2026 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cargar librerias
require(summarytools)

# Lectura de datos
datos <- read.csv("https://raw.githubusercontent.com/saagudeloga/CursosITM/refs/heads/main/Datos_practica_ITM.csv")
str(datos)

#-----------------------------------
#      PUNTO 1: Descriptiva
#-----------------------------------
# Descriptiva
descr(datos$Longitud)
stby(datos$Longitud, datos$Maquina, descr)
freq(datos$Maquina)

# Graficos
# Histograma por máquina
hist(datos$Longitud[datos$Maquina=="1"], main="Histograma Longitud - M1", xlab="cm", col="skyblue4", xlim = c(6,11))
hist(datos$Longitud[datos$Maquina=="2"], main="Histograma Longitud - M2", xlab="cm", col="orange3", xlim = c(6,11))

# Boxplot comparativo
boxplot(Longitud ~ Maquina, data=datos,
        main="Longitud por máquina", xlab="Máquina", ylab="Longitud (cm)", col=c("skyblue4","orange3"), boxwex = 0.5,
        las = 1, pch = 19)
abline(h=8.7, lty=2, col = "red")
legend("bottomleft",lwd = 1, lty = 2,bty = "n", legend = "Media Muestral = 8.7", col = "red")

#--------------------------------------
#   PUNTO 2: Verificacion Normalidad
#--------------------------------------
# Grafico de distribucion
plot(density(datos$Longitud[datos$Maquina=="1"]),
     col=rgb(0,0,1,0.6), lwd=2,
     main="Distribución de Longitud por Máquina",
     xlab="Longitud (cm)", xlim = c(6,11));polygon(density(datos$Longitud[datos$Maquina=="1"]),
        col=rgb(0,0,1,0.3), border=NA);lines(density(datos$Longitud[datos$Maquina=="2"]),
      col=rgb(1,0,0,0.6), lwd=2);polygon(density(datos$Longitud[datos$Maquina=="2"]),
        col=rgb(1,0,0,0.3), border=NA);legend("topright",
       legend=c("Máquina 1","Máquina 2"),
       fill=c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)),
       border=NA)

# Grafico QQplot
par(mfrow=c(1,2))
qqnorm(datos$Longitud[datos$Maquina==1], main="QQ-plot M1", pch = 19); qqline(datos$Longitud[datos$Maquina==1], lwd = 2, col = "red")
qqnorm(datos$Longitud[datos$Maquina==2], main="QQ-plot M2", pch = 19); qqline(datos$Longitud[datos$Maquina==2], lwd = 2, col = "red");par(mfrow=c(1,1))

# Prueba de hipotesis para comprobar normalidad
shapiro.test(datos$Longitud[datos$Maquina==1]) # Maquina 1
shapiro.test(datos$Longitud[datos$Maquina==2]) # Maquina 2

#---------------------------------
#     PUNTO 3: Inferencia
#---------------------------------
t.test(datos$Longitud[datos$Maquina==1],conf.level = 0.95)
t.test(datos$Longitud[datos$Maquina==2],conf.level = 0.95)

#----------------------------------
#     PUNTO 4: Prueba hipotesis
#----------------------------------
t.test(datos$Longitud[datos$Maquina==1], mu=8.7, alternative="two.sided")
t.test(datos$Longitud[datos$Maquina==2], mu=8.7, alternative="two.sided")

#-----------------------------------
#  PUNTO 5: Inferencia comparacion
#-----------------------------------
# Comparacion entre maquinas
t.test(datos$Longitud ~ Maquina, data = datos, var.equal = F)

