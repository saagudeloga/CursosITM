#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       PARCIAL 1 Supletorio - CEP/C 2026 1 : 07 de abril
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cargar librerias
require(summarytools)

# Lectura de datos
datos <- read.table("https://raw.githubusercontent.com/saagudeloga/CursosITM/refs/heads/main/datos2.txt", sep = "\t",header = T, dec = ",")
str(datos)

#-----------------------------------
#      PUNTO 1: Descriptiva
#-----------------------------------
# Descriptiva
descr(datos$Peso)
stby(datos$Peso, datos$Maquina, descr)
freq(datos$Maquina)


# Graficos
# Histograma por máquina
hist(datos$Peso[datos$Maquina=="1"], main="Histograma Peso - M1", xlab="Peso (gramos)", col="skyblue4", xlim = c(240,260), breaks = 20)
hist(datos$Peso[datos$Maquina=="2"], main="Histograma Peso - M2", xlab="Peso (gramos)", col="orange3", xlim = c(240,260),breaks = 20)

# Boxplot comparativo por máquina
boxplot(Peso ~ Maquina, data=datos,
        main="Peso por máquina", xlab="Máquina", ylab="Peso (g)", col=c("skyblue4","orange3"), boxwex = 0.5,
        las = 1, pch = 19);abline(h=c(245,255), lty=2, col = "red");legend("bottomleft",lwd = 1, lty = 2,bty = "n", legend = "Limites especificación", col = "red")
abline(h = 250, lty = 2, col = "darkgreen")


boxplot(datos$Peso, data=datos,
        main="Peso todas la piezas", xlab="", ylab="Peso (g)", col="grey50", boxwex = 0.5,
        las = 1, pch = 19);abline(h=c(245,255), lty=2, col = "red");legend("bottomleft",lwd = 1, lty = 2,bty = "n", legend = "Limites especificación", col = "red")
abline(h = 250, lty = 2, col = "darkgreen")
#--------------------------------------
#   PUNTO 2: Verificacion Normalidad
#--------------------------------------
# Grafico de distribucion
plot(density(datos$Peso[datos$Maquina=="1"]),
     col=rgb(0,0,1,0.6), lwd=2,
     main="Distribución de Peso por Máquina",
     xlab="Peso (g)", xlim = c(235,265),las = 1);polygon(density(datos$Peso[datos$Maquina=="1"]),
                                                   col=rgb(0,0,1,0.3), border=NA);lines(density(datos$Peso[datos$Maquina=="2"]),
                                                                                        col=rgb(1,0,0,0.6), lwd=2);polygon(density(datos$Peso[datos$Maquina=="2"]),
                                                                                                                           col=rgb(1,0,0,0.3), border=NA);legend("topright",
                                                                                                                                                                 legend=c("Máquina 1","Máquina 2"),
                                                                                                                                                                 fill=c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)),
                                                                                                                                                                 border=NA)

# Grafico QQplot
par(mfrow=c(1,2))
qqnorm(datos$Peso[datos$Maquina==1], main="QQ-plot M1", pch = 19); qqline(datos$Peso[datos$Maquina==1], lwd = 2, col = "red")
qqnorm(datos$Peso[datos$Maquina==2], main="QQ-plot M2", pch = 19); qqline(datos$Peso[datos$Maquina==2], lwd = 2, col = "red");par(mfrow=c(1,1))

# Prueba de hipotesis para comprobar normalidad
shapiro.test(datos$Peso[datos$Maquina==1]) # Prueba Normalidad Maquina 1
shapiro.test(datos$Peso[datos$Maquina==2]) # Prueba Normalidad Maquina 2

#----------------------------------
#     PUNTO 3: Prueba hipotesis
#----------------------------------
t.test(datos$Peso[datos$Maquina==1], mu=250, alternative="two.sided")
t.test(datos$Peso[datos$Maquina==2], mu=250, alternative="two.sided")

#-------------------------------------------------------
#   PUNTO 4: Comparacion de pesos entre maquinas
#-------------------------------------------------------
# Comparacion entre maquinas
t.test(datos$Peso ~ Maquina, data = datos, var.equal = F)

#----------------------------------------------------
#         PUNTO 5: Piezas no conformes
#----------------------------------------------------
bp <- barplot(tabla <- table(datos$fuera_especificacion_peso, datos$Maquina),
              beside=TRUE, col=c("steelblue","tomato"),
              ylim=c(0,max(tabla)*1.3),
              legend.text=TRUE,
              main="Conformidad por Máquina",
              ylab="Número de piezas",las=1,xla="Máquina"); text(bp, tabla+40, labels=paste0(tabla,"\n(",round(prop.table(tabla,2)*100,1),"%)"), cex=1)
