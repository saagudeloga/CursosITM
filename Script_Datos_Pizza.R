#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Analisis descriptivo: Datos Pizzeria 2025
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cargar e instalar librerias
if (!require("summarytools", character.only = TRUE)) {
  install.packages("summarytools")
  library("summarytools", character.only = TRUE)
}

# Lectura datos
datos <- read.delim(file = "https://raw.githubusercontent.com/saagudeloga/CursosITM/refs/heads/main/Datos_Pizza_2025.txt")
str(datos)

#------------------------------------------------
# Estadistica Descriptiva: Variables Categoricas
#------------------------------------------------
# Tabla frecuencias
freq(datos$satisfaccion_cliente)
freq(datos$satisfaccion_cliente, order = "-frec") # Ordenar niveles categorias

# Tabla cruzada (contingenica)
ctable(datos$satisfaccion_cliente, datos$punto_venta, prop = "c") # % por columna
ctable(datos$satisfaccion_cliente, datos$punto_venta, prop = "r") # % por fila

# Grafico de barras
datos$satisfaccion_cliente <- factor(datos$satisfaccion_cliente, levels = c("Malo","Regular","Bueno"))
bp <- barplot(table(datos$satisfaccion_cliente), col="gray",
              main="Distribución de satisfacción del cliente", 
              xlab="Satisfacción Cliente",
              ylim = c(0,750))
text(bp, table(datos$satisfaccion_cliente), paste0("n=", table(datos$satisfaccion_cliente), "\n", round(100*prop.table(table(datos$satisfaccion_cliente)),1), "%"), pos=3)

# Grafico de barras apiladas
tab <- table(datos$satisfaccion_cliente, datos$punto_venta)
bp <- barplot(prop.table(tab, 2), beside=TRUE,
              main="Satisfacción por punto de venta (proporciones)",
              xlab="Punto de venta", ylab="Proporción",
              ylim = c(0,1))
legend("top",
       legend=rownames(tab),
       fill=c("black","gray50","grey90"),
       horiz=TRUE,
       bty="n", cex = 1.2)
text(bp, prop.table(tab,2),
     paste0("n=", tab, "\n", round(100*prop.table(tab,2),1), "%"),
     cex=1, pos=3)

#------------------------------------------------------
#  Estadistica Descriptiva: Variables Cuantitativas
#------------------------------------------------------
# Medidas de resumen
descr(datos$tiempo_entrega)
stby(datos$tiempo_entrega, datos$punto_venta, descr) #Describir por categorias

# Histograma tiempo entrega
hist(datos$tiempo_entrega,
     probability = T,
     main = "Distribución del tiempo de entrega",
     xlab = "Tiempo de entrega (minutos)",
     ylab = "",
     col = "dodgerblue2",
     border = "white",
     xlim = c(10, 55),
     breaks = 20,
     las = 1, yaxt = "n")
curve(dnorm(x, mean = mean(datos$tiempo_entrega), sd = sd(datos$tiempo_entrega)),
      add = TRUE,
      lwd = 4,
      col = "orange3");abline(v = media,
       lwd = 3,
       col = "black",
       lty = 2)

#Grafico cajas y bigotes (Cuantitativa vs categorica)
boxplot(tiempo_entrega ~ satisfaccion_cliente, data = datos,
        main = "Tiempo de entrega según satisfacción",
        xlab = "Satisfacción",
        ylab = "Minutos",
        col  = c("gray50","gray70","grey90"),
        las = 1,boxwex = 0.5, pch = 19)
