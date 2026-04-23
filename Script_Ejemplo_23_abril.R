datos <- data.frame(
  subgrupo = 1:10,
  n = c(98, 102, 100, 97, 105, 101, 99, 103, 100, 96),
  defectuosas = c(4, 5, 4, 6, 7, 5, 12, 6, 5, 11),
  total_defectos = c(5, 6, 5, 7, 9, 6, 18, 7, 6, 16)
)

# Cálculos
datos$p <- datos$defectuosas / datos$n
datos$u <- datos$total_defectos / datos$n

datos

library(qcc)

#-------------------------
# Carta p (proporción)
#-------------------------
qcc(datos$defectuosas,
    sizes = datos$n,
    type = "p",
    title = "Carta p – Proporción de defectuosas",
    xlab = "Subgrupo",
    ylab = "Proporción")

#-------------------------
# Carta u (defectos por unidad)
#-------------------------
qcc(datos$total_defectos,
    sizes = datos$n,
    type = "u",
    title = "Carta u – Defectos por unidad",
    xlab = "Subgrupo",
    ylab = "Defectos por unidad")
