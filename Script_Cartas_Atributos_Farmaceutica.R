#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Taller Cartas de Control para Atributos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Planta Farmaceutica
# Proceso: Inspeccionar 30 subgrupos de 100 tabletas

# Lectura de datos
datos <- read.delim(file = "https://raw.githubusercontent.com/saagudeloga/CursosITM/refs/heads/main/datos_farmaceutica.txt")

# Libreria para instalar que realiza las cartas
install.packages(qcc)
library(qcc)

#-------------------------
# 1. Carta p
#-------------------------
qcc(datos$n_defectuosas, 
    sizes = datos$n_inspeccionadas,
    type = "p",
    title = "Carta p", axes.las = 1)

#-------------------------
# 2. Carta np
#-------------------------
qcc(datos$n_defectuosas, 
    sizes = datos$n_inspeccionadas,
    type = "np",
    title = "Carta np", axes.las = 1)

#-------------------------
# 3. Carta c
#-------------------------
qcc(datos$total_defectos, 
    type = "c",
    title = "Carta c")

#-------------------------
# 4. Carta u
#-------------------------
qcc(datos$total_defectos, 
    sizes = datos$n_inspeccionadas,
    type = "u",
    title = "Carta u")