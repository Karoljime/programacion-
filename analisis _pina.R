
# TRABAJO UNIDAD 02
# Analisis estadistico del ancho de fruto de piña

# 1. Cargar la base de datos
library(readxl)

datos <- read_excel("Base_organizada_ancho_fruto_pina.xlsx")

# 2. Convertir tratamientos y bloques en factores
datos$Tratamiento <- as.factor(datos$Tratamiento)
datos$Bloque <- as.factor(datos$Bloque)

# 3. Revisar la base de datos
View(datos)
str(datos)
summary(datos)

# 4. Modelo estadistico: DBCA
# Variable: ancho promedio del fruto

modelo <- aov(Ancho_promedio ~ Bloque + Tratamiento,
              data = datos)

# 5. Analisis de varianza (ANOVA)
summary(modelo)



# 6. Comparacion de medias - Tukey al 5%

tukey <- TukeyHSD(modelo, "Tratamiento")

# Mostrar resultados
print(tukey)



# 7. Grafica de resultados

# Calcular el promedio por tratamiento
medias <- aggregate(Ancho_promedio ~ Tratamiento,
                    data = datos,
                    FUN = mean)

# Crear grafica de barras
barplot(medias$Ancho_promedio,
        names.arg = medias$Tratamiento,
        col = "skyblue",
        main = "Ancho promedio del fruto de piña",
        xlab = "Tratamientos",
        ylab = "Ancho del fruto",
        ylim = c(0, 16))
