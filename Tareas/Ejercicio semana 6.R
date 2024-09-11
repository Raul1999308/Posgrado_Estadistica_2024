
# Ejercicio Datos Madera MET ----------------------------------------------

library(repmis)
url <- "https://raw.githubusercontent.com/mgtagle/Met_Est_2024/main/Datos_Madera_MET.csv"
Datos_Madera <- read.csv(url, header = T)
View(Datos_Madera)


# Establecer la hipotesis de partida --------------------------------------

# H0: existe
# H1


# Representacion grafica de la variable y sus niveles de factor -----------
boxplot(Datos_Madera$Peso_g ~ Datos_Madera$Especie,xlab = "Niveles de factor", ylab = "pesos(gr)", col = "red")


# Estadisticas descriptiva de la variable comun (peso_g) ------------------

tapply(Datos_Madera$Peso_g, Datos_Madera$Especie, mean)
tapply(Datos_Madera$Peso_g, Datos_Madera$Especie, var)
# la varianza del peso de la especie gavia es 7 veces myor que la
# la varianza de el peso observado de la barreta.

#¿cuantos niveles de factor existen?



#Realizar la prueba, asi como revisar si los supuestos de normalidad y homogenidad de varianza se cumple
shapiro.test(Datos_Madera$Peso_g)
bartlett.test(Datos_Madera$Peso_g ~ Datos_Madera$Especie)
Datos_Madera$peso_t <- log10(Datos_Madera$Peso_g + 0.5)
tapply(Datos_Madera$Peso_g, Datos_Madera$Especie, var)
boxplot(Datos_Madera$Peso_g ~ Datos_Madera$Especie)


t.test(Datos_Madera$Peso_g ~ Datos_Madera$Especie, var.equal = T)




# ¿Que hipotesis se acepta? -----------------------------------------------


