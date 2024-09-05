library(repmis)
conjunto<-source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

# función subset para la variable Altura ----------------------------------

H.media <- subset(Cuadro1, Cuadro1$Altura <= mean(Cuadro1$Altura))
mean(Cuadro1$Altura)
H.16 <- subset(Cuadro1, Cuadro1$Altura <= 16.5)
# El numero con menor valor a 16.5 es 39.

# función subset para la variable Vecinos ---------------------------------
vecinos.3 <- subset(Cuadro1, Cuadro1$Vecinos <= 3)
# EL numero vecino con valor menor a 3 es de 26.

vecinos.4 <- subset(Cuadro1, Cuadro1$Vecinos > 4)
# El numero vecino  con valor mayor a 4 es de 11.


# función subset para la variable Diametro --------------------------------

DBH.media <- subset(Cuadro1, Cuadro1$Diametro < mean(Cuadro1$Diametro))
# El diametro menor a la media es de 25.

DBH.16 <- subset(Cuadro1, Cuadro1$Diametro > 16)
# El diametro mayor a 16 es 24.


# función subset para la variable Especie ---------------------------------

cedro <- subset(Cuadro1, Cuadro1$Especie == "C")

Tsuga <- subset(Cuadro1, Cuadro1$Especie !="C")

DH16.9 <- subset(Cuadro1, Cuadro1$Especie <= 16.9)
# las observaciones son menor o iguales de 16.9 son 0.
DH18.5 <- subset(Cuadro1, Cuadro1$Altura > 18.5)
# las observaciones mayores de 18.5 son 2



# Elaboracion de graficas -------------------------------------------------
hist(Cuadro1$Altura, col =  "red")
hist(H.media$Altura, col = "blue")
hist(H.16$Altura, col = "green")
hist(Cuadro1$Vecinos, col = "yellow")
hist(vecinos.3$Vecinos, col = "black")
hist(vecinos.4$Vecinos, col = "brown")
hist(Cuadro1$Diametro, col = "orange")
hist(DBH.media$Diametro, col = "pink")
hist(DBH.16$Diametro, col = "purple")



# Media y desviación  estándar --------------------------------------------

mean(Cuadro1$Altura) # 13.9432
sd(Cuadro1$Altura) # 2.9071
mean(H.media$Altura) # 11.5312
sd(H.media$Altura) # 1.7465
mean(H.16$Altura) # 12.8553
sd(H.16$Altura) # 2.2105
mean(Cuadro1$Vecinos) # 3.34
sd(Cuadro1$Vecinos) # 1.5985
mean(vecinos.3$Vecinos) # 2.1153
sd(vecinos.3$Vecinos) # 1.0705
mean(vecinos.4$Vecinos) # 5.4545
sd(vecinos.4$Vecinos) # 0.5222
mean(Cuadro1$Diametro) # 15.794
sd(Cuadro1$Diametro) # 3.2270
mean(DBH.media$Diametro) # 13.256
sd(DBH.media$Diametro) # 2.0986
mean(DBH.16$Diametro) # 18.4375
sd(DBH.16$Diametro) # 1.8155

