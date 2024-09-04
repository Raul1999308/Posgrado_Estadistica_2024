prof_ur1 <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_ur1, encoding = "latin1")
head(profepa)

summary(profepa)

mean(profepa$Inspección)

ins <- subset(profepa$Inspección >= mean(profepa$Inspección)

bajo <- subset(profepa, profepa$Inspección <= mean(profepa$Inspección)
            
cero <- subset(profepa, profepa$Inspección == 0)

todos <- subest(profepa, profepa$Inspección !=0)

# Descarga de sitios seguros ----------------------------------------------


library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
conjunto$Especie <- as.factor(conjunto$Especie)
conjunto$Clase <- as.factor(conjunto$Clase)
summary(conjunto)

boxplot(conjunto$Diametro ~ conjunto$Especie)

SpFH <- subset(conjunto, conjunto$Especie != "c")
tapply(SpFH$Diametro, SpFH$Especie, mean)

boxplot(SpFH$Diametro ~ SpFH$Especie, xlab = "Especies", ylab = " Diametro", col = "indianred")


# Normalidad de datos -----------------------------------------------------

shapiro.test(SpFH$Diametro)

# Homogenidad de varianza -------------------------------------------------

bartlett.test(SpFH$Diametro, SpFH$Especie)


# Prueba de t Independientes ----------------------------------------------

t.test(SpFH$Diametro ~ SpFH$Especie, var.equal = TRUE)


# Prueba de t de una muestra ----------------------------------------------

t.test(conjunto$Diametro, mu = 16.4)

prod <- read.csv("produccion.csv", header = T)

boxplot(prod$Kgsem ~ prod$Tiempo)

t.test(prod$Kgsem ~ prod$Tiempo, paired = T)
tapply(prod$Kgsem, prod$Tiempo, mean)
