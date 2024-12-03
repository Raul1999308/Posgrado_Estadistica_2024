library(repmis)
suelo <- source_data("https://www.dropbox.com/s/3pi3huovq6qce42/obs.csv?dl=1")
head(suelo)

suelo$zone <- as.factor(suelo$zone)
suelo$wrbl <- as.factor(suelo$wrb1)


# Actividad 1 -------------------------------------------------------------

# Aplique la funcion summary a la variable contenida de arcilla (Clay) para los tres perfiles y conteste lo siguiente:
# ¿Cual es la tendencia del contenido promedio de Arcilla (clay) con respeto a la profundidad?

summary(suelo$Clay1)
summary(suelo$Clay2)
summary(suelo$Clay5)

#El contenido de arcilla aumenta a mayor profundidad

# Actividad 2 -------------------------------------------------------------

# Graficar la distribucion del primer perfil Clay 1 con la funcion de stem-and-leaf
# ¿Los datos de contenido de arcilla siguen una distribucion simetrica o con sesgo?
#   El contenido de los datos tienen un sesgo 

stem(suelo$Clay1)


# Actividad 3 -------------------------------------------------------------

# Graficar mediante un boxplot para e primer perfil Clay 1 de arcilla.
# ¿existe evidencia de outliers?, en caso de existir,
# ¿puede identificar cuales observacion son mediante una simple restriccion de datos?, pista: obsrve los valores mediante el boxplot y haga restriccion

boxplot(suelo$Clay1, 
        ylab = "contenido de arcilla",
        col = "purple",
        main = "Suelos",
        las =1)

# se observan 3 valores atipicos los cuales son los contenidos de arcilla con valor de 61, 71 y 72.
Cont.arc <- subset(suelo, suelo$Clay1 <=55)

boxplot(Cont.arc$clay1, 
        ylab = "contenido de arcilla",
        col = "pink3",
        main = "Suelos",
        las =1)



#aplicando la restriccion la grafica se presenta sin valores atipicos.



# Actividad 4 -------------------------------------------------------------

# Determine la media de la variable Clay 1
mean(suelo$Clay1)

# Estime si el contenido de arcilla promedio en los suelos tropicales de 30% es significativamente diferente que la media observada en el campo experimental TCP
t.test(suelo$Clay1, mu=30)

# De acuerdo al valor de P= 0.2702, nos muestra que no existe una diferecia signiticativa en los procentajes de contenido de arcillas. 


# Actividad 5 -------------------------------------------------------------

# La primera pregunta del investigador es saber si existe una relacion entre el contenido de arcilla en los tres perfiles. de aqui se desprende la siguiente pregunta especifica:
# ¿Existe una relacion positiva, negativa o para nada relacionados, entre los perfiles superior clay 1 e inferior Clay 5?

cor.test(suelo$Clay1, suelo$Clay5)

# Al realizar la prueba de correlacion, nos muestra que tiene una relacion positiva al observar el valor de cor=0.8977

# ¿La correlacion es estadisticamente significativa?
# si, tiene una correlaion estadisticamente significativa basandonos en el valor de p= 2.2e-16

# Actividad 6 -------------------------------------------------------------

# Despues de cada analisis de correlacion surge la siguente pregunta:
plot(suelo$Clay1, suelo$Clay5)
Clay.lm <- lm (suelo$Clay5 ~ suelo$Clay1)
summary(Clay.lm)
# ¿Es posible determinar una ecuacion significativa para predecir el comportamiento del contenido de arcilla en el perfil inferior clay 5?
# si, es posible por medio de la ecuacion de regresion y= α+β(x)

# ¿Cual es la ecuacion final para predecir el comportamiento del contenido de arcilla en el perfil mas profundo?
# Al final la ecuacion seria y= 18.7585+0.8289(x)

# son parametros alfa y beta significativos?
# si, ambos parametros muestran valores significativos, esto se demuestra con los valores de P

# ¿Cual es el porcentaje de varianza explicado por el metodo aplicado?


# Actividad 7 -------------------------------------------------------------

# Con la base de datos se puede predecir que tan variable es el contenido de arcilla en la capa mas profunda entre las cuatro zonas identificadas
# ¿Existe una forma de identificar la variacion entre las cuatro zonas que se encuentran en el estudio?

# si existe,por medio visual (plot) y por medio estadistico (anova)


# Realice una inspeccion visual del contenido de arcilla en el perfil Clay5 y las cuatro zonas presentes en el area de estudio. ¿existen indicios de que las cuatro zonas son diferentes en cuanto al contenido de arcillas en el perfil clay 5?

plot(suelo$Clay5 ~ suelo$zone)

# Si, existen diferencias en los sitios.

# observa alguna tendencia en los datos de las diferentes zonas

by(suelo$Clay5, suelo$zone, summary)

# Se observa que conforme se va cambiando de sitio el porcentaje de arcilla va disminuyendo.




# Actividad 8 -------------------------------------------------------------

# Determine mediante un analisis de varianza con las variables contenido de arcilla en el perfil clay 5 y el factor o tratamiento de zonas
Por.var <- aov(suelo$Clay5 ~ suelo$zone)
Por.var
summary(Por.var)  
TukeyHSD(Por.var)

# ¿Existen diferencias significativas entre el contenido de arcilla del perfil 30-50 y las zonas de estudio?
# De acuerdo a los resultados de la prueba de tukey, se observan diferencias significativas en los resultados de la comparacion entre el contenido de arcilla y las zonas de estudio

# En caso de existir diferencias ¿cuales zonas son diferentes estadisticamente entre si en el contenido de arcilla en el perfil 30-50cm?
# de acuerdo a la prueba de tukey:
# La zona 1 es diferente a la zona 3 y 4, 
# La zona 2 es diferente a la zona 3 y 4,
# La zona 3 es diferente a la zona 1, 2 y 4
# La zona 4 es diferente a la zona 1, 2 y 3

