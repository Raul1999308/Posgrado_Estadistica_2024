library(repmis)

erupciones <- source_data("https://www.dropbox.com/s/liir6sil7hkqlxs/erupciones.csv?dl=1")
summary(erupciones)

plot(erupciones$waiting,erupciones$eruptions,xlab="Tiempo de espera entre erupciones (min)",
     ylab="Duración de las erupciones (min)",pch=19, col= "blue")


# correlacion
cor.test(erupciones$waiting, erupciones$eruptions, xlab = "tiempo de esperan entre erupcion (min)",
         ylab = "duracion de las erupciones (min)", pch = 19, col = "red")

# Obtencion de alfa y beta ------------------------------------------------

ed.lm <- lm(erupciones$eruptions ~ erupciones$waiting)
ed.lm


# significancia de los datos ----------------------------------------------

symmary(ed.lm)

plot(erupciones$waiting, erupciones$eruptions, xlab = "tiempo de espera erupciones (min)", 
     ylab = "duracion de las erupciones (min)", pch =19)
abline(ed.lm, col = "red")
text(1,80,)
