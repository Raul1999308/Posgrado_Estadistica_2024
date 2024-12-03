# correlacion de datos

library(repmis)
edad <- source_data("https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1")

summary(edad)

plot(edad$DAP, edad$EDAD, pch = 19, col  = "indianred",
     xlab = "Diamentro (cm)",
     ylab = "Edad", 
     ylim = c(20,140), 
     xlim = c(10,50))
abline(ed.lm)
text(20,120, "y = -8.4 * 2.43(x)")

ed.lm$coefficients
ed.lm$residuals
edad$res <- ed.lm$residuals
edad$edprim <- ed.lm$fitted.values
edad$com.res <- edad$EDAD ~ edad$edprm
sum(edad$res)
sum(edad$res^2/58)

# estimar la edad (prima) para los valores de DAP: 15,30
valores <- c(15, 30, 45, 47)
prima <- -8.4 + 2.4*(valores)

cor.test(edad$DAP, edad$EDAD)

ed.lm <- lm(edad$EDAD ~ edad$DAP)

# solo obtener el intercepto (alfa) y beta
ed.lm

# para obtener la significacia aplico sumary
summary(ed.lm)



