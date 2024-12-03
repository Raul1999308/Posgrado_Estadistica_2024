datos <- data.frame(diam_arboles, altura_arboles)

# codigo prgunta 15 y 16 --------------------------------------------------

set.seed(24)
n <- 30
altura <- rnorm(30, mean = 170, sd = 10)
peso <- 0.5 * altura + rnorm(n, mean = 0, sd = 5)
t.test(altura, peso)


# codigo pregunta 17 y 18 -------------------------------------------------

set.seed(42)
n <- 30
altura <- rnorm(n, mean = 170, sd = 10)
peso <- 0.5 * altura + rnorm(n, mean = 0, sd = 5)
t.test(altura, peso)

# codigo 19 y 20 ----------------------------------------------------------

set.seed(25)
n <- 40
altura <- rnorm(n, mean = 20, sd = 5)
peso <- 1.5 * altura + rnorm(n, mean = 0, sd = 0)
t.test(altura, peso)

