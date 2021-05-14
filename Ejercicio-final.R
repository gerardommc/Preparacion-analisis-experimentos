#Generación de datos de ejercicio final

set.seed(12345)

x1 <- rnorm(100, 15, 3)
x2 <- rnorm(100, -20, 4)
x3 <- x2 + rnorm(100, 0, 2)
x4 <- rnorm(100, 25, 3)

beta.0 <- runif(3, -3, 3)

beta.1.1 <- runif(3, -2.5, 2.5)
beta.1.2 <- runif(3, -4, 4)

beta.2.1 <- runif(3, 1, 2)

beta.4.1 <- runif(3, 1, 1)

beta.4.1.1 <- runif(3, -2, 2)

library(foreach)
y <- foreach(i = 1:3, .combine = cbind) %do% {
      beta.0[i] + beta.1.1[i] * x1 + beta.1.2[i] * x1^2 + 
            beta.2.1[i] * x2 + beta.4.1[i] * x4 + beta.4.1.1[i] * x4 * x1
}
y <- data.frame(y)
names(y) <- c("y1", "y2", "y3")

df.1 <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, y)
df.1 <- round(df.1, 1)

df.mel <- reshape2::melt(df.1, id.vars = c("x1", "x2", "x3", "x4"))
names(df.mel) <- c("x1", "x2", "x3", "x4", "Tratamiento", "y")

df.mel$y <- with(df.mel, (y + min(y))/(max(y) - min(y)))
levels(df.mel$Tratamiento) <- c("A", "B", "C")

library(ggplot2)

ggplot(df.mel) + geom_point(aes(x = x2 , y = y, colour = Tratamiento))

ggplot(df.mel) + geom_point(aes(x = x1, y = x4, colour = Tratamiento))

write.csv(df.mel, "Unidad 5-Regresion correlacion/Ejercicio-Regresion-ANCOVA.csv")

saveRDS(list(beta.0 = beta.0, 
             beta.1.1 = beta.1.1,
             beta.1.2 = beta.1.2,
             beta.2.1 = beta.2.1,
             beta.4.1 = beta.4.1,
             beta.4.1.1 = beta.4.1.1),
        "Unidad 5-Regresion correlacion/Coeficientes-regresión.rds")
