set.seed(29842)

x1 <- rnorm(100, 15, 3)
x2 <- x1 + rnorm(100, 0, 2)
x3 <- rnorm(100, -20, 4)
x4 <- x3 * runif(1, -1, 0) + rnorm(100, 0, 3)

beta.0 <- runif(3, -3, 3)

beta.1.1 <- runif(3, -2.5, 2.5)
beta.1.2 <- runif(3, -4, 4)

beta.2.1 <- runif(3, 1, 2)

library(foreach)
y <- foreach(i = 1:3, .combine = cbind) %do% {
      beta.0[i] + beta.1.1[i] * x1 + beta.1.2[i] * x1^2 + 
            beta.2.1[i] * x3 + rnorm(100, 0, 100)
}


y <- data.frame(y)
names(y) <- c("y1", "y2", "y3")

df.1 <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, y)
df.1 <- round(df.1, 1)

df.mel <- reshape2::melt(df.1, id.vars = c("x1", "x2", "x3", "x4"))
names(df.mel) <- c("x1", "x2", "x3", "x4", "Tratamiento", "y")

df.mel$y <- with(df.mel, (y + min(y))/(max(y) - min(y)))
levels(df.mel$Tratamiento) <- c("A", "B", "C")

# Análisis exploratorio de variables independientes

##Visual

library(ggplot2)

ggplot(df.mel) + geom_point(aes(x = x1, y = x2)) + 
      geom_smooth(aes(x = x1, y = x2), method = "lm")
ggplot(df.mel) + geom_point(aes(x = x1, y = x3))+ 
      geom_smooth(aes(x = x1, y = x3), method = "lm")
ggplot(df.mel) + geom_point(aes(x = x1, y = x4))+ 
      geom_smooth(aes(x = x1, y = x4), method = "lm")

ggplot(df.mel) + geom_point(aes(x = x2, y = x3))+ 
      geom_smooth(aes(x = x2, y = x3), method = "lm")
ggplot(df.mel) + geom_point(aes(x = x2, y = x4))+ 
      geom_smooth(aes(x = x2, y = x4), method = "lm")

ggplot(df.mel) + geom_point(aes(x = x3, y = x4))+ 
      geom_smooth(aes(x = x3, y = x4), method = "lm")

## Modo rápido pero feito
df.x <- subset(df.mel, select = names(df.mel)[1:4])
pairs(df.x)

## Pruebas de correlación
cor.test(x1, x2, data = df.x)
cor.test(x1, x4, data = df.x)
cor.test(x3, x4, data = df.x)

# Análisis exploratorio de la relación con la variable dependiente 
ggplot(df.mel) + geom_point(aes(x = x1 , y = y, colour = Tratamiento))
ggplot(df.mel) + geom_point(aes(x = x2 , y = y, colour = Tratamiento))
ggplot(df.mel) + geom_point(aes(x = x3 , y = y, colour = Tratamiento))
ggplot(df.mel) + geom_point(aes(x = x4 , y = y, colour = Tratamiento))
ggplot(df.mel) + geom_point(aes(x = x3*x1 , y = y, colour = Tratamiento))

m1 <- lm(y ~ Tratamiento*(x1 + I(x1^2) + x3), df.mel)
summary(m1)
anova(m1)

plot(m1)

m2 <- lm(y ~ Tratamiento + Tratamiento:(x1 + I(x1^2) + x3), df.mel)
summary(m2)
anova(m2)

plot(m2)

anova(m1, m2)

# Sin término al cuadrado
m3 <- lm(y ~ Tratamiento + Tratamiento:(x1 + x3), df.mel)
summary(m3)
anova(m3)

plot(m3)

anova(m1, m3)
AIC(m1, m3)

# Porbaremos x4 en lugar de x3
m4 <- lm(y ~ Tratamiento*(x1 + I(x1^2) + x4), df.mel)
summary(m4)
anova(m1, m4)

AIC(m1, m4)

# Interacción entre x1 y x3
m5 <- lm(y ~ Tratamiento*(x1*x3 + I(x1^2)), df.mel)
summary(m5)
anova(m1, m5)
AIC(m1, m5)

# Sustituyendo x1 con x2 en m1
m6 <- lm(y~Tratamiento*(x2 + I(x2^2) + x3), df.mel)
summary(m6)
anova(m1, m6)
AIC(m1, m6)

# Conclusión, m1 puede ser el mejor, podemos mejorarlo?
anova(m1)
summary(m1)

# Podemos eliminar interacción x3
m1.1 <- lm(y ~ x3 + Tratamiento * (x1 + I(x1^2)), df.mel)
summary(m1.1)
anova(m1.1)

anova(m1, m1.1)
AIC(m1, m1.1)

plot(m1.1)

m1.2 <- lm(y ~ Tratamiento * (x1 + I(x1^2)), df.mel)
anova(m1.2)
summary(m1.2)

AIC(m1.2, m1)
