library(ggplot2)

df.xy <- expand.grid(x = 1:4, y = 1:4)
df.xy$Tratamiento <- c("A", "B", "C", "D", 
                 "D", "A", "B", "C",
                 "C", "D", "A", "B",
                 "B", "C", "D", "A")
df.xy$bloque <- 1:16

df.xy$N <- rpois(16, 50)

png("Diseño-1.png", width = 500, height = 400)
ggplot(df.xy) + geom_tile(aes(x = x, y = y, fill = Tratamiento)) +
      geom_text(aes(x = x, y = y, label = N)) +
      theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
dev.off()

df.na <- df.xy
df.na$Tratamiento[sample(1:16, 5)] <- NA
df.na$N <- rpois(16, 20)
df.na$N[is.na(df.na$Tratamiento)] <- 0

png("Diseño-2.png", width = 500, height = 400)
ggplot(df.na) + geom_tile(aes(x = x, y = y, fill = Tratamiento)) +
      geom_text(aes(x = x, y = y, label = N))+
      theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
dev.off()

#Diselo de fisher con dos bloques

Trats <- expand.grid(x = seq(2, 20, by = 2), y = seq(1, 10, by = 1))

Trats$Tratamiento <- rep(c("A", "B", "B", "A"), 25)

ggplot(Trats) + geom_point(aes(x = x, y = y, colour = Tratamiento), size = 3) +
      labs(x = "", y = "") +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())

##

sp.plot <- rbind(df.xy, df.xy, df.xy, df.xy)
edad <- c(rep(c("N", "J"), 4), rep(c("S-Ad", "Ad"), 4))

sp.plot$Edad <- c(edad, edad, edad, edad)

x1 <- seq(0.75, 4.25, len = 8)
y1 <- seq(0.75, 4.25, len = 8)

df.xy1 <- expand.grid(y1 = y1, x1 = x1)

sp.plot <- cbind(sp.plot, df.xy1)

png("Diseño-3.png", width = 500, height = 400)
ggplot(sp.plot) + geom_tile(aes(x = x, y = y, fill = Tratamiento)) +
      geom_text(aes(x = x1, y = y1, label = Edad), size = 4) +
      theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
dev.off()

# Random block

df.xy2 <- expand.grid(x = 1:10, y = 1:10)

source("Random-functions/Shuffle.R")

trat <- rep(LETTERS[1:10], 10)
trat <- trat[shuffle(length(trat))]

df.xy2$Tratamiento <- trat


png("Diseño-4.png", width = 600, height = 600)
ggplot(df.xy2) + geom_tile(aes(x = x, y = y, fill = Tratamiento)) +
      geom_text(aes(x = x, y = y, label = Tratamiento), size = 5) + 
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
dev.off()





