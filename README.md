# Datos_Hidrologicos

# datos hidrologicos ejercicio explorativo

inp <- read.csv("FDC.csv", na.strings = "")
inp <- read.csv("FDC.csv")

head(inp)
dim(inp)

inp[!complete.cases(inp),]

# newinp <- na.omit(inp)

plot(inp[,2], type = "l", col="blue")
lines(inp[,3], col="green")

summary(inp[,2:3])
hist(inp[,2])
hist(inp[,3])

names(inp) <- c("fecha","estrella","banano")
attach(inp)
plot(estrella)

Tempdate <- strptime(inp[,1], format= "%d/%m/%Y")

MAQ_estrella <- tapply(estrella, format(Tempdate, format="%Y"), FUN=sum)
MAQ_banano <- tapply(banano, format(Tempdate, format="%Y"), FUN=sum)

write.csv(rbind(MAQ_estrella,MAQ_banano), file="MAQ.csv")

plot(MAQ_banano, ylim=c(100,3000))
lines(MAQ_estrella, col=2)

MMQ_estrella <- tapply(estrella, format(Tempdate, format="%m"), FUN=sum)
MMQ_banano <- tapply(banano, format(Tempdate, format="%m"), FUN=sum)

write.csv(rbind(MMQ_estrella,MMQ_banano), file="MMQ.csv")

# Analisis de correlacion
corinp <- cor(inp[,2:3],method= "spearman")
plot(estrella, banano)

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)
summary(inp.lm)
plot(inp.lm)
