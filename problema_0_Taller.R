# Crear un dataframe con los datos
setwd("C:/Users/HP/Downloads")
datos <- read.csv("problema0.csv",header=T,sep=",")
plot(datos)

# Realizar el análisis de varianza de dos vías
modelo_lm=lm(sobrevida ~ veneno + tratamiento, data = datos)
summary(modelo_lm)
modelo_anova <- aov(sobrevida ~ veneno + tratamiento, data = datos)

# Mostrar los resultados
summary(modelo_anova)

model <- aov(sobrevida ~ veneno + tratamiento, data=datos)
plot(TukeyHSD(model, conf.level=.95), las = 2,col="darkgreen")

modelo_tuk=HSD.test(model, trt = 'tratamiento')

modelo_interactivo=lm(sobrevida ~ veneno * tratamiento, data = datos)
summary(modelo_interactivo)
anova_interactivo= aov(sobrevida ~ veneno * tratamiento, data = datos)
tukey_interactivo=aov(sobrevida ~ veneno * tratamiento, data=datos)
plot(TukeyHSD(model, conf.level=.95), las = 2,col="darkred")
