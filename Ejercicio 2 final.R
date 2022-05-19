# UCU 2020                 
# Proyecto final DATA SCIENCE
# Integrantes: Bruno Cabrera, Tomás Manggiarotti, Sol Preusse, Lucía Raimondo y Carolina Savio
#############################################################################################

#### Ejercicio 2 ####

library(ggplot2)

#Tablas de frecuencia

Proyecto$Nivel.SE.Alto = ifelse(Proyecto$Nivel.SE == 'Alto', 1, 0)
table(Proyecto$Nivel.SE.Alto, Proyecto$Nivel.SE)

tabla = table(Proyecto$Nivel.SE.Alto)
TABLA_nivelSE = cbind(tabla,round(prop.table(tabla)*100,1))
colnames(TABLA_nivelSE) <- c('Frecuencia','Porcentaje %')
TABLA_nivelSE

#Diagrama de caja

boxplot(Proyecto$Ingreso ~ Proyecto$Nivel.SE.Alto, xlab="Nivel socioeconómico alto", ylab="Ingreso", main = 'Ingresos vs Nivel socioeconómico alto', cex.main=1, col="dodgerblue")
boxplot(Proyecto$Ant.Lab ~ Proyecto$Nivel.SE.Alto, xlab="Nivel socioeconómico alto", ylab="Antiguedad laboral", main = 'Antiguedad laboral vs Nivel socioeconómico alto', cex.main=1, col="dodgerblue")
boxplot(Proyecto$Hab ~ Proyecto$Nivel.SE.Alto, xlab="Nivel socioeconómico alto", ylab="Habitantes del hogar", main = 'Habitantes del hogar vs Nivel socioeconómico alto', cex.main=1, col="dodgerblue")
boxplot(Proyecto$Edu.Ter ~ Proyecto$Nivel.SE.Alto, xlab="Nivel socioeconómico alto", ylab="Educacion Terciaria", main = 'Educacion Terciaria vs Nivel socioeconómico alto', cex.main=1, col="dodgerblue")

#Histogramas

qplot(Proyecto$Nivel.SE.Alto, geom="histogram", xlab="Nivel Socio Economico Alto", ylab="Frecuencias", main="Nivel Socio Economico Alto", fill=I("cornflowerblue"), 
      col=I("blue"),) 


# Modelizacion

# Datos en training y testing
set.seed(1111)   
train <- sample(nrow(Proyecto), nrow(Proyecto)*0.7) 
test <- (-train)

# Variables dummy: Proyecto_dummy 
Proyecto$Nivel.SE.Alto = ifelse(Proyecto$Nivel.SE == 'Alto', 1, 0)
table(Proyecto$Nivel.SE.Alto, Proyecto$Nivel.SE)

modelo_log1 = glm(Ingreso ~ Nivel.SE.Alto, data = Proyecto, subset=train, family = "binomial")
summary(modelo_log1)


modelo_log2 = glm(Nivel.SE.Alto ~ Ingreso+ Ant.Lab+ Edu.Ter+Hab, data = Proyecto, subset=train, family = "binomial")
summary(modelo_log2)

# ARBOL

library(rio) 
library(rpart) 
library(rattle) 
library(corrplot)

# Analsis preliminar exploratorio de la base
addmargins(table(Proyecto$Ingreso, Proyecto$Nivel.SE.Alto))
addmargins(prop.table(table(Proyecto$Ingreso, Proyecto$Nivel.SE.Alto), 1), 2)*100

addmargins(table(Proyecto$Edu.Ter, Proyecto$Nivel.SE.Alto))
addmargins(prop.table(table(Proyecto$Edu.Ter, Proyecto$Nivel.SE.Alto), 1), 2)*100

par(mfrow = c(1, 3))
boxplot(Proyecto$Ingreso ~ Proyecto$Nivel.SE.Alto, col = 'gold', main= 'Nivel SE alto vs. Ingreso')
boxplot(Proyecto$Edu.Ter ~ Proyecto$Nivel.SE.Alto, col = 'gold', main= 'Nivel SE alto vs. Educacion Terciaria')
boxplot(Proyecto$Ant.Lab ~ Proyecto$Nivel.SE.Alto, col = 'gold', main= 'Nivel SE alto vs. Antiguedad Laboral')

#Semilla

set.seed(1111)   
train <- sample(nrow(Proyecto), nrow(Proyecto)*0.5) 
test <- (-train)

# Arbol de clasificacion training
par(mfrow = c(1, 1))

# Elimino la variable ID
Arbol1 = rpart(Nivel.SE.Alto ~ . , Proyecto[train, -c(1) ], method = 'class')
Arbol1

# Representacion grafica
fancyRpartPlot(Arbol1)

plotcp(Arbol1)

#Simplificacion
Arbol2 <- prune(Arbol1, cp = 0.028)
fancyRpartPlot(Arbol2)
