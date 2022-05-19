#####################################################################
# UCU 2020                                                                                                                   #
# Curso: DATA SCIENCE                                                                                              #
# Script: CLASE 8 MODELO DE REGRESION LINEAL SIMPLE                                # 
####################################################################


# Descargamos el dataset Publicidad.xlsx desde Webasignatura
# Importamos el dataset Publicidad.xlsx a RStudio:

# File > Import Dataset > From Excel...

# La base de datos Publicidad.xlsx consiste en las ventas de un producto en 200 mercados diferentes
# junto con presupuestos publicitarios para el producto en cada uno de esos mercados para tres medios
# diferentes: TV, Radio y Diarios
# Las variables son:
#   VENTAS: ventas del articulo, expresadas en miles de unidades. Es la varable a predecir (Y)
#   ID: representa uno de los mercados donde se comercializa el producto
#   TV: gasto en publicidad en television , expresado en miles de $
#   RADIO: gasto en publicidad en RADIO, expresado en miles de $
#   DIARIOS: gasto en publicidad en DIARIOS, expresado en miles de $


# Usaremos las técnicas de Regresión Lineal en estos datos. 
# Realizamos primero un Analisis Exploratorio preliminar de los datos

# Estos comandos ayudan a visualizar el tipo de datos que tenemos en la base de datos
View(Publicidad)

# Vemos la distribución de los datos
par(mfrow=c(2,2))   # Para poder ver varios gráficos al mismo tiempo. El primer     
# argumento c(,) 
# son las filas y el segundo la cantidad de columnas en las que lo 
# queremos dividir.  

# Histogramas
hist(Publicidad$TV, xlab="TV", main="Gastos en TV" , col="red")
hist(Publicidad $RADIO, xlab="Radio", main="Gastos en Radio", col ="red")
hist(Publicidad$DIARIOS,xlab="Diarios", main="Gastos en Diarios", col ="red")
hist(Publicidad$VENTAS,xlab="Ventas", main="Ventas en miles de unidades", col ="red")

# Diagramas de caja
boxplot(Publicidad$TV, xlab="TV", main="Gastos en TV" , col="red")
boxplot(Publicidad $RADIO, xlab="Radio", main="Gastos en Radio", col ="red")
boxplot(Publicidad$DIARIOS,xlab="Diarios", main="Gastos en Diarios", col ="red")
boxplot(Publicidad$VENTAS,xlab="Ventas", main="Ventas en miles de unidades", col ="red")

# Indicadores Estadisticos 
summary(Publicidad) # La variable X representa el ID de las observaciones 
summary(Publicidad[,-1]) # Eliminamos ID del resumen de indicadores estadisticos

# Matriz de correlación 
# Observamos la correlacion entre la variable a predecir (Y:VENTAS) y las predictoras (medos de publicidad: Xs)
cor(Publicidad[,-1])

# Gráficos de dispersión entre la variable a predecir (VENTAS) y cada uno de los 3 medios de publicidad
par(mfrow=c(1,3))  

plot(Publicidad$RADIO, Publicidad$VENTAS, xlab="RADIO", ylab="Ventas", col = "deeppink2")
abline(lm(Publicidad$VENTAS~Publicidad$RADIO), col="blueviolet", lwd=2)   
# Para incluir la recta de regresión lineal utilizo el comando abline
# incluye la fórmula para la regresión lineal en RStudio:  Y ~ X 

plot(Publicidad$RADIO, Publicidad$VENTAS,xlab="Radio", ylab="Ventas", col = "red")
abline(lm(Publicidad$VENTAS~ Publicidad$RADIO), col="blue", lwd=2)

plot(Publicidad$DIARIOS, Publicidad$VENTAS,xlab="Diarios", ylab="Ventas", col = "red")
abline(lm(Publicidad$VENTAS~Publicidad$DIARIOS), col="blue", lwd=2)


par(mfrow=c(1,1)) # Volvemos a la configuración inicial. 

# MODELO DE REGRESION LINEAL SIMPLE

# Separamos a la base de datos en testing y training data 
# Funcion:  sample()

# Utilizamos el 70% de los datos para entrenar al modelo (train)  y el restante 30% para evaluar (test) 

set.seed(1111)   # Fijamos una "semilla" para que los resultados aleatorios sean reproducibles

# TRAIN: Base de entrenamiento 
train = sample(nrow(Publicidad),nrow(Publicidad)*0.7) # Elegimos al azar el 70% de los datos (140 de 200)

# TEST: Base de prueba
test  <- (-train)  

# Estimación del Modelo de Regresion Lineal Simple
# VENTAs en funcion d elos gastos en TV
# Se busca predecir las ventas con el presupuesto de gastos en TV

#  Sintaxis:  lm(FORMULA, DATOS, OPCIONES)
#  donde FORMULA= y ~ x 
#                "y es la variable a predecir, X es la independente"  
ModeloR =lm(VENTAS~RADIO, data= Publicidad, subset=train) # Los resultados se guardan en el elemento modelo1


# ESTIMACIONES de los Parametros del modelo

# Los resultados se guardan en el elemento que nombramos modelo1. Para ver los parametros estimados, podemos 
# escribir el nombre del elemento que creamos:
ModeloR

# Para ver toda la informacion QUE guarda el comando lm() usamos names():
names(ModeloR)


# Tambien podemos ver los coeficientes estimados escribiendo:
coef(ModeloR)
ModeloR$coefficients  

# El comando summary() muestra un resumen del modelo ajustado
summary(Modelo) 

#Intervalo de confianza del 95% para los parametros del modelo 
confint(ModeloR) 


# PREDICCIONES de Y (VENTAS)

# Creamos las variable predicha por el modelo en la base de datos original:

Publicidad$VENTAS_pred = predict(ModeloR, newdata=Publicidad)

# Podemos predecir para valores que no estan en la base de datos original
# Creamos por ejemplo una base de datos con solo 5 valores para la variable TV

data_nuevos = data.frame(RADIO=(c(5,100, 1000, 1500, 20000)))  
predict(ModeloR, data_nuevos)   


# Evaluamos la calidad del Modelo en la base de TEST 

# Creamos un vector de valores que contiene las predicciones para los datos de TEST

pred.mco = predict(ModeloR,newdata=Publicidad[test,])

# Calculo el ECM (error cuadratico medio)

ECM.mco.test= mean((pred.mco-Publicidad$VENTAS[test])^2)
ECM.mco.test

