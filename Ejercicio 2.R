# EJERCICIO 2
# a) Prueba de Hipotesis para un promedio:

t.test(ausentismo$Edad, mu=45, conf.level = 0.99)

# Ho: mu Edad = 45
# H1: mu Ausentismo distitnto a 45
# Nivel de significacion: 1%


t.test(ausentismo$Edad, mu=40, conf.level = 0.90)

# Ho: mu Edad = 40
# H1: mu Ausentismo distitnto a 40
# Nivel de significacion: 10%

