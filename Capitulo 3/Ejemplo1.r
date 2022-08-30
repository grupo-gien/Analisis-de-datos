# Ejercicio 1
# A (2,1,1,3)
# B (1,4,2,5,0,3)
# Calcular: (1) B'.A' (2) (A.B)' (3) Demostrar que B'.A' = (A.B)'

# R./
A = matrix(c(2,1,1,3),2,2,byrow=T)		# Estructura de comandos para las matrices 
A						# 2,2: dos filas y dos columnas.

B = matrix(c(1,4,2,5,0,3),2,3,byrow=T)	# byrow: ordenar valores por filas de la matriz.
B						# 2,3: dos filas y tres columnas

tA= t(A)		# t(A): Transpuesta de la matriz A
tA
tB= t(B)		# t(B): Transpuesta de la matriz B
tB
tB%*%tA == t(A%*%B)	  # %*%: para multiplicar matrices. 
#==: demostrar una igualdad.

#-------------
# Ejercicio 2
# A (2,3,3,2)
# B (1,4,2,5,0,3)
# Calcular: Determinante de A y de B

#R./
A = matrix(c(2,3,3,2),2,2,byrow=T) 		  # Matriz A
A
B = matrix(c(1,4,2,5,0,3),3,3,byrow=T) 	# Matriz B
B
det(A)		# det(A): Determinante de la matriz A
det(B) 		# det(b): Determinante de la matriz B


#------------
# Ejercicio 3
# A (5,2,2,2)
# Calcular inversa de A

# R./
A = matrix(c(5,2,2,2),2,2,byrow=T) 		  # Matriz A
A
solve(A)		# solve(A): Determinante de la matriz A




#------------
# Ejercicio 4
# A (5,2,2,2)
# Calcular valores y vectores propios de A

# R./
A = matrix(c(5,2,2,2),2,2,byrow=T) # Matriz A
A
eigen(A)    # Valores y vectores propios

# Ejercicios:
# 1. Extraer los valores y vectores propios por separado.
# 2. Extraer el primer vector fila y el primer vector columna, por separado del resto de insumos.


# R./ ***
eigen(A)$vectors
eigen(A)$vectors[,1] 
eigen(A)$vectors[1,] 


#------------
# Ejercicio 5
# Fila (observación) 1: 5,2
# Fila (observación) 2: 2,2
# Calcular la matriz de distancia euclidea entre las filas

# R./
A = matrix(c(5,2,2,2),2,2,byrow=T)  # Matriz A
A
dist(A)   # dist(A): Matriz de distancia euclídea



#------------
# Ejercicio 6
# Fila (observación) 1: 5,2
# Fila (observación) 2: 2,2
# Fila (observación) 3: 2,5
# Calcular la matriz de distancia euclídea entre las filas

# R./
A = matrix(c(5,2,2,2,2,5),3,2,byrow=T)   # Matriz A
A
dist(A)           # dist(A): Matriz de distancia euclídea
round(dist(A),1)  # dound(): Disminuye decimales del resultado

#------------
# Ejercicio 7
# Variable 1: 2,1,2
# Variable 2: 4,3,5
# Calcular las matrices de Covarianza y de Correlación

# R./
A = matrix(c(2,1,2,4,3,5),3,2,byrow=F) 
A
cov(A)
var(A)
cor(A)
round(cov(A),1)
round(cor(A),1)


# Calcular la matriz de distancia y analizar el resultado.
dist(A)
round(dist(A),1)