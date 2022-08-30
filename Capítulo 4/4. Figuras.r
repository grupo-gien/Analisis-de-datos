# Librerías requeridas
library(lattice)
library(ellipse)
require(SciViews)
library(plotrix)
require(stats)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(gtable)
library(grid)
library(ggforce)

#------------------
# Base de datos de insectos acuáticos

datos<-read.csv2("Datos1.csv",row.names=1)

# Organización de los datos
str(datos)      # Estructura de la base de datos
datos$cuenca=as.factor(datos$cuenca)  # Convertir cuenca a factor
str(datos)        # Nueva estructura de los datos
summary(datos[,3:9])  # Resumen estadístico

#------------------
# 1. Gráfica por pares
pairs(datos[,3:9])        # [,3:9] relaciona a las columnas 2 a la 8.
pairs(log10(datos[,3:9])) # log10 es la transformación logarítmica

# 2. Figuras de pares con curvas de ajuste
pairs ((datos[,c(2:9)]),panel=function(x,y)
{abline(lsfit(x,y)$coef,lwd=2,col=3)
  lines(lowess(x,y),lty=2,lwd=2,col=2)
  points(x,y,cex=1)})

# 3. Pares con "cuenca" como un factor
pairs ((datos[,c(2,6,7,9)]),panel=function(x,y)
{abline(lsfit(x,y)$coef,lwd=2,col=3)
  lines(lowess(x,y),lty=2,lwd=2,col=2)
  points(x,y,col=datos$cuenca, cex=1.4)})

# 4. Pares con coeficientes de Pearson
pairs(datos[, 2:9], diag.panel = panel.hist, 
      upper.panel = panel.cor, lower.panel = panel.smooth)

#------------------
# 5.Elipses en escalas de grises 
plotcorr(cor(datos[,2:9]))

# 6. Especies de insectos
M <- cor(datos[,2:9])            # Matriz de Correlación (M)
x11()                            # Panel gráfico adicional
corrplot(M, method = "ellipse")  # Figura de correlaciones con elipses
corrplot(M, method = "circle")   # Figura de correlaciones con círculos
corrplot.mixed(M, upper="ellipse")


#------------------
# 7. Figura con tres variables (Función: coplot)
with(datos,coplot(Efem~pH|temp))

# Coplot con líneas de ajuste suavizado (loess)
with(datos, {
  coplot(Efem~pH|temp, number = 3,
         panel = function(x, y, ...) panel.smooth(x, y, span = .8, ...))
  coplot(Efem~pH|temp,
         panel = panel.smooth)})


# 8. Coplot con categórias 
summary(datos[,3:9])

clasetemp<-cut(datos$temp,seq(15,20,1.2),include.lowest=T)
clasetemp
clasepH<-cut(datos$pH,seq(5,8,1,include.lowest=T))
clasepH

panel.lm = function(x, y, ...) {
  tmp<-lm(y~x,na.action=na.omit)
  abline(tmp, lwd = 1.5, col= 2)
  points(x,y, ...)}

coplot(Efem~pH | clasetemp, pch=19, panel = panel.lm, data=datos)

# 9. Splom para variables categorizadas
splom(~datos[,4:8]|clasepH,pscales=0)   
splom(~datos[,4:8]|clasepH+clasetemp,pscales=0)

# 10. xyplot para para cada cuenca
xyplot(Efem~pH|cuenca,data=datos)


#------------------
# 11. Histogramas 

ggplot(datos, aes(x=Ab)) +
  geom_histogram(bins = 12, color="black", fill="white") +
  labs( y="Frecuencia", x="Abundancia de insectos", 
        title="Histograma de Abundancias")


# Histograma por tipos de cuencas
ggplot(datos, aes(x=Ab, fill=cuenca))+
  geom_histogram(color="black")+
  facet_wrap(~cuenca)


# Figura de densidad por tipos de cuencas
ggplot(data = datos, aes(x = Ab, color = cuenca)) +
  geom_density(aes(fill = cuenca)) +
  labs( y="Frecuencia", x="Abundancia")

ggplot(data = datos, aes(x = Ab, color = cuenca)) +
  geom_density(aes(fill = cuenca), alpha = 0.5) +
  labs( y="Frecuencia", x="Abundancia")