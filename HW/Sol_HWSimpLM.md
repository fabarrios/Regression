## Regression in biostatistics, 2024  
## F.A. Barrios  
### Regresión y Correlación. En todos los problemas explique sus resultados y discuta los niveles de confianza estadística, e intervalos de confianza.  

1.	Metadona es comúnmente usada en el tratamiento de adicción a opiáceos y dolor crónico. Krantz et al. estudiaron la relación entre dosis de metadona y el intervalo QT corregido (QTc) de 17 pacientes que desarrollaron taquicardia ventricular en repuesta a los medicamentos. QTc se calcula a partir del electrocardiograma y se mide en mm/s. Un valor mayor de QTc indica un riesgo mayor de mortalidad cardiovascular. Una pregunta de interés es saber si se puede predecir y estimar el valor de QTc a partir de la dosis de metadona. Por lo que se considerará la variable repuesta los datos de QTc y dosis de metadona como la variable independiente de los datos en EXR_C09_S03_03.csv.  
2.	Evans et al. examinaron el efecto de la velocidad en la fuerza de reacción del suelo (GRF) en perros que cojeaban a cause del ligamento cruzado rasgado. Los perros caminaban y corrían sobre una plataforma de fuerza que medía la fuerza de respuesta (GRF) en Newtons. La tabla en EXR_C09_S03_06.csv contiene 22 medidas de fuerza resultado del promedio de 5 tomas de fuerza por perro cuando camina y de 5 tomas del perro corriendo. use los valores de respuesta al caminar para relacionar los valores de respuesta al trotar (trotar es la variable respuesta).  
3.	En una muestra aleatoria simple de niños sanos entre 6 meses y 15 años de edad, resultó en datos de edad (X) y volumen del hígado por unidad de peso corporal (ml/kg) Y. con los datos de la tabla en EXR_C09_S07_06.csv, que se puede concluir que estas variables están correlacionadas?  
4.	En la tabla REV_C09_23.csv se incluyen los pesos (kg) y niveles de glucosa en sangre (mg/100 ml) de 16 aparentemente hombres adultos. Estime si existe un modelo lineal de las variables y determine el intervalo de confianza al 95% de su coeficiente de correlación. Cuan es el valor predicho de nivel de glucosa en sangre para un hombre de 95 kg de peso. al 95% de confianza?  
5.	Gelb et al. condujeron un estudio en el que exploraron la relación entre limitación moderada a severa de flujo de aire espiratorio en la presencia y extensión de morfología y CT calificado (calificado por Tomografía Computarizada) enfisema en pacientes ambulatorios evaluados consecutivamente con Enfermedad Pulmonar Obstructiva Crónica (EPOC). Los datos en REV_C09_36.csv muestran resultados de calificación de CT pulmonar y patología (PATH) de enfisema. A que conclusión se puede llegar con estos datos?  
  
# Soluciones Simple linear regression and correlation homework
```{r message = FALSE}
# Simple Linear Regression and correlations
# Packages setup (loading libraies)
library(tidyverse)
library(psych)
library(ggm)

# posthoc
library(emmeans)
library(multcomp)
```
Correlations (from R in Action, R.I.K). 
The Pearson product-moment correlation assesses the degree of linear relationship between two quantitative variables. Spearman’s rank-order correlation coefficient assesses the degree of relationship between two rank-ordered variables. Kendall’s tau is also a nonparametric measure of rank correlation.  
THe R function is `cor()` and the `cov()` functions produces covariances. With similar inputs.  
Examples using the state.x77 database from the 50 US states in 1977, and packages `psych` and `ggm`.  

```{r} 
states<- state.x77[,1:6]
cov(states)
cor(states)
cor(states, method="spearman")
```

## Problem 3.  
Loading data from the exercise 6 in chapter 9 of Daniel, we will estimate the different correlations types and estimates for significance of different methods.  

```{r}
Exr09.07_p06 <- read_csv("DataSets/ch09_all/EXR_C09_S07_06.csv",
                 show_col_types = FALSE)

```