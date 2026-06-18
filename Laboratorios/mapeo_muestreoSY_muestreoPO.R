library(sampling)
library(haven)
library(tidyverse)
library(sf)
library(tmap)

# Importamos el marco censal. Se encuentra en formato .sav.

marco=read_spss("Marco_2011_con_barrio_y_Sec_po_savl.sav")
View(marco)

# Filtramos el marco para el departamento de Montevideo.

mdeo=marco %>% filter(dpto=="01")
View(mdeo)

# El identificador de la manzana es la variable codcomp que está formada por la concatenación de los códigos de departamento, sección, segmento y zona censal. A modo de ejemplo el codcomp “0101001001” indica que es la zona “001” del segmento “001” de la sección “01” del departamento “01”, que es Montevideo.

# La variable que indica la cantidad de personas que viven en la zona es “P_TOT”. ¿Cuántas zonas vacías hay?

table(mdeo$P_TOT==0)
## 3160 zonas están vacías.

# ¿Dónde se encuentran las zonas vacías y las más pobladas? Para responder esta pregunta tenemos que hacer un mapa. Para ello importamos la cartografía digital de las zonas de Montevideo. El tipo de archivo es shp, y deben encontrarse en el directorio los 5 archivos que lo acompañan, si no la importación no se realiza o se realiza de forma incorrecta. Se leen con la función st_read() de la librería sf.

zmdeo=st_read("zonas_mdeo.shp")
View(zmdeo)

# Para visualizarlo usamos la librería tmap. La lógica de esta librería es similar a ggplot2. En este caso se tiene un archivo de tipo polígono. Para visualizar el mapa se debe indicar el objeto espacial con la función tm_shape() y luego indicar el tipo de archivo, en este caso tm_polygons().

tm_shape(zmdeo)+
  tm_polygons() # el segundo tm está definido segun lo que devuelva el codigo anterior en la linea "Geometry type: MULTIPOLYGON"

# Es la variable CODCOMP_A que es del tipo character. Hacemos el merge:

zmdeo=merge(zmdeo,mdeo,by.x="CODCOMP_A",by.y="codcomp")

# Verifico en el mapa. Le voy a indicar que mapee de acuerdo a los valores del “P_TOT”, y le voy a dar los cortes.

tm_shape(zmdeo)+
  tm_polygons("P_TOT",
              breaks=c(-Inf,0,100,500,1000,2000,3500),interval.closure="right", ## es necesario agregar el "-Inf"
              palette="Reds",
              id = "P_TOT")

## Para ver mejor el mapa
tmap_mode("view")

tmap_options(check.and.fix = TRUE) ## esto me tiró error pero no me impidió ver el mapa

tm_shape(zmdeo)+
  tm_polygons("P_TOT",alpha=0.5,
              breaks=c(-Inf,0,100,500,1000,2000,3500),interval.closure="right",
              palette="Reds",
              id = "P_TOT")+
  tm_basemap("OpenStreetMap") ## una especie de library extraida de google maps

# ¿Hay algo raro? Deberíamos considerar sólo las manzanas que tengan al menos un hogar particular.

zmdeo=zmdeo %>% filter(H_PAR!=0)

# Calculamos las probabilidades de inclusión para una muestra de 150 zonas, realizo una descriptiva de las probabilidades y se las pego al objeto zmdeo para poder visualizarlas luego en el mapa.

n=150
pik=inclusionprobabilities(zmdeo$P_TOT,n)
summary(pik)

zmdeo=cbind(zmdeo,pik)

# Selecciono la muestra con un πps sistemático:
  
set.seed(54787)
s1=UPsystematic(pik) ## muestreo sistematico !!
m1=zmdeo %>% filter(s1==1)

# Hago el mapa para visualizar la muestra.

tm_shape(m1)+
  tm_polygons(col="red",id="P_TOT",popup.vars="pik")+
  tm_basemap("OpenStreetMap")

# Con el método de Brewer

set.seed(54787)
s2=UPbrewer(pik)
m2=zmdeo %>% filter(s2==1)

# Hago el mapa para visualizar la muestra.

tm_shape(m2)+
  tm_polygons(col="red",id="P_TOT",popup.vars="pik")+
  tm_basemap("OpenStreetMap")

# Selecciono una tercera muestra con un diseño Poisson:

set.seed(54787)
s3=UPpoisson(pik)
m3=zmdeo %>% filter(s3==1)

# Visualizo la muestra:
  
tm_shape(m3)+
  tm_polygons(col="red",id="P_TOT",popup.vars="pik")+
  tm_basemap("OpenStreetMap")

