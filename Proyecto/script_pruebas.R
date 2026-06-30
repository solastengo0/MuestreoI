#PROYECTO MODELOS
library(ggplot2)

View(riv)

## Exportar riv a Excel

# install.packages("writexl")
# library(writexl)
# write_xlsx(riv, path = "riv.xlsx")


## Jerarquía de identificación de la base:

# Jerarquía de la base:

# SEGMENTO -> DIRECCION_ID -> VIVID -> HOGID -> PERID

# ID_CENSO: identificador único de cada persona.
# ID_HOGAR: identificador único de cada hogar.

# La base se encuentra a nivel persona, por lo que varias filas pueden compartir el mismo ID_HOGAR.

# El tamaño de un hogar puede obtenerse como la cantidad de registros asociados a un mismo ID_HOGAR.

# HOGCE13(Nº): Cantidad de automóviles o camionetas

#---------------------------
# Códigos genéricos utilizados en la base:

# 7777 = No corresponde
# 8888 = No relevado
# 9898 = Ignorado
# 99 / 9999 = No recuerda

# Estos códigos representan respuestas faltantes o especiales y
# no deben interpretarse como valores reales de las variables.
# Se tendrá especial cuidado al analizarlas y resumirlas.
#---------------------------

summary(riv$HOGCE13)
table(riv$HOGCE13, useNA = "always")

table(riv$ESTRATO, useNA = "always")

## Extraer un data frame reducido de la base original unicamente con las variables que nos interesan:


datos <- riv[, c(
  "ID_CENSO",
  "ID_HOGAR",
  
  "DIRECCION_ID",
  "VIVID",
  "HOGID",
  
  "SEGMENTO",
  "SECCION",
  "LOCALIDAD",
  "AREA",
  "REGION_4",
  
  "BARRIO85",
  "CCZ",
  "MUNICIPIO_136",
  
  "VIVVO00",
  
  "HOGTE01",
  "HOGTE02",
  "HOGTE03",
  "I_HOGTE02",
  "I_HOGTE02_TIPO",
  
  "HOGSH01",
  "HOGSH02",
  "HOGSH03",
  "I_HOGSH03",
  "I_HOGSH03_TIPO",
  
  "HOGSC01",
  "HOGSC02",
  
  "HOGHD00",
  "HOGHD01",
  
  "HOGCA01",
  "HOGRS01",
  
  "HOGCE03",
  "HOGCE04",
  "HOGCE11",
  "HOGCE12_1",
  "HOGCE13",
  "HOGCE17",
  "HOGCE17_1",
  "HOGCE22",
  "HOGCE26",
  "HOGCE27",
  "HOGCE28",
  
  "HOGMA01_1",
  "HOGMA01_1_1",
  "HOGMA01_2",
  "HOGMA01_2_1",
  
  "HOGPR01",
  "HOGPR01_1",
  "HOGPR01_2",
  "HOGPR01_3"
)]


datos$HOGCE13 <- dplyr::na_if(datos$HOGCE13, 99)
datos$HOGCE13 <- dplyr::na_if(datos$HOGCE13, 7777)
datos$HOGCE13 <- dplyr::na_if(datos$HOGCE13, 8888)
datos$HOGCE13 <- dplyr::na_if(datos$HOGCE13, 9898)
datos$HOGCE13 <- dplyr::na_if(datos$HOGCE13, 9999)

# Verificación de consistencia de la variable de interés

datos %>%
  group_by(ID_HOGAR) %>%
  summarise(n_val = n_distinct(HOGCE13)) %>%
  count(n_val)

# Se verifica que HOGCE13 es constante dentro de cada hogar.
# Por lo tanto, puede construirse una base a nivel hogar
# conservando una única fila por ID_HOGAR.

datos <- datos %>%
  distinct(ID_HOGAR, .keep_all = TRUE)

nrow(datos)

#Verificamos que todos los códigos especiales desaparecieron

table(datos$HOGCE13, useNA = "ifany")

cant_NA = sum(is.na(datos$HOGCE13))

# 1. Calcular el total poblacional de la variable de objeto de estudio

total = sum(datos$HOGCE13, na.rm = TRUE)

# 2. Determinar cual es el diseño más eficiente. n=1500

N <- nrow(datos)
n <- 1500
B <- 5000
total <- 42024

#### DISEÑO SIMPLE
set.seed(123)

est_si <- replicate(B, {
  s <- sample(1:N, size = n, replace = FALSE)
  (N / n) * sum(datos$HOGCE13[s], na.rm = TRUE)
})

mean(est_si)
var(est_si)
sd(est_si)
mean(est_si) - total

summary(est_si)

df_si <- data.frame(est_si)

#Histograma
ggplot(df_si, aes(x = est_si)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "white") +
  geom_vline(xintercept = total,
             color = "red",
             linetype = "dashed",
             linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π bajo SI",
    x = expression(hat(t)[pi]),
    y = "Frecuencia"
  ) +
  theme_minimal()

#Densidad
ggplot(df_si, aes(x = est_si)) +
  geom_density(fill = "lightblue", alpha = 0.4) +
  geom_vline(xintercept = total,
             colour = "red",
             linewidth = 1.2,
             linetype = "dashed") +
  labs(
    x = expression(hat(t)[pi]),
    y = "Densidad",
    title = "Distribución empírica del estimador π bajo SI"
  ) +
  theme_bw()

# DISEÑO SIR
#1.Calcular 
pi_k <- 1 - (1 - 1/N)^n

#2.Simular el diseño SIR
set.seed(123)

est_sir <- replicate(B, {
  
  # 1500 extracciones con reposición
  s <- sample(1:N, size = n, replace = TRUE)
  
  # hogares distintos incluidos
  s_unicos <- unique(s)
  
  # estimador π
  sum(datos$HOGCE13[s_unicos], na.rm = TRUE) / pi_k
  
})

mean(est_sir)
var(est_sir)
sd(est_sir)
mean(est_sir) - total
summary(est_sir)

df_sir <- data.frame(est_sir)

#Densidad estimador pi SIR
ggplot(df_sir, aes(x = est_sir)) +
  geom_density(fill = "lightgreen", alpha = 0.4) +
  geom_vline(xintercept = total,
             colour = "red",
             linetype = "dashed",
             linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π - SIR",
    x = expression(hat(t)[pi]),
    y = "Densidad"
  ) +
  theme_bw()

#DISEÑO ESTRATIFICADO
datos$estrato <- cut(
  datos$HOGPR01,
  breaks = c(0, 1, 2, 4, Inf),
  labels = c("1 persona", "2 personas", "3-4 personas", "5 o más"),
  right = TRUE
)

table(datos$estrato, useNA = "ifany")

Nh <- table(datos$estrato) #tamaño poblacional del estrato
nh <- round(n * Nh / N) #tamaños muestrales de cada estrato

nh 
sum(nh)

#Simulamos
set.seed(123)

est_st <- replicate(B, {
  
  total_est <- 0
  
  for(h in names(Nh)){
    
    indices_h <- which(datos$estrato == h)
    
    Nh_h <- length(indices_h)
    nh_h <- nh[h]
    
    s_h <- sample(indices_h, size = nh_h, replace = FALSE)
    
    total_est <- total_est + (Nh_h / nh_h) * sum(datos$HOGCE13[s_h], na.rm = TRUE)
  }
  
  total_est
})

mean(est_st)
var(est_st)
sd(est_st)
sesgo_st = mean(est_st) - total
summary(est_st)

#Graficamos densidad

df_st <- data.frame(est_st = est_st)

ggplot(df_st, aes(x = est_st)) +
  geom_density(fill = "lightblue", alpha = 0.4) +
  geom_vline(xintercept = total,
             color = "red",
             linetype = "dashed",
             linewidth = 1) +
  geom_vline(xintercept = mean(est_st),
             color = "blue",
             linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π - Diseño estratificado",
    x = expression(hat(t)[pi]),
    y = "Densidad"
  ) +
  theme_bw()


#DISEÑO POR CONGLOMERADOS 
library(dplyr)

seg <- datos %>%
  group_by(SEGMENTO) %>%
  summarise(
    M_i = n(),                       # cantidad de hogares en el segmento
    t_i = sum(HOGCE13, na.rm = TRUE),# total de autos en el segmento
    .groups = "drop"
  )

N_I <- nrow(seg)       # cantidad total de segmentos
M_bar <- mean(seg$M_i) # tamaño promedio de segmento


n_I <- round(n / M_bar)
n_I
#Ahora simulamos un muestreo SI de segmentos. Si se seleccionan nI​segmentos de NI​
#, entonces la probabilidad de inclusión de primer orden para cada segmento es:
set.seed(123)

est_cong <- replicate(B, {
  
  s_I <- sample(1:N_I, size = n_I, replace = FALSE)
  
  (N_I / n_I) * sum(seg$t_i[s_I])
  
})

#Vemos cuántos hogares entran efectivamente en cada simulación 
tam_cong <- replicate(B, {
  
  s_I <- sample(1:N_I, size = n_I, replace = FALSE)
  
  sum(seg$M_i[s_I])
  
})

mean(est_cong)
var(est_cong)
sd(est_cong)
mean(est_cong) - total
summary(est_cong)


summary(tam_cong)
mean(tam_cong)
sd(tam_cong)


df_cong <- data.frame(est_cong = est_cong)

ggplot(df_cong, aes(x = est_cong)) +
  geom_density(fill = "orange", alpha = 0.4) +
  geom_vline(xintercept = total,
             color = "red",
             linetype = "dashed",
             linewidth = 1) +
  geom_vline(xintercept = mean(est_cong),
             color = "blue",
             linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π - Diseño por conglomerados",
    x = expression(hat(t)[pi]),
    y = "Densidad"
  ) +
  theme_bw()

data.frame(
  diseno = c("SI", "SIR", "Estratificado", "Conglomerados"),
  media = c(mean(est_si), mean(est_sir), mean(est_st), mean(est_cong)),
  varianza = c(var(est_si), var(est_sir), var(est_st), var(est_cong)),
  desvio = c(sd(est_si), sd(est_sir), sd(est_st), sd(est_cong)),
  sesgo_empirico = c(mean(est_si)-total,
                     mean(est_sir)-total,
                     mean(est_st)-total,
                     mean(est_cong)-total)
)


var_si   <- var(est_si)
var_sir  <- var(est_sir)
var_st   <- var(est_st)
var_cong <- var(est_cong)

deff_si   <- 1
deff_sir  <- var_sir  / var_si
deff_st   <- var_st   / var_si
deff_cong <- var_cong / var_si


comparacion <- data.frame(
  Diseño = c("SI",
             "SIR",
             "Estratificado",
             "Conglomerados"),
  
  Media = c(mean(est_si),
            mean(est_sir),
            mean(est_st),
            mean(est_cong)),
  
  Varianza = c(var(est_si),
               var(est_sir),
               var(est_st),
               var(est_cong)),
  
  SD = c(sd(est_si),
         sd(est_sir),
         sd(est_st),
         sd(est_cong)),
  
  Sesgo = c(mean(est_si)-total,
            mean(est_sir)-total,
            mean(est_st)-total,
            mean(est_cong)-total),
  
  DEFF = c(deff_si,
           deff_sir,
           deff_st,
           deff_cong)
)

comparacion
    #El mejor diseño empleado es el estratificado. Podríamos probar otras estratificaciones

