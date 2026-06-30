# ============================================================
# PROYECTO MUESTREO
# ============================================================

library(ggplot2)
library(sampling)
library(survey)
library(haven)
library(tidyverse)
library(patchwork)

load("Rivera.RData")

# ============================================================
# 0. BASE REDUCIDA
# ============================================================

datos <- riv[, c(
  "ID_CENSO", "ID_HOGAR",
  "DIRECCION_ID", "VIVID", "HOGID",
  "SEGMENTO", "SECCION", "LOCALIDAD", "AREA", "REGION_4",
  "BARRIO85", "CCZ", "MUNICIPIO_136",
  "HOGHD00", "HOGHD01",
  "HOGCE11", "HOGCE12_1", "HOGCE13", "HOGCE17", "HOGCE17_1",
  "HOGCE22", "HOGCE26", "HOGCE27", "HOGCE28",
  "HOGPR01", "HOGPR01_1", "HOGPR01_2", "HOGPR01_3"
)]

# Códigos especiales
codigos_na <- c(99, 7777, 8888, 9898, 9999)

datos$HOGCE13 <- ifelse(datos$HOGCE13 %in% codigos_na, NA, datos$HOGCE13)

# Eliminamos hogares/personas sin dato válido en HOGCE13
datos <- datos[!is.na(datos$HOGCE13), ]

# Construimos base de hogares
datos <- datos %>%
  distinct(ID_HOGAR, .keep_all = TRUE)

# Verificaciones
nrow(datos)
length(unique(datos$ID_HOGAR))
sum(duplicated(datos$ID_HOGAR))

# ============================================================
# 1. TOTAL POBLACIONAL
# ============================================================

total <- sum(datos$HOGCE13)

N <- nrow(datos)
n <- 1500
f <- n / N
B <- 500

total #total de autos por hogar
N     #total de observaciones

# ============================================================
# 2. DISEÑO SIMPLE SI
# ============================================================

set.seed(123)

est_si <- replicate(B, {
  
  s <- sample(1:N, size = n, replace = FALSE)
  
  y_s <- datos$HOGCE13[s]
  
  y_barra_s <- mean(y_s)
  
  t_hat <- N * y_barra_s
  
  t_hat 
})

media_empirica_SI <- mean(est_si)
V_empirica_SI <- var(est_si)
SE_empirico_SI <- sd(est_si)
sesgo_empirico_SI <- media_empirica_SI - total


resumen_SI <- data.frame(
  diseno = "SI",
  media_empirica = media_empirica_SI,
  varianza_empirica = V_empirica_SI,
  error_estandar_empirico = SE_empirico_SI,
  sesgo_empirico = sesgo_empirico_SI
)

S2_yU <- var(datos$HOGCE13)
V_teorica_SI <- N^2 * (1 - f) * S2_yU / n
SE_teorico_SI <- sqrt(V_teorica_SI)

resumen_SI

df_si <- data.frame(est_si = est_si)

ggplot(df_si, aes(x = est_si)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "white") +
  geom_vline(xintercept = total, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(est_si), color = "blue", linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π bajo SI",
    x = expression(hat(t)[pi]),
    y = "Frecuencia"
  ) +
  theme_minimal()

# ============================================================
# 3. DISEÑO SIMPLE CON REEMPLAZO SIR
# ============================================================

pi_k <- 1 - (1 - 1/N)^n

set.seed(123)

est_sir <- replicate(B, {
  
  s <- sample(1:N, size = n, replace = TRUE)
  
  s_unicos <- unique(s)
  
  y_s <- datos$HOGCE13[s_unicos]
  
  t_hat <- sum(y_s) / pi_k
  
  t_hat
})


media_empirica_SIR <- mean(est_sir)
V_empirica_SIR <- var(est_sir)
SE_empirico_SIR <- sd(est_sir)
sesgo_empirico_SIR <- media_empirica_SIR - total

resumen_SIR <- data.frame(
  diseno = "SIR",
  media_empirica = media_empirica_SIR,
  varianza_empirica = V_empirica_SIR,
  error_estandar_empirico = SE_empirico_SIR,
  sesgo_empirico = sesgo_empirico_SIR
)

resumen_SIR

df_sir <- data.frame(est_sir = est_sir)

ggplot(df_sir, aes(x = est_sir)) +
  geom_histogram(bins = 40, fill = "lightgreen", color = "white") +
  geom_vline(xintercept = total, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(est_sir), color = "blue", linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π bajo SIR",
    x = expression(hat(t)[pi]),
    y = "Frecuencia"
  ) +
  theme_minimal()

# ============================================================
# 4. DISEÑO ESTRATIFICADO
# Estratificación según cantidad de habitaciones
# ============================================================


datos$estrato <- cut(
  datos$HOGHD00,
  breaks = c(0, 1, 2, 3, 4, 5, Inf),
  labels = c("1 habitación", "2 habitaciones",
             "3 habitaciones", "4 habitaciones", 
             "5 habitaciones", "6 o más"),
  right = TRUE
)

table(datos$estrato, useNA = "ifany")

tam <- datos %>%
  count(estrato, name = "Nh") %>%
  mutate(
    nh = round(n * Nh / sum(Nh))
  )

dif <- n - sum(tam$nh)

if(dif != 0){
  tam$nh[which.max(tam$Nh)] <- tam$nh[which.max(tam$Nh)] + dif
}

sum(tam$nh)
tam

datos_st <- datos %>%
  left_join(tam, by = "estrato") %>%
  arrange(estrato)

set.seed(123)

est_st <- replicate(B, {
  
  s <- sampling::strata(
    datos_st,
    stratanames = "estrato",
    size = tam$nh,
    method = "srswor"
  )
  
  muestra <- sampling::getdata(datos_st, s)
  
  dis <- svydesign(
    ids = ~1,
    strata = ~estrato,
    data = muestra,
    fpc = ~Nh
  )
  
  as.numeric(coef(svytotal(~HOGCE13, dis)))
})

media_empirica_ST <- mean(est_st)
V_empirica_ST <- var(est_st)
SE_empirico_ST <- sd(est_st)
sesgo_empirico_ST <- media_empirica_ST - total

resumen_ST <- data.frame(
  diseno = "Estratificado",
  media_empirica = media_empirica_ST,
  varianza_empirica = V_empirica_ST,
  error_estandar_empirico = SE_empirico_ST,
  sesgo_empirico = sesgo_empirico_ST
)

resumen_ST

df_st <- data.frame(est_st = est_st)

ggplot(df_st, aes(x = est_st)) +
  geom_histogram(bins = 40, fill = "lightpink", color = "white") +
  geom_vline(xintercept = total, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(est_st), color = "blue", linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π bajo diseño estratificado",
    subtitle = "Estratificación según número de habitaciones en el hogar",
    x = expression(hat(t)[pi]),
    y = "Frecuencia"
  ) +
  theme_minimal()

# ============================================================
# 5. DISEÑO POR CONGLOMERADOS SIN SURVEY
# UPM: segmentos censales
# ============================================================

# Verificamos si SEGMENTO identifica unívocamente al conglomerado
length(unique(datos$SEGMENTO))

datos %>%
  distinct(SECCION, SEGMENTO) %>%
  nrow()

# Como SEGMENTO se repite entre secciones, creamos un identificador compuesto
datos$codsegm <- paste(datos$SECCION, datos$SEGMENTO, sep = "_")

# Cantidad total de conglomerados
NI <- length(unique(datos$codsegm))

# Base a nivel conglomerado
# ti = total de autos del conglomerado i
# Mi = cantidad de hogares del conglomerado i
cong <- datos %>%
  group_by(codsegm) %>%
  summarise(
    Mi = n(),
    ti = sum(HOGCE13),
    .groups = "drop"
  )

# Tamaño promedio de los conglomerados
M_bar <- mean(cong$Mi)

# Cantidad de conglomerados a seleccionar
# Usamos al menos 2 conglomerados para poder estudiar variabilidad
nI <- max(2, ceiling(n / M_bar))

NI
M_bar
nI

# Simulación del estimador bajo muestreo por conglomerados
set.seed(123)

est_cong <- replicate(B, {
  
  # Selecciono nI conglomerados con SI 
  sI <- sample(1:NI, size = nI, replace = FALSE)
  
  # Totales de autos de los conglomerados seleccionados
  t_sI <- cong$ti[sI]
  
  # Estimador del total poblacional
  t_hat_cong <- (NI / nI) * sum(t_sI)
  
  t_hat_cong
})

media_empirica_CONG <- mean(est_cong)
V_empirica_CONG <- var(est_cong)
SE_empirico_CONG <- sd(est_cong)
sesgo_empirico_CONG <- media_empirica_CONG - total

resumen_CONG <- data.frame(
  diseno = "Conglomerados",
  media_empirica = media_empirica_CONG,
  varianza_empirica = V_empirica_CONG,
  error_estandar_empirico = SE_empirico_CONG,
  sesgo_empirico = sesgo_empirico_CONG
)

resumen_CONG

df_cong <- data.frame(est_cong = est_cong)

ggplot(df_cong, aes(x = est_cong)) +
  geom_histogram(bins = 40, fill = "orange", color = "white") +
  geom_vline(xintercept = total, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(est_cong), color = "blue", linewidth = 1) +
  labs(
    title = "Distribución empírica del estimador π bajo diseño por conglomerados",
    subtitle = "UPM: segmentos censales",
    x = expression(hat(t)[pi]),
    y = "Frecuencia"
  ) +
  theme_minimal()

# ============================================================
# 6. COMPARACIÓN FINAL
# ============================================================

comparacion <- rbind(
  resumen_SI,
  resumen_SIR,
  resumen_ST,
  resumen_CONG
)

comparacion$deff <- comparacion$varianza_empirica / V_empirica_SI

comparacion

# Gráfico conjunto

df_comp <- rbind(
  data.frame(estimador = est_si, diseno = "SI"), 
  data.frame(estimador = est_sir, diseno = "SIR"),
  data.frame(estimador = est_st, diseno = "Estratificado"),
  data.frame(estimador = est_cong, diseno = "Conglomerados")
)


df_si <- data.frame(estimador = est_si, diseno = "SI")

df_sir_si <- rbind(
  data.frame(estimador = est_si, diseno = "SI"),
  data.frame(estimador = est_sir, diseno = "SIR")
)

df_st_si <- rbind(
  data.frame(estimador = est_si, diseno = "SI"),
  data.frame(estimador = est_st, diseno = "Estratificado")
)

df_cong_si <- rbind(
  data.frame(estimador = est_si, diseno = "SI"),
  data.frame(estimador = est_cong, diseno = "Conglomerados")
)

g1 <- ggplot(df_si, aes(x = estimador)) +
  geom_density(fill = "lightblue", alpha = 0.45) +
  geom_vline(xintercept = total, color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(est_si), color = "blue") +
  labs(
    title = "1. SI",
    x = expression(hat(t)[pi]),
    y = "Densidad"
  ) +
  theme_minimal()

g2 <- ggplot(df_sir_si, aes(x = estimador, fill = diseno)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = total, color = "red", linetype = "dashed") +
  labs(
    title = "2. SIR comparado con SI",
    x = expression(hat(t)[pi]),
    y = "Densidad",
    fill = "Diseño"
  ) +
  theme_minimal()

g3 <- ggplot(df_st_si, aes(x = estimador, fill = diseno)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = total, color = "red", linetype = "dashed") +
  labs(
    title = "3. Estratificado comparado con SI",
    x = expression(hat(t)[pi]),
    y = "Densidad",
    fill = "Diseño"
  ) +
  theme_minimal()

g4 <- ggplot(df_cong_si, aes(x = estimador, fill = diseno)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = total, color = "red", linetype = "dashed") +
  labs(
    title = "4. Conglomerados comparado con SI",
    x = expression(hat(t)[pi]),
    y = "Densidad",
    fill = "Diseño"
  ) +
  theme_minimal()

(g1 | g2) / (g3 | g4)

((g1 + g2) / (g3 + g4)) +
  plot_annotation(
    title = "Distribuciones empíricas del estimador π según diseño muestral"
  )


#======================== INTERVALOS DE CONFIANZA =======================

# Usando las distribuciones empiricas muestrales obtenidas en el punto anteior,
# se definen intervalos de dos formas distintas para cada diseño de muestreo.

#----------------------------------
# Intervalos empíricos (cuantiles)
#----------------------------------
## Para el vector de totales obtenidos calculamos los cuantiles 0,025 y 0,975 para 
## obtener los extremos de los intervalos al 95% de confianza.

# SI
ic_emp_si <- quantile(est_si, c(0.025, 0.975))
ic_emp_si

# SIR
ic_emp_sir <- quantile(est_sir, c(0.025, 0.975))
ic_emp_sir

# Estratificado
ic_emp_st <- quantile(est_st, c(0.025, 0.975))
ic_emp_st

# Conglomerados
ic_emp_cong <- quantile(est_cong, c(0.025, 0.975))
ic_emp_cong

#----------------------------------
# Intervalos asumiendo normalidad
#----------------------------------
## Asumiendo normalidad en las distribuciones empiricas de los vectores de totales
## de cada diseño, se definen intervalos de confianza al 95%.

## Usamos la media de las estimaciones simuladas como centro del intervalo y el 
## desvío estándar empírico como medida de variabilidad. Aunque en algún diseño, 
## como conglomerados, la distribución no parezca muy normal, igual calculamos este 
## intervalo para poder compararlo con el intervalo empírico basado en cuantiles.

z <- qnorm(0.975)  # percentil 97.5% de la Normal estándar

# SI
ic_norm_si <- c(
  mean(est_si) - z * sd(est_si),
  mean(est_si) + z * sd(est_si)
)
ic_norm_si

# SIR
ic_norm_sir <- c(
  mean(est_sir) - z * sd(est_sir),
  mean(est_sir) + z * sd(est_sir)
)
ic_norm_sir

# Estratificado
ic_norm_st <- c(
  mean(est_st) - z * sd(est_st),
  mean(est_st) + z * sd(est_st)
)
ic_norm_st

# Conglomerados
ic_norm_cong <- c(
  mean(est_cong) - z * sd(est_cong),
  mean(est_cong) + z * sd(est_cong)
)
ic_norm_cong



#----------------------------------
# Tabla resumen de intervalos
#----------------------------------

## Armamos tabla para comparar intervalos empiricos y asumiendo normalidad

# Esta función recibe un intervalo, por ejemplo c(13500, 14500),
# y lo transforma en texto con formato prolijo: "[13500 ; 14500]".
# Esto es solo para que la tabla final sea más fácil de leer.
formato_ic <- function(ic){
  paste0(
    "[",
    round(as.numeric(ic[1]), 2),  # límite inferior redondeado
    " ; ",
    round(as.numeric(ic[2]), 2),  # límite superior redondeado
    "]"
  )
}

# Esta función calcula la longitud del intervalo.
# Es decir: límite superior - límite inferior.
longitud_ic <- function(ic){
  round(as.numeric(ic[2] - ic[1]), 2)
}

# Armamos una tabla resumen con una fila por diseño.
# Para cada diseño guardamos:
# - el intervalo empírico
# - la longitud del intervalo empírico
# - el intervalo bajo normalidad
# - la longitud del intervalo bajo normalidad
tabla_ic <- data.frame(
  
  # Nombre de cada diseño muestral
  Diseno = c("SI", "SIR", "Estratificado", "Conglomerados"),
  
  # Intervalos empíricos, construidos con cuantiles
  IC_empirico = c(
    formato_ic(ic_emp_si),
    formato_ic(ic_emp_sir),
    formato_ic(ic_emp_st),
    formato_ic(ic_emp_cong)
  ),
  
  # Longitud de los intervalos empíricos
  Long_empirico = c(
    longitud_ic(ic_emp_si),
    longitud_ic(ic_emp_sir),
    longitud_ic(ic_emp_st),
    longitud_ic(ic_emp_cong)
  ),
  
  # Intervalos construidos suponiendo normalidad
  IC_normal = c(
    formato_ic(ic_norm_si),
    formato_ic(ic_norm_sir),
    formato_ic(ic_norm_st),
    formato_ic(ic_norm_cong)
  ),
  
  # Longitud de los intervalos bajo normalidad
  Long_normal = c(
    longitud_ic(ic_norm_si),
    longitud_ic(ic_norm_sir),
    longitud_ic(ic_norm_st),
    longitud_ic(ic_norm_cong)
  )
)

# Mostramos la tabla final
tabla_ic

## Conclusiones ...

#======================== TOTAL ALT PARA CONGLOMERADO =======================

# El estimador alternativo parte de estimar primero la media poblacional
# y luego multiplicarla por el total conocido de hogares N.
#
# y_eS = t_hat_y_pi / N_hat_pi
#
# donde:
# t_hat_y_pi : estimador HT del total de la variable de interés
# N_hat_pi   : estimador HT del tamaño poblacional
#
# Luego:
# t_alt = N * y_eS
#
# En nuestro caso:
# - y = cantidad de autos/camionetas del hogar (HOGCE13)
# - N = total de hogares de la población
# - los conglomerados son los segmentos censales
#
# Como seleccionamos conglomerados con igual probabilidad:
#
# t_hat_y_pi = (NI / nI) * suma(t_i seleccionados)
# N_hat_pi   = (NI / nI) * suma(M_i seleccionados)
#
# donde:
# t_i = total de autos/camionetas en el conglomerado i
# M_i = cantidad de hogares en el conglomerado i
#
# Al dividir t_hat_y_pi / N_hat_pi, el factor (NI / nI) se cancela.
# Entonces:
#
# y_eS = suma(t_i seleccionados) / suma(M_i seleccionados)
#
# y finalmente:
#
# t_alt = N * y_eS

set.seed(123)

cong_alt <- datos %>%
  group_by(codsegm) %>%
  summarise(
    Mi = n(),
    ti = sum(HOGCE13),
    .groups = "drop"
  )

NI <- nrow(cong_alt)
M_bar <- mean(cong_alt$Mi)
nI <- max(2, ceiling(n / M_bar))

sim_cong_alt <- replicate(B, {
  
  # Selecciono nI conglomerados con SI
  sI <- sample(1:NI, size = nI, replace = FALSE)
  
  # Total de autos/camionetas en los conglomerados seleccionados
  t_sI <- cong_alt$ti[sI]
  
  # Cantidad de hogares en los conglomerados seleccionados
  M_sI <- cong_alt$Mi[sI]
  
  # Tamaño final de la muestra de hogares
  n_final <- sum(M_sI)
  
  # Estimador HT del total de autos/camionetas
  t_hat_y_pi <- (NI / nI) * sum(t_sI)
  
  # Estimador HT del total de hogares
  N_hat_pi <- (NI / nI) * n_final
  
  # Estimador alternativo de la media poblacional
  y_eS <- t_hat_y_pi / N_hat_pi
  
  # Estimador alternativo del total poblacional
  t_cong_alt <- N * y_eS
  
  c(
    est_cong_alt = t_cong_alt,
    n_final = n_final
  )
})

sim_cong_alt <- as.data.frame(t(sim_cong_alt))

est_cong_alt <- sim_cong_alt$est_cong_alt
n_final_cong_alt <- sim_cong_alt$n_final

head(sim_cong_alt)

summary(est_cong_alt)

summary(n_final_cong_alt)

#----------------------------------
# Comparación entre estimador π y estimador alternativo
# en diseño por conglomerados
#----------------------------------

comparacion_cong <- data.frame(
  estimador = c("Conglomerados - π", "Conglomerados - alternativo"),
  
  media_empirica = c(
    mean(est_cong),
    mean(est_cong_alt)
  ),
  
  varianza_empirica = c(
    var(est_cong),
    var(est_cong_alt)
  ),
  
  error_estandar_empirico = c(
    sd(est_cong),
    sd(est_cong_alt)
  ),
  
  sesgo_empirico = c(
    mean(est_cong) - total,
    mean(est_cong_alt) - total
  )
)

comparacion_cong

#----------------------------------
# Intervalos para estimador alternativo
#----------------------------------

# IC empírico por cuantiles
ic_emp_cong_alt <- quantile(est_cong_alt, probs = c(0.025, 0.975))

# IC bajo normalidad
z <- qnorm(0.975)

ic_norm_cong_alt <- c(
  mean(est_cong_alt) - z * sd(est_cong_alt),
  mean(est_cong_alt) + z * sd(est_cong_alt)
)

ic_emp_cong_alt
ic_norm_cong_alt


tabla_ic_cong <- data.frame(
  
  # Nombre de cada diseño muestral
  Diseno = c("Conglomerados", "Conglomerados alt"),
  
  # Intervalos empíricos, construidos con cuantiles
  IC_empirico = c(
    formato_ic(ic_emp_cong),
    formato_ic(ic_emp_cong_alt)
  ),
  
  # Longitud de los intervalos empíricos
  Long_empirico = c(
    longitud_ic(ic_emp_cong),
    longitud_ic(ic_emp_cong_alt)
  ),
  
  # Intervalos construidos suponiendo normalidad
  IC_normal = c(
    formato_ic(ic_norm_cong),
    formato_ic(ic_norm_cong_alt)
  ),
  
  # Longitud de los intervalos bajo normalidad
  Long_normal = c(
    longitud_ic(ic_norm_cong),
    longitud_ic(ic_norm_cong_alt)
  )
)

tabla_ic_cong

