# ==============================
# 1. BIBLIOTECAS
# ==============================

install.packages("tidyverse")
install.packages("readxl")
install.packages("fitdistrplus")
install.packages("actuar")

library(tidyverse)
library(readxl)
library(fitdistrplus)
library(actuar)

# ==============================
# 2. CARGAR DATOS
# ==============================

df_original <- read_excel("Basehistorica_2000_a_2024.xlsx")

# ==============================
# 3. LIMPIEZA
# ==============================

df_proyecto <- df_original %>%
  
  rename(
    anio = `Año`,
    clasificacion = `Clasificación del fenómeno`,
    tipo_fenomeno = `Tipo de fenómeno`,
    estado = `Estado`,
    danos_millones = `Total de daños (millones de pesos)`
  ) %>%
  
  filter(
    clasificacion == "Hidrometeorológico",
    danos_millones > 0
  ) %>%
  
  mutate(
    edo_ajustado = case_when(
      estado %in% c("Baja California Sur","Chiapas","Michoacán",
                    "Nayarit","Veracruz","Tamaulipas",
                    "Guerrero","Oaxaca","Sinaloa") ~ "Con Costa",
      TRUE ~ "Sin Costa"
    )
  ) %>%
  
  mutate(
    fenomeno_ajustado = case_when(
      tipo_fenomeno %in% c("Ciclón tropical","Tormenta tropical","Huracán") ~ "Ciclones",
      tipo_fenomeno %in% c("Inundación pluvial","Inundación fluvial") ~ "Inundaciones",
      tipo_fenomeno == "Sequía" ~ "Sequía",
      TRUE ~ "Otros_Hidro"
    )
  )

# ==============================
# 4. LOGARITMO DE LAS PÉRDIDAS
# ==============================

df_proyecto <- df_proyecto %>%
  mutate(
    log_danos = log(danos_millones)
  )

# ==============================
# 5. ANÁLISIS DESCRIPTIVO
# ==============================

summary(df_proyecto$danos_millones)

sd(df_proyecto$danos_millones)

quantile(df_proyecto$danos_millones)

table(df_proyecto$fenomeno_ajustado)

# ==============================
# 6. HISTOGRAMA
# ==============================

hist(df_proyecto$danos_millones,
     breaks = 40,
     main = "Distribución de pérdidas",
     xlab = "Daños (millones de pesos)")

# ==============================
# 7. AJUSTE DE DISTRIBUCIONES
# ==============================

datos <- df_proyecto$danos_millones

fit_lognorm <- fitdist(datos, "lnorm")

fit_gamma <- fitdist(datos, "gamma")

fit_pareto <- fitdist(datos, "pareto",
                      start=list(shape=1, scale=1))

# ==============================
# 8. PRUEBAS DE BONDAD DE AJUSTE
# ==============================

gofstat(list(fit_lognorm, fit_gamma, fit_pareto))

# ==============================
# 9. MODELOS POR GRUPOS
# ==============================

# Modelo 1: Estados con costa
datos_costa <- df_proyecto %>%
  filter(edo_ajustado == "Con Costa")

fit_costa <- fitdist(datos_costa$danos_millones, "lnorm")


# Modelo 2: Estados sin costa
datos_sincosta <- df_proyecto %>%
  filter(edo_ajustado == "Sin Costa")

fit_sincosta <- fitdist(datos_sincosta$danos_millones, "lnorm")


# Modelo 3: Ciclones
datos_ciclones <- df_proyecto %>%
  filter(fenomeno_ajustado == "Ciclones")

fit_ciclones <- fitdist(datos_ciclones$danos_millones, "lnorm")


# Modelo 4: Inundaciones
datos_inundaciones <- df_proyecto %>%
  filter(fenomeno_ajustado == "Inundaciones")

fit_inundaciones <- fitdist(datos_inundaciones$danos_millones, "lnorm")

# ==============================
# 10. COSTO PROMEDIO ESPERADO
# ==============================

# Parámetros estimados del modelo lognormal
mu <- fit_lognorm$estimate["meanlog"]
sigma <- fit_lognorm$estimate["sdlog"]

# Esperanza teórica de la Lognormal
EX <- exp(mu + sigma^2/2)

cat("Costo promedio esperado por evento:", EX, "millones de pesos\n")

# ==============================
# 11. VERIFICACIÓN DEL MODELO
# ==============================

# Media empírica de los datos
media_empirica <- mean(df_proyecto$danos_millones)

# Valor esperado según el modelo ajustado
EX

media_empirica
EX

# Observamos qué tan distintos son nuestros resultados
EX - media_empirica
EX