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
      estado %in% c("Baja California", "Baja California Sur", "Sonora", "Sinaloa",
                    "Nayarit", "Jalisco", "Colima", "Michoacán", "Guerrero",
                    "Oaxaca", "Chiapas", "Tamaulipas", "Veracruz",
                    "Tabasco", "Campeche", "Yucatán", "Quintana Roo") ~ "Con Costa",
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
  ) %>%
  
  # Eliminamos ruido
  filter(fenomeno_ajustado != "Otros_Hidro")

# ==============================
# 4. AJUSTE DE DAÑOS (BASE)
# ==============================

# Aquí puedes meter inflación después si quieres
df_proyecto <- df_proyecto %>%
  mutate(
    danos_ajustados = danos_millones
  )

# ==============================
# 5. TRANSFORMACIÓN LOG
# ==============================

df_proyecto <- df_proyecto %>%
  mutate(
    log_danos = log(danos_ajustados)
  )

# ==============================
# 6. ANÁLISIS DESCRIPTIVO
# ==============================

summary(df_proyecto$danos_ajustados)

sd(df_proyecto$danos_ajustados)

quantile(df_proyecto$danos_ajustados)

table(df_proyecto$fenomeno_ajustado)

table(df_proyecto$edo_ajustado)

# ==============================
# 7. HISTOGRAMA
# ==============================

hist(df_proyecto$danos_ajustados,
     breaks = 40,
     main = "Distribución de pérdidas",
     xlab = "Daños (millones de pesos)")

# ==============================
# 8. AJUSTE DE DISTRIBUCIONES
# ==============================

datos <- df_proyecto$danos_ajustados

fit_lognorm <- fitdist(datos, "lnorm")
fit_gamma   <- fitdist(datos, "gamma")
fit_pareto  <- fitdist(datos, "pareto",
                       start = list(shape = 1, scale = 1))

# ==============================
# 9. PRUEBAS DE BONDAD DE AJUSTE
# ==============================

gofstat(list(fit_lognorm, fit_gamma, fit_pareto))

# ==============================
# 10. MODELOS POR GRUPOS
# ==============================

# Con Costa
datos_costa <- df_proyecto %>%
  filter(edo_ajustado == "Con Costa")

fit_costa <- fitdist(datos_costa$danos_ajustados, "lnorm")

# Sin Costa
datos_sincosta <- df_proyecto %>%
  filter(edo_ajustado == "Sin Costa")

fit_sincosta <- fitdist(datos_sincosta$danos_ajustados, "lnorm")

# Ciclones
datos_ciclones <- df_proyecto %>%
  filter(fenomeno_ajustado == "Ciclones")

fit_ciclones <- fitdist(datos_ciclones$danos_ajustados, "lnorm")

# Inundaciones
datos_inundaciones <- df_proyecto %>%
  filter(fenomeno_ajustado == "Inundaciones")

fit_inundaciones <- fitdist(datos_inundaciones$danos_ajustados, "lnorm")

# ==============================
# 11. COSTO PROMEDIO ESPERADO
# ==============================

mu <- fit_lognorm$estimate["meanlog"]
sigma <- fit_lognorm$estimate["sdlog"]

EX <- exp(mu + sigma^2/2)

EX
cat("Costo promedio esperado por evento:", EX, "millones de pesos\n")

# ==============================
# 12. VERIFICACIÓN DEL MODELO
# ==============================

media_empirica <- mean(df_proyecto$danos_ajustados)

media_empirica
EX
EX - media_empirica