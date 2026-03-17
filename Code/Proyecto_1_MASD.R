# ==============================
# 1. BIBLIOTECAS
# ==============================

install.packages("tidyverse")
install.packages("readxl")
install.packages("fitdistrplus")
install.packages("actuar")
install.packages("writexl")


library(tidyverse)
library(readxl)
library(fitdistrplus)
library(actuar)
library(writexl) # para guardar la base de datos limpia

# ==============================
# 2. CARGAR DATOS
# ==============================

# Asumimos que estamos en el directorio general, el del repositorio de Github

ruta <- "data/Basehistorica_2000_a_2024.xlsx"

df_original <- read_excel(ruta,
                          na = c("sd", "SD", "SNR", "SC", "SR", "NSR", "NR", "Nsr",
                                 '7 niveles de un edificio', '19 362')
                          )

# ==============================
# 3. LIMPIEZA
# ==============================

# corrige errores puntuales
df_original[1087,16] <- as.numeric(df_original[1087,15])*0.001
df_original[9296,11] <- as.numeric(19362)

# Formatea columnas
df_original$`Fecha de Fin` <- as.Date(
  as.numeric(ifelse(grepl("^[0-9]+$", df_original$`Fecha de Fin`),
                    df_original$`Fecha de Fin`, NA)),
  origin = "1899-12-30"
)
df_original$`Clasificación del fenómeno` <- as.factor(df_original$`Clasificación del fenómeno`)
df_original$`Tipo de fenómeno` <- as.factor(df_original$`Tipo de fenómeno`)
df_original$Fuente <- as.factor(df_original$Fuente)

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

# Guarda la base de datos limpia
write_xlsx(df_proyecto, "data/df_limpia.xlsx")

# ==============================
# 4. AJUSTE DE DAÑOS (BASE)
# ==============================

# Aquí meteremos inflación, de ser necesario
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