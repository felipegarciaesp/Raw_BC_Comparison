# =====================================================================
# Codigo para comparar data cruda y BC de modelos GCM 
# con data observada / Felipe Garcia
# =====================================================================

## Limpiar ambiente
#-------------------
rm(list=ls())
graphics.off()
cat("\014")

## Carga de paquetes
#-------------------
library(pacman)
p_load(openxlsx, tidyverse, lubridate, ggnewscale)


## Directorio de trabajo
#-------------------
setwd("C:/Codigos/Raw_BC_Comparison")
dir_inputs  <- file.path(getwd(), "Inputs")
dir_outputs <- file.path(getwd(), "Outputs")

## Definicion del periodo historico a analizar
#-------------------
yr_hist_start <- 1979
yr_hist_end   <- 2014

## Lectura de archivos
#-------------------
df_obs  <- openxlsx::read.xlsx(file.path(dir_inputs, "02_Vicuna3_tasmax_daily_obs_raw_analysis.xlsx"),      sheet = "Data", colNames = TRUE)
df_hist <- openxlsx::read.xlsx(file.path(dir_inputs, "02_Vicuna3_tasmax_daily_historical_raw_analysis.xlsx"), sheet = "Data", colNames = TRUE)

# Convertir columna de fechas
df_obs[,1]  <- openxlsx::convertToDate(df_obs[,1])
df_hist[,1] <- openxlsx::convertToDate(df_hist[,1])

# Combinar en un unico dataframe: fecha + obs + modelos GCM
df_combined <- df_obs %>%
  rename(Date = 1) %>%
  left_join(df_hist %>% rename(Date = 1), by = "Date") %>%
  filter(year(Date) >= yr_hist_start & year(Date) <= yr_hist_end)

## Calcular promedios mensuales
#-------------------

# Observaciones
df_obs_mon <- df_combined %>%
  select(Date, 2) %>%
  rename(obs = 2) %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarise(obs = mean(obs, na.rm = TRUE))

# Modelos GCM
gcm_cols <- colnames(df_combined)[-(1:2)]

df_hist_mon <- df_combined %>%
  select(Date, all_of(gcm_cols)) %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarise(across(all_of(gcm_cols), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(gcm_cols), names_to = "model", values_to = "value")

## Grafico
#-------------------
month_labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

n_models   <- length(gcm_cols)
grey_scale <- colorRampPalette(c("grey20", "grey80"))(n_models)

ggplot() +
  # Lineas GCM historico en grises
  geom_line(data = df_hist_mon,
            aes(x = month, y = value, group = model, color = model),
            linewidth = 0.6, alpha = 0.8) +
  scale_color_manual(values = setNames(grey_scale, gcm_cols), name = "Modelos GCM") +
  # Nueva escala de color para observaciones
  ggnewscale::new_scale_color() +
  # Linea observada en rojo
  geom_line(data = df_obs_mon,
            aes(x = month, y = obs, color = "Observado"),
            linewidth = 1.2) +
  scale_color_manual(values = c("Observado" = "red"), name = "") +
  # Ejes y etiquetas
  scale_x_continuous(breaks = 1:12, labels = month_labels) +
  labs(
    title    = "Promedio mensual de Tasmax - Data cruda vs Observada",
    subtitle = paste0("Periodo: ", yr_hist_start, " - ", yr_hist_end),
    x        = "Mes",
    y        = "Tasmax [°C]"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5)
  )
