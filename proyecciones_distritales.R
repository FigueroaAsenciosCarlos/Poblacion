# =================================================
# MODELO DE PROYECCION POBLACIONAL DISTRITAL
# =================================================

# 1. CARGA DE LIBRERÍAS
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(writexl)
library(ggplot2)

# 2. DATOS BASE
poblacion <- data.frame(
  Distrito = c("Arequipa", "Cayma", "Cerro Colorado", "Characato", "Jacobo Hunter",
               "Mollebaya", "Paucarpata", "Sachaca", "Socabaya", "Tiabaya", "Uchumayo", "Yanahuara"),
  P2007 = c(61747, 75054, 113590, 6751, 46263, 1415, 120893, 17602, 59892, 14731, 10712, 22975),
  P2017 = c(55437, 91935, 197954, 12949, 52426, 4756, 131346, 25612, 75351, 17102, 15391, 25417),
  P2023 = c(54362, 110831, 247274, 18423, 54315, 8090, 138046, 30126, 87750, 17709, 17981, 27353)
)

agua <- data.frame(
  Distrito = c("Arequipa", "Cayma", "Cerro Colorado", "Jacobo Hunter", "Paucarpata",
               "Sachaca", "Socabaya", "Tiabaya", "Uchumayo", "Yanahuara"),
  Agua2017 = c(25845, 26829, 49663, 11891, 27142, 7201, 17980, 2068, 978, 8966),
  Agua2023 = c(18209, 27665, 55885, 12026, 27277, 7780, 18729, 2096, 992, 8665)
)

territorio <- data.frame(
  Distrito = poblacion$Distrito,
  tipo_territorio = c("patrimonio", "semiurbano", "expansion", "semiurbano", "semiurbano",
                      "expansion", "semiurbano", "semiurbano", "semiurbano", "semiurbano",
                      "expansion", "patrimonio")
)

# 3. UNIÓN Y CÁLCULOS
datos <- poblacion %>%
  left_join(agua, by = "Distrito") %>%
  left_join(territorio, by = "Distrito") %>%
  mutate(
    r_exp_pob = log(P2023 / P2017) / 6,
    r_exp_agua = log(Agua2023 / Agua2017) / 6,
    diferencial = abs(r_exp_pob - r_exp_agua),
    ajuste_hidrico = coalesce(diferencial > 0.02, FALSE),
    r_exp_ajustada = ifelse(ajuste_hidrico, (r_exp_pob + r_exp_agua)/2, r_exp_pob),
    r_log_factor = case_when(
      tipo_territorio == "patrimonio" ~ 0.5,
      tipo_territorio == "semiurbano" ~ 0.7,
      tipo_territorio == "expansion" ~ 0.9,
      TRUE ~ 0.7
    ),
    r_log_ajustada = r_log_factor * r_exp_ajustada
  )

# 4. K DIFERENCIADO (Dato exogeno al modelo)
factores_K <- c(
  "Arequipa" = 1.10, "Cayma" = 1.20, "Cerro Colorado" = 1.28, "Characato" = 1.50,
  "Jacobo Hunter" = 1.15, "Mollebaya" = 1.65, "Paucarpata" = 1.20, "Sachaca" = 1.30,
  "Socabaya" = 1.25, "Tiabaya" = 1.40, "Uchumayo" = 1.42, "Yanahuara" = 1.12
)
datos <- datos %>% mutate(K_base = P2023 * factores_K[Distrito])

# 5. PENDIENTE LINEAL PARA AREQUIPA
pendientes <- (datos %>% filter(Distrito == "Arequipa") %>% mutate(pendiente_m = (P2023 - P2017)/6))$pendiente_m

# 6. PARÁMETROS Y CONVERSIÓN A LISTA DE LISTAS
parametros <- datos %>% transmute(Distrito, P0 = P2023, r_log = r_log_ajustada, K = K_base)
parametros_list <- split(parametros, seq(nrow(parametros))) %>% lapply(as.list)

# 7. FUNCIÓN DE PROYECCIÓN
proyectar_distrito <- function(Distrito, P0, r_log, K, escenario = "Base") {
  t <- 0:27
  if (escenario == "Alto") { r_log <- r_log * 1.2; K <- K * 1.10 }
  if (escenario == "Bajo") { r_log <- r_log * 0.85; K <- K * 0.90 }
  
  if (Distrito == "Arequipa") {
    pendiente <- pendientes
    if (pendiente < 0) pendiente <- pendiente * ifelse(escenario == "Alto", 0.8, ifelse(escenario == "Bajo", 1.2, 1))
    poblacion <- P0 + pendiente * t
    
  } else if (Distrito %in% c("Characato", "Mollebaya", "Sachaca", "Tiabaya", "Uchumayo")) {
    poblacion <- numeric(length(t))
    poblacion[1] <- P0
    P_anterior <- P0
    cambio <- FALSE
    t_inicio <- NA  # inicializa fuera del bucle
    
    for (i in 2:length(t)) {
      if (!cambio && P_anterior < 0.80 * K) {
        poblacion[i] <- P_anterior * exp(r_log)
        P_anterior <- poblacion[i]
      } else {
        if (!cambio) {
          cambio <- TRUE
          P_intermedia <- P_anterior
          t_inicio <- t[i - 1]  # ← CORRECCIÓN CLAVE AQUÍ
        }
        t_log <- t[i] - t_inicio
        poblacion[i] <- K / (1 + ((K - P_intermedia)/P_intermedia) * exp(-r_log * t_log))
        P_anterior <- poblacion[i]
      }
    }
    
  } else {
    poblacion <- K / (1 + ((K - P0)/P0) * exp(-r_log * t))
  }
  
  return(data.frame(Distrito = Distrito, Anio = 2023:2050, Escenario = escenario,
                    Poblacion = round(poblacion), K = K))
}

# 8. EJECUCIÓN PARA LOS 3 ESCENARIOS
escenarios <- c("Base", "Alto", "Bajo")
proyecciones_finales <- purrr::map_dfr(
  escenarios,
  function(esc) {
    purrr::map_dfr(parametros_list, function(p) {
      do.call(proyectar_distrito, c(p, escenario = esc))
    })
  }
)

# 9. EXPORTACIÓN DE RESULTADOS
write_csv(proyecciones_finales, "proyecciones_poblacionales_Arequipa_VF2806.csv")
write_xlsx(datos, "Parametros_y_Ajustes_Hidricos_Arequipa_VFF2806.xlsx")

# Verifica si hay filas para el año 2050
#proyecciones_finales %>% filter(Año == 2050) %>% nrow()

# Verifica contenido de esas filas
#proyecciones_finales %>% filter(Año == 2050)

# Tabla de proyecciones al 2050 con k_Base
tabla_2050 <- proyecciones_finales %>%
  filter(Anio == 2050) %>%
  select(Distrito, Escenario, Poblacion) %>%
  pivot_wider(names_from = Escenario, values_from = Poblacion) %>%
  left_join(select(datos, Distrito, K_base), by = "Distrito") %>%
  arrange(Distrito)
print(tabla_2050)
# ==============================================================================
