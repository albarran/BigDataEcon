# =============================================================================
# EJERCICIO TEMA 02 - TRANSFORMACIÓN DE DATOS
# =============================================================================
# Apellidos, Nombre: _______________________
# DNI / NIE (sin letra final): _____________
# email: ___________________________________
# =============================================================================

library(tidyverse)
library(rio)

# Cargar datos
empleados <- import("https://raw.githubusercontent.com/albarran/00datos/main/empleados.csv.zip")
regiones <- import("https://raw.githubusercontent.com/albarran/00datos/main/regionEmpleado.xlsx",
                   sheet = "regiones")
regionEmpleado <- import("https://raw.githubusercontent.com/albarran/00datos/main/regionEmpleado.xlsx",
                         sheet = "regionEmpleado")

# ⚠️ CAMBIAR POR TUS 4 ÚLTIMAS CIFRAS DEL DNI
MI_DNI <- 1234


# =============================================================================
# EJERCICIO 1.a (1.0 puntos)
# Periodo (año y mes) con mayores ventas totales para tu empleado
# =============================================================================

resultado_1a <- empleados |>
  # Tu código aquí


# =============================================================================
# EJERCICIO 1.b (1.5 puntos)
# Periodo con mayor proporción de ventas relativas al total de la empresa
# =============================================================================

resultado_1b <- empleados |>
  # Tu código aquí


# =============================================================================
# EJERCICIO 1.c (1.0 puntos)
# Para CADA empleado, su periodo con mayores ventas
# =============================================================================

resultado_1c <- empleados |>
  # Tu código aquí


# =============================================================================
# EJERCICIO 1.d (1.0 puntos)
# Periodo con mayores ventas totales en toda la empresa
# =============================================================================

resultado_1d <- empleados |>
  # Tu código aquí


# =============================================================================
# EJERCICIO 1.e (1.5 puntos)
# 2 gráficos y 1 tabla
# =============================================================================

# Gráfico 1: Evolución temporal ventas anuales por género (líneas)
grafico_1 <- ggplot(datos, aes(___)) +
  # Tu código aquí


# Gráfico 2: Número de empleados por género y año (barras)
grafico_2 <- ggplot(datos, aes(___)) +
  # Tu código aquí


# Tabla: Media de ventas anuales por género (años 2000, 2005, 2010)
tabla_ventas <- datos |>
  # Tu código aquí


# =============================================================================
# EJERCICIO 2.a (1.5 puntos)
# Empleado con mayores ventas totales en cada región
# =============================================================================

resultado_2a <- empleados |>
  # Tu código aquí


# =============================================================================
# EJERCICIO 2.b (1.5 puntos)
# Región con más ventas cada año (nombre completo de la región)
# =============================================================================

resultado_2b <- empleados |>
  # Tu código aquí


# =============================================================================
# EJERCICIO 2.c (1.0 puntos)
# Empleados "misteriosos" (sin región asignada)
# =============================================================================

resultado_2c <- empleados |>
  # Tu código aquí


# =============================================================================
# ANTES DE ENTREGAR:
# 1. Verifica MI_DNI (línea 20)
# 2. Ejecuta todo
# 3. Verifica que no hay errores
# 4. Guarda con nombre que incluye tu DNI / NIE, ej.: Tema02ej1_123456789.R
# 5. Sube a Google Forms
# =============================================================================
