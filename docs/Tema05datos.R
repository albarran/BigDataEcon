# Tema05datos.R -------------------------------------------------------------
#
# Genera los datos del Tema 05 y los guarda en data/tienda.csv.
#
# POR QUE EXISTE (19-oct-2026): el running example del tema es "la tienda y la campaña de
# descuento": 404 clientes de una cadena. El deck hace source() de este guion y el
# ejercicio carga el CSV desde el repo 00datos, como en los Temas 03 y 04. Escribe tambien
# data/cadena.csv: el gasto de los 100.000 clientes (la poblacion de la Parte I).
#
# NO sustituye a data/descuento.csv (el generador anterior, con ruido sd = 50 y R2 ~ 1):
# ese fichero lo siguen usando Tema08.qmd y Tema08ej1.qmd con cifras verificadas.
#
# LO QUE EL ALUMNO NO DEBE LEER es este fichero: aqui esta la forma funcional verdadera
# de cada variable, que es lo que el tema enseña a descubrir. Va en PUBLISH porque el CI
# lo necesita para renderizar el deck.
#
# FORMA FUNCIONAL (cada pieza justifica una transparencia del bloque III):
#   - renta entra en LOGARITMO: rendimientos decrecientes del gasto en la renta
#   - edad entra como CUADRATICA (-3 por anyo al cuadrado, Pedro 2-nov-2026: la curva tiene que
#     verse en la nube de clientes), con maximo a los 45 y caida clara
#     (simetrica) hasta los 70: la misma curva se aproxima con poly(edad, 2) y con cut(edad, ...)
#   - educ entra por TRAMOS, sin pendiente dentro: sube, sube mas, baja y vuelve a subir, con
#     saltos de tamanyo distinto (+200 con 12, +600 con 14, +250 con 16, +500 con 18-20; se muestran con medias por nivel). Cortes para cut(): c(0, 10, 12, 14, 16, 20)
#   - mujer suma 500 de nivel e INTERACTUA con log(renta): 200 frente a 1.700 por log
#   - zona: factor de tres niveles
#   - descuento (0/1) se asigna con probabilidad DECRECIENTE en la renta: a solas su
#     coeficiente sale sesgado hacia abajo (confusion con renta); su efecto real es +500
#   - ruido sd = 300 euros: errores estandar
#     visibles y U del error de prueba visible

rm(list = ls())

library(tidyverse)

if (!dir.exists("data")) dir.create("data")

# Semilla elegida (19-oct-2026) entre 2000 y 3000 para que, con n = 404, las medias por nivel de
# estudios formen tres escalones limpios y el gasto medio crezca de pueblo a ciudad y a capital.
set.seed(2027)

genera <- function(n) {
  renta <- exp(rnorm(n, mean = log(2000), sd = 0.45)) |> round(0)
  renta <- pmin(pmax(renta, 700), 8000)

  edad <- runif(n, min = 20, max = 70) |> round(0)

  mujer <- rbinom(n, size = 1, prob = 0.5)

  educ <- sample(c(8, 10, 12, 14, 16, 18, 20), size = n, replace = TRUE,
                 prob = c(0.08, 0.12, 0.25, 0.15, 0.20, 0.12, 0.08))

  zona <- sample(c("pueblo", "ciudad", "capital"), size = n, replace = TRUE,
                 prob = c(0.25, 0.45, 0.30))

  # El descuento se dio sobre todo a los clientes de menor renta
  p_desc <- plogis(-0.2 - 2.2 * log(renta / 1000))
  descuento <- rbinom(n, size = 1, prob = p_desc)

  ventas <- 3000 +
    200 * log(renta / 1000) +
    - 3 * (edad - 45)^2 +
    200 * (educ == 12) + 600 * (educ == 14) + 250 * (educ == 16) + 500 * (educ >= 18) +
    500 * mujer + 1500 * mujer * log(renta / 1000) +
    500 * descuento +
    200 * (zona == "ciudad") + 450 * (zona == "capital") +
    rnorm(n, mean = 0, sd = 300)

  tibble(ventas = round(ventas, 0), renta, edad, mujer, educ, zona, descuento)
}

# La muestra (404 encuestados) son los primeros de la poblacion: misma secuencia aleatoria
tienda <- genera(404)
resto  <- genera(100000 - 404)
cadena <- bind_rows(tienda, resto) |> slice_sample(prop = 1)

write_csv(tienda, "data/tienda.csv")
# La poblacion: el gasto de los 100.000 clientes de la cadena (solo esa columna)
write_csv(select(cadena, ventas), "data/cadena.csv")
