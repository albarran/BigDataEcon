# Tema04datos.R -------------------------------------------------------------
#
# Genera los datos del Tema 04 y los guarda en data/marketplace.RData.
#
# POR QUE EXISTE (1-oct-2026): los datos estaban generados DENTRO de Tema04.qmd, asi que
# solo existian al renderizar el deck y el alumno no tenia de donde bajarlos para hacer
# el ejercicio. Misma arquitectura que el Tema 03: el deck hace source() de este guion, y
# el ejercicio carga el .RData desde el repo 00datos.
#
# LO QUE EL ALUMNO NO DEBE LEER es justamente este fichero: aqui se ve que filas son el
# error de tecleo (idx_err) y cuales el pedido mayorista (idx_b2b), que es lo que el
# ejercicio pide descubrir. Va en PUBLISH porque el CI lo necesita para renderizar el
# deck, igual que Tema03datos.R.

rm(list = ls())

library(tidyverse)

if (!dir.exists("data")) dir.create("data")

# Datos SIMULADOS: un "volcado" del marketplace tal y como llegaría,
# con los problemas de calidad que veremos en el tema.

set.seed(2026)

n <- 1200

cp_reales  <- c("03003", "03201", "46001", "46700",
                "30001", "28001", "08001", "02001")
prov_sucia <- c("Alicante", "alicante", "ALICANTE", " Alicante ", "Valencia",
                "València", "Murcia", "murcia", "Madrid", "Barcelona")

# --- 1. Tabla de vendedores (con unidades de medida MEZCLADAS) -------------
vendedores <- tibble(
  id_vendedor = sprintf("V%03d", 1:60),
  provincia   = sample(prov_sucia, 60, replace = TRUE),
  sector      = sample(c("Textil", "Electrónica", "Hogar", "Alimentación"),
                       60, replace = TRUE),
  empleados   = pmax(1, rpois(60, 6))
) |>
  mutate(
    fact_euros        = round(empleados * rlnorm(60, meanlog = 10.5, sdlog = 0.5)),
    unidad            = sample(c("euros", "miles"), 60,
                               replace = TRUE, prob = c(0.75, 0.25)),
    facturacion_anual = if_else(unidad == "miles",
                                round(fact_euros / 1000), fact_euros)
  ) |>
  select(id_vendedor, provincia, sector, empleados, facturacion_anual)

# --- 2. Tabla de pedidos ---------------------------------------------------
pedidos <- tibble(
  id_pedido   = sprintf("P%05d", 1:n),
  id_cliente  = sprintf("C%04d", sample(400, n, replace = TRUE)),
  id_vendedor = sprintf("V%03d", sample(60,  n, replace = TRUE)),
  dia         = as.Date("2025-01-01") + sample(0:364, n, replace = TRUE),
  canal       = sample(c("web", "app", "tienda"), n,
                       replace = TRUE, prob = c(0.45, 0.40, 0.15)),
  unidades    = 1 + rpois(n, 1.6),
  importe     = round(rlnorm(n, meanlog = 3.6, sdlog = 0.8), 2),
  cp_envio    = sample(cp_reales, n, replace = TRUE)
)

# 2a. Atípicos: pedidos mayoristas REALES (muchas unidades) y un error de tecleo
idx_b2b <- sample(n, 6)
pedidos$importe[idx_b2b]  <- pedidos$importe[idx_b2b] * 40
pedidos$unidades[idx_b2b] <- pedidos$unidades[idx_b2b] * 30
idx_err <- sample(n, 2)
pedidos$importe[idx_err] <- pedidos$importe[idx_err] * 100

# 2a-bis. PRECIO DE CATALOGO, para que exista una IDENTIDAD que deba cumplirse:
#     importe = unidades * precio_unit.
# Se construye DESPUES de los atipicos y a partir de ellos, asi que ningun valor de
# `importe` ni de `unidades` cambia respecto a lo que ya habia. Para los pedidos
# mayoristas la identidad SI se cumple (crecieron importe y unidades a la vez); para
# los dos errores de tecleo NO, porque solo se multiplico el importe y el precio de
# catalogo del producto es el de siempre. Resultado: la identidad aisla exactamente
# los dos errores reales y exonera a los seis mayoristas.
# Esta es la version marketplace de la identidad contable (activo = pasivo +
# patrimonio) del ejercicio de pymes: el dominio dice que una relacion TIENE que
# cumplirse; si no se cumple, o sobra una variable o los datos estan mal. Y de paso
# ensenya que con dos de las tres columnas se obtiene la tercera: una es redundante.
pedidos$precio_unit <- round(pedidos$importe / pedidos$unidades, 2)
pedidos$precio_unit[idx_err] <- round(pedidos$importe[idx_err] /
                                        (100 * pedidos$unidades[idx_err]), 2)

# 2b. Valoración post-compra: responden MÁS los satisfechos (ausencia MNAR).
#     La valoración media VERDADERA es 7,5 por construcción.
pedidos <- pedidos |>
  mutate(
    val_real   = pmin(10, pmax(1, round(rnorm(n, 7.5, 1.8)))),
    p_resp     = 1 / (1 + exp(-(0.6 + 0.55 * (val_real - 7)))),
    valoracion = if_else(runif(n) < p_resp, val_real, NA_real_),
    # parte de la no respuesta llega codificada con un valor centinela
    valoracion = if_else(is.na(valoracion) & runif(n) < 0.5, -99, valoracion),
    # descuento: NA ESTRUCTURAL (el pedido no entró en promoción)
    descuento  = if_else(runif(n) < 0.7, NA_real_, round(runif(n, 0.05, 0.40), 2)),
    # devuelve más quien quedó insatisfecho, y más por app que en tienda
    ef_canal   = case_when(canal == "web" ~ 0, canal == "app" ~ 0.4,
                           canal == "tienda" ~ -1.5),
    devuelto   = runif(n) < 1 / (1 + exp(-(-2.6 - 0.35 * (val_real - 7.5) + ef_canal)))
  ) |>
  select(-val_real, -p_resp, -ef_canal)

# 2c. "Ensuciar": texto sin homogeneizar, fechas en dos formatos,
#     código postal leído como número (pierde el cero inicial)
ensuciar_mayus <- function(x) {
  r <- sample(1:3, length(x), replace = TRUE, prob = c(0.6, 0.2, 0.2))
  x[r == 2] <- toupper(x[r == 2])
  x[r == 3] <- paste0(" ", x[r == 3], " ")
  x
}

pedidos_bruto <- pedidos |>
  mutate(
    fecha    = if_else(runif(n) < 0.75,
                       format(dia, "%Y-%m-%d"), format(dia, "%d/%m/%Y")),
    canal    = ensuciar_mayus(canal),
    cp_envio = as.numeric(cp_envio)
  ) |>
  select(id_pedido, fecha, id_cliente, id_vendedor, canal, unidades,
         importe, precio_unit, descuento, cp_envio, valoracion, devuelto)

# 2d. Duplicados: exportación repetida (exactos) y mismo pedido con IVA (clave)
dup_exactos <- slice_sample(pedidos_bruto, n = 15)
dup_clave   <- slice_sample(pedidos_bruto, n = 8) |>
  mutate(importe = round(importe * 1.21, 2))

pedidos_bruto <- bind_rows(pedidos_bruto, dup_exactos, dup_clave) |>
  slice_sample(prop = 1) |>
  mutate(importe = format(importe, decimal.mark = ",", big.mark = ".",
                          nsmall = 2, trim = TRUE))

# --- 3. Datos para la Parte II --------------------------------------------

# Serie mensual (tendencia + estacionalidad + ruido)
meses      <- seq(as.Date("2023-01-01"), as.Date("2025-12-01"), by = "month")
estacional <- c(0.92, 0.88, 0.97, 1.00, 1.02, 1.05,
                0.95, 0.80, 1.03, 1.05, 1.25, 1.45)

serie_ventas <- tibble(
  mes     = meses,
  anio    = as.integer(format(meses, "%Y")),
  num_mes = as.integer(format(meses, "%m"))
) |>
  mutate(ventas = round(100000 * 1.004^(row_number() - 1) *
                          estacional[num_mes] * rnorm(n(), 1, 0.03)))

# Ventas por provincia (para el efecto base)
ventas_prov <- tribble(
  ~provincia,  ~ventas_2024, ~ventas_2025,
  "Alicante",        412000,       445000,
  "Valencia",        380000,       399000,
  "Madrid",          915000,       951000,
  "Teruel",            4000,        10000
)

# Devoluciones por canal y categoría (paradoja de Simpson)
devoluciones <- tribble(
  ~categoria,     ~canal, ~pedidos, ~devueltos,
  "Electrónica",   "web",      200,         20,
  "Electrónica",   "app",      800,         96,
  "Textil",        "web",      800,        240,
  "Textil",        "app",      200,         64
)

# --- Guardar -------------------------------------------------------------------
# `pedidos` (la version ya limpia) NO se guarda a proposito: es el resultado de la
# limpieza y el alumno tiene que llegar a el por su cuenta.
save(pedidos_bruto, vendedores, serie_ventas, ventas_prov, devoluciones,
     file = "data/marketplace.RData")

cat(
  "data/marketplace.RData guardado:", nrow(pedidos_bruto), "pedidos,",
  nrow(vendedores), "vendedores,", nrow(serie_ventas), "meses de serie\n"
)
