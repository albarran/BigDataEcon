rm(list = ls())

library(tidyverse)

if (!dir.exists("data")) dir.create("data")
set.seed(2085)

# ========================================
# CATEGORIAS
# Clave: categoria (numérico)
# ========================================
categorias <- data.frame(
  categoria = 1:8,
  nombre_categoria = c("Dispositivos Electrónicos", "Moda y Vestimenta", 
                       "Artículos del Hogar", "Material Deportivo",
                       "Productos Alimenticios", "Juguetería", 
                       "Librería y Papelería", "Cosmética y Cuidado Personal"),
  margen_objetivo = c(0.25, 0.45, 0.35, 0.40, 0.15, 0.35, 0.30, 0.50)
)

# ========================================
# PRODUCTOS
# Se une a CATEGORIAS por: id_categoria -> categoria
# ========================================
productos <- data.frame(
  id_producto = 1:150,
  nombre_producto = paste0("Producto_", sprintf("%03d", 1:150)),
  id_categoria = sample(1:8, 150, replace = TRUE),
  precio = round(runif(150, 10, 500), 2),
  costo = round(runif(150, 5, 400), 2),
  activo = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.9, 0.1))
) %>%
  mutate(precio = ifelse(precio < costo, costo * 1.2, precio))

# ========================================
# TIENDAS
# ========================================
tiendas <- data.frame(
  id_tienda = 1:12,
  nombre_tienda = c("Madrid Centro", "Barcelona Diagonal", "Valencia Puerto",
                    "Sevilla Este", "Bilbao Centro", "Málaga Costa",
                    "Zaragoza Plaza", "Alicante Playa", "Valladolid Norte",
                    "Vigo Mar", "Gijón Centro", "Murcia Sur"),
  region = c("Centro", "Nordeste", "Levante", "Sur", "Norte", "Sur",
             "Centro", "Levante", "Norte", "Noroeste", "Norte", "Levante"),
  tamaño_m2 = c(800, 1200, 600, 750, 900, 650, 700, 850, 500, 550, 600, 700)
)

# ========================================
# EMPLEADOS
# ========================================
empleados <- data.frame(
  id_empleado = 1:100,
  nombre = paste0("Empleado_", 1:100),
  id_tienda = c(sample(1:12, 85, replace = TRUE), rep(NA, 15)),
  puesto = sample(c("Vendedor", "Supervisor", "Gerente", "Almacén"),
                  100, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1)),
  salario_mensual = round(rnorm(100, 1800, 400), 0)
) %>%
  mutate(salario_mensual = pmax(salario_mensual, 1100))

# ========================================
# CLIENTES
# ========================================
clientes <- data.frame(
  id_cliente = 1:2500,
  email = paste0("cliente", 1:2500, "@email.com"),
  programa_fidelidad = sample(c("Básico", "Silver", "Gold", "Platinum", NA),
                              2500, replace = TRUE, 
                              prob = c(0.4, 0.25, 0.15, 0.05, 0.15))
)

# ========================================
# PROMOCIONES_EMPLEADO (Clave compuesta: cod_tienda + cod_empleado)
# Se une a EMPLEADOS usando DOS variables:
#   cod_tienda -> id_tienda
#   cod_empleado -> id_empleado
# ========================================
empleados_con_promocion <- empleados %>%
  filter(!is.na(id_tienda)) %>%
  sample_n(40)

promociones_empleado <- empleados_con_promocion %>%
  select(id_empleado, id_tienda) %>%
  mutate(
    cod_empleado = id_empleado,
    cod_tienda = id_tienda,
    tipo_bono = sample(c("Ventas", "Antigüedad", "Desempeño"), n(), replace = TRUE),
    monto_bono = round(rnorm(n(), 500, 200), 0),
    fecha_otorgado = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
                            n(), replace = TRUE)
  ) %>%
  select(cod_tienda, cod_empleado, tipo_bono, monto_bono, fecha_otorgado)

# ========================================
# VENTAS (tabla central)
# ========================================
fechas_ventas <- c(
  sample(seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day"),
         1000, replace = TRUE),
  sample(seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day"),
         1500, replace = TRUE),
  sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
         2000, replace = TRUE),
  sample(seq(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day"),
         500, replace = TRUE)
)

ventas <- data.frame(
  id_venta = 1:5000,
  fecha = sample(fechas_ventas, 5000, replace = FALSE),
  id_cliente = sample(clientes$id_cliente, 5000, replace = TRUE),
  id_producto = sample(productos$id_producto, 5000, replace = TRUE),
  id_tienda = sample(tiendas$id_tienda, 5000, replace = TRUE),
  id_empleado = sample(empleados$id_empleado[1:85], 5000, replace = TRUE),
  cantidad = sample(1:5, 5000, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.07, 0.03)),
  descuento_porcentaje = sample(c(0, 5, 10, 15, 20, 25),
                                5000, replace = TRUE,
                                prob = c(0.5, 0.2, 0.15, 0.1, 0.04, 0.01))
) %>%
  left_join(productos %>% select(id_producto, precio), by = "id_producto") %>%
  mutate(
    precio_unitario = precio,
    subtotal = cantidad * precio_unitario,
    descuento_aplicado = round(subtotal * descuento_porcentaje / 100, 2),
    total = subtotal - descuento_aplicado,
    año = year(fecha),
    mes = month(fecha),
    trimestre = quarter(fecha),
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1)
  ) %>%
  select(-precio)

# ========================================
# DEVOLUCIONES
# ========================================
ventas_con_devolucion <- sample(ventas$id_venta, 
                                round(nrow(ventas) * 0.06), 
                                replace = FALSE)

devoluciones <- ventas %>%
  filter(id_venta %in% ventas_con_devolucion) %>%
  mutate(
    id_devolucion = row_number(),
    fecha_devolucion = fecha + sample(1:30, n(), replace = TRUE),
    motivo = sample(c("Defectuoso", "No satisfecho", "Talla incorrecta", 
                      "Color diferente", "Cambio de opinión"),
                    n(), replace = TRUE,
                    prob = c(0.15, 0.25, 0.20, 0.15, 0.25)),
    cantidad_devuelta = pmin(cantidad, sample(1:3, n(), replace = TRUE)),
    reembolso = round(total * (cantidad_devuelta / cantidad), 2),
    procesado = sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.9, 0.1))
  ) %>%
  select(id_devolucion, id_venta, fecha_devolucion, motivo, 
         cantidad_devuelta, reembolso, procesado)

# ========================================
# VENTAS_ANCHO (para pivoting)
# ========================================
ventas_ancho <- tibble(
  tienda = c("Madrid", "Barcelona", "Valencia"),
  Q1 = c(145, 152, 138),
  Q2 = c(158, 164, 151),
  Q3 = c(151, 156, 149),
  Q4 = c(169, 175, 162)
)

# ========================================
# INFORMACIÓN DE VARIABLES
# ========================================
ventas_info <- data.frame(
  variable = c("id_venta", "fecha", "id_cliente", "id_producto", "id_tienda",
               "id_empleado", "cantidad", "descuento_porcentaje", "precio_unitario",
               "subtotal", "descuento_aplicado", "total", "año", "mes",
               "trimestre", "dia_semana"),
  descripcion = c(
    "Identificador único de la transacción de venta",
    "Fecha de la venta (formato YYYY-MM-DD)",
    "Identificador del cliente que realizó la compra",
    "Identificador del producto vendido",
    "Identificador de la tienda donde se realizó la venta",
    "Identificador del empleado que atendió la venta",
    "Número de unidades vendidas",
    "Porcentaje de descuento aplicado (0-25%)",
    "Precio por unidad del producto en euros",
    "Precio total antes de descuento (cantidad × precio_unitario)",
    "Importe del descuento aplicado en euros",
    "Precio final pagado por el cliente en euros",
    "Año de la venta",
    "Mes de la venta (1-12)",
    "Trimestre del año (1-4)",
    "Día de la semana de la venta"
  ),
  tipo = c("Entero", "Fecha", "Entero", "Entero", "Entero", "Entero",
           "Entero", "Numérica", "Numérica", "Numérica", "Numérica",
           "Numérica", "Entero", "Entero", "Entero", "Factor")
)

productos_info <- data.frame(
  variable = c("id_producto", "nombre_producto", "id_categoria", "precio", "costo", "activo"),
  descripcion = c(
    "Identificador único del producto",
    "Nombre comercial del producto",
    "Código de categoría (se une con categoria en tabla categorias)",
    "Precio de venta al público en euros",
    "Costo de adquisición/producción en euros",
    "Indica si el producto está activo en catálogo (TRUE/FALSE)"
  ),
  tipo = c("Entero", "Carácter", "Entero", "Numérica", "Numérica", "Lógico")
)

categorias_info <- data.frame(
  variable = c("categoria", "nombre_categoria", "margen_objetivo"),
  descripcion = c(
    "Código numérico de categoría (clave primaria, valores 1-8)",
    "Nombre descriptivo de la categoría",
    "Margen de beneficio objetivo para la categoría (proporción)"
  ),
  tipo = c("Entero", "Carácter", "Numérica")
)

clientes_info <- data.frame(
  variable = c("id_cliente", "email", "programa_fidelidad"),
  descripcion = c(
    "Identificador único del cliente",
    "Dirección de correo electrónico del cliente",
    "Nivel en el programa de fidelidad (Básico/Silver/Gold/Platinum/NA)"
  ),
  tipo = c("Entero", "Carácter", "Factor")
)

empleados_info <- data.frame(
  variable = c("id_empleado", "nombre", "id_tienda", "puesto", "salario_mensual"),
  descripcion = c(
    "Identificador único del empleado",
    "Nombre del empleado",
    "Identificador de la tienda donde trabaja (NA si sin asignar)",
    "Puesto de trabajo (Vendedor/Supervisor/Gerente/Almacén)",
    "Salario mensual bruto en euros"
  ),
  tipo = c("Entero", "Carácter", "Entero", "Factor", "Numérica")
)

tiendas_info <- data.frame(
  variable = c("id_tienda", "nombre_tienda", "region", "tamaño_m2"),
  descripcion = c(
    "Identificador único de la tienda",
    "Nombre comercial de la tienda",
    "Región geográfica (Centro/Norte/Sur/Nordeste/Levante/Noroeste)",
    "Superficie de la tienda en metros cuadrados"
  ),
  tipo = c("Entero", "Carácter", "Factor", "Numérica")
)

promociones_empleado_info <- data.frame(
  variable = c("cod_tienda", "cod_empleado", "tipo_bono", "monto_bono", "fecha_otorgado"),
  descripcion = c(
    "Código de tienda (clave primaria compuesta, se une con id_tienda en empleados)",
    "Código de empleado (clave primaria compuesta, se une con id_empleado en empleados)",
    "Tipo de bono otorgado (Ventas/Antigüedad/Desempeño)",
    "Monto del bono en euros",
    "Fecha en que se otorgó el bono"
  ),
  tipo = c("Entero", "Entero", "Factor", "Numérica", "Fecha")
)

devoluciones_info <- data.frame(
  variable = c("id_devolucion", "id_venta", "fecha_devolucion", "motivo",
               "cantidad_devuelta", "reembolso", "procesado"),
  descripcion = c(
    "Identificador único de la devolución",
    "Identificador de la venta original asociada",
    "Fecha en que se procesó la devolución",
    "Motivo de la devolución (Defectuoso/No satisfecho/etc.)",
    "Cantidad de unidades devueltas",
    "Importe reembolsado al cliente en euros",
    "Indica si la devolución ha sido procesada (TRUE/FALSE)"
  ),
  tipo = c("Entero", "Entero", "Fecha", "Factor", "Entero", "Numérica", "Lógico")
)

# ========================================
# GUARDAR
# ========================================
save(ventas, productos, categorias, clientes, empleados, tiendas, 
     promociones_empleado, devoluciones, ventas_ancho,
     ventas_info, productos_info, categorias_info, clientes_info, 
     empleados_info, tiendas_info, promociones_empleado_info, devoluciones_info,
     file = "data/retail_data.RData")

# load("data/retail_data.RData")

# write_csv(ventas, "data/ventas.csv")
# write_csv(productos, "data/productos.csv")
# write_csv(categorias, "data/categorias.csv")
# write_csv(clientes, "data/clientes.csv")
# write_csv(empleados, "data/empleados.csv")
# write_csv(tiendas, "data/tiendas.csv")
# write_csv(promociones_empleado, "data/promociones_empleado.csv")
# write_csv(devoluciones, "data/devoluciones.csv")
