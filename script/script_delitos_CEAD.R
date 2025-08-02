### TASA DE DELITOS REGISTRADOS REGIONES DE ATACAMA Y COQUIMBO ###

# ==========================================
# PASO 0: Descripción del proyecto
# ==========================================
"
 Este análisis utiliza datos del Centro de Estudios y Análisis del Delito (CEAD)
 para examinar la evolución y distribución de los delitos registrados en las regiones 
 de Atacama y Coquimbo entre los años 2013 y 2024. 
 Se abordan cinco objetivos principales que permiten una lectura crítica del fenómeno delictual.
"
# ==========================================
# PASO 1: Instalar y cargar paquetes
# ==========================================

# 1.1 Instalar paquetes

install.packages("tidyverse")
install.packages("readxl")
install.packages("ggrepel")


# ------------------------------------------

# 1.2 Cargar las librerias 

library(tidyverse)
library(readxl)
library(ggrepel)

# ==========================================
# PASO 2: Definir y verificar directorio de trabajo
# ==========================================

# 2.1 Ver directorio actual

getwd()

# ------------------------------------------

# 2.2 Cambiar directorio de trabajo (ajusta la ruta)

setwd("C:\\Users\\brook\\OneDrive\\tablas_delitos_CEAD")

# ------------------------------------------

# 2.3 Verificar que se cambió

getwd()

# ==========================================
# PASO 3: Leer archivo Excel
# ==========================================

# 3.1 Visualizamos los archivos cargados en el directorio

list.files()

# 3.2 Cargar una hoja específica

delitos_atacama <- read_excel("reportesEstadisticos-porDelito_Atacama.xlsx")

delitos_coquimbo <- read_excel("reportesEstadisticos-porDelito_Coquimbo.xlsx")

# ==========================================
# PASO 4: Visualizar los datos cargados
# ==========================================

# 4.1 Ver primeras filas

head(delitos_atacama)
head(delitos_coquimbo)

# ------------------------------------------

# 4.2 Ver estructura del dataframe

str(delitos_atacama)
str(delitos_coquimbo)

# ------------------------------------------

# 4.3 Ver resumen estadístico básico

summary(delitos_atacama)
summary(delitos_coquimbo)

# ------------------------------------------

# 4.4 Visualizamos tipo tabla en consola

View(delitos_atacama)
View(delitos_coquimbo)

# ==========================================
# PASO 5: Limpiar y normalizar los datos
# ==========================================

# 5.1 Eliminamos metadatos superiores para encabezados

delitos_atacama <- read_excel(     
  "reportesEstadisticos-porDelito_Atacama.xlsx",
  skip = 20
)

delitos_coquimbo <- read_excel(     
  "reportesEstadisticos-porDelito_Coquimbo.xlsx",
  skip = 20
)

# ------------------------------------------

# 5.2 Cambiamos nombre de la primera columna

delitos_atacama <- delitos_atacama %>%
  rename("Tipos_delito" = ...1)

delitos_coquimbo <- delitos_coquimbo %>%
  rename("Tipos_delito" = ...1)

# ------------------------------------------

# 5.3 Elimiar filas innecesarias

delitos_atacama <- delitos_atacama[-1, ]

delitos_coquimbo <- delitos_coquimbo[-1, ]

# ------------------------------------------

# 5.4 Creamos una copia para poder manipular en otra plataforma

write.csv(delitos_atacama, "delitos_atacama.csv", row.names = FALSE)

write.csv(delitos_coquimbo,"delitos_coquimbo.csv", row.names = FALSE)

# ==========================================
# PASO 6: Manipulación de datos
# ==========================================

# 6.1 Agregamos la columna "Región" como variable categórica

delitos_atacama <- delitos_atacama %>% mutate(Region = "Atacama")

delitos_coquimbo <- delitos_coquimbo %>% mutate(Region = "Coquimbo")

# ------------------------------------------

# 6.2 Desplazamos al inicio la variable "Región"

delitos_atacama <- delitos_atacama %>%
  select(Region,Tipos_delito,everything())

delitos_coquimbo <- delitos_coquimbo %>%
  select(Region,Tipos_delito,everything())

# ------------------------------------------

# 6.3 Juntamos ambas tablas

delitos_ambas_regiones <- bind_rows(delitos_atacama, delitos_coquimbo)

# ------------------------------------------

# 6.4 Vista rapida de la nueva tabla

str(delitos_ambas_regiones)

summary(delitos_ambas_regiones)

View(delitos_ambas_regiones)

# ------------------------------------------

# 6.4 Creamos una copia

write.csv(delitos_ambas_regiones,"delitos_ambas_regiones.csv", row.names = FALSE)

# ==========================================
# PASO 7: Manipulación de datos avanzada
# ==========================================

"
En este paso vamos a preparar la información para poder llevar a cabo los 
objetivos del trabajo:

Objetivo 1: Identificar los grupos delictuales más frecuentes por región
Objetivo 2: Analizar la evolución temporal total delictual por región
Objetivo 3: Detectar delitos en aumento sostenido o con repuntes recientes
Objetivo 4: Comparar Atacama vs Coquimbo en delitos de alto impacto social
Objetivo 5: Composición delictual por región (año más reciente)

Para ello vamos a distinguir entre grupos y subgrupos de delitos registrados
"

# 7.1 Distinguimos entre "Familias de delitos","Grupos" y "Subgrupo" de delitos

#7.1.1 Agrupamos jerarquía

delitos_ambas_regiones <- delitos_ambas_regiones %>%
  mutate(
    Ordenado_por = case_when(
      str_detect(Tipos_delito, "^\\s{2,}") ~ "Subgrupo",  # Sangría de más de 2 espacios
      str_detect(Tipos_delito, "^\\s") ~ "Grupo",         # Sangría de 1 espacio
      TRUE ~ "Familia de delitos"
    )
  )

# ------------------------------------------

# 7.1.2 Eliminamos el espacio en los grupos para normalizar los nombres

delitos_ambas_regiones <- delitos_ambas_regiones %>%
  mutate(
    Tipos_delito = str_trim(Tipos_delito, side = "left")  # elimina solo espacios a la izquierda
  )

# ------------------------------------------

# 7.1.3 Desplazamos las columnas categoricas al inicio

delitos_ambas_regiones <- delitos_ambas_regiones %>%
  select(Region,Tipos_delito,Ordenado_por,everything())

# ------------------------------------------

# 7.2 Hacemos tablas resúmenes

# 7.2.1 Tabla solo de Familias

tabla_resumen_familias <- delitos_ambas_regiones %>% filter(Ordenado_por == "Familia de delitos")

# ------------------------------------------

# 7.2.2 Tabla solo de Grupos

tabla_resumen_grupos <- delitos_ambas_regiones %>% filter(Ordenado_por == "Grupo")

# ------------------------------------------

# 7.2.3 Tabla solo de Subgrupos

tabla_resumen_subgrupo <- delitos_ambas_regiones %>% filter(Ordenado_por == "Subgrupo")

# ------------------------------------------

# 7.3 Creamos columnas separadas según la jerarquía

delitos_ambas_regiones <- delitos_ambas_regiones %>%
  mutate(
    Familia_delito   = if_else(Ordenado_por == "Familia de delitos", Tipos_delito, NA_character_),
    Grupo_delito     = if_else(Ordenado_por == "Grupo",   Tipos_delito, NA_character_),
    Subgrupo_delito  = if_else(Ordenado_por == "Subgrupo", Tipos_delito, NA_character_)
  ) %>%
  fill(Familia_delito, Grupo_delito, Subgrupo_delito, .direction = "down")

# ------------------------------------------

# 7.4 Respaldamos las tablas en formato csv.

write.csv(tabla_resumen_familias, "tabla_familias.csv", row.names = FALSE)

write.csv(tabla_resumen_grupos, "tabla_grupos.csv", row.names = FALSE)

write.csv(tabla_resumen_subgrupo, "tabla_subgrupos.csv", row.names = FALSE)

# ------------------------------------------

# Objetivo 1: Identificar los grupos delictuales más frecuentes por región

# Calculamos el total de delitos por grupo y región

resumen_grupos <- tabla_resumen_grupos %>%
  select(Region, Tipos_delito, `2013`:`2024`) %>%
  group_by(Region, Tipos_delito) %>%
  summarise(Total = sum(across(`2013`:`2024`)), .groups = "drop") %>%
  arrange(Region, desc(Total))

# Mostramos los primeros resultados por región

resumen_grupos %>% filter(Region == "Atacama") %>% head(5)
resumen_grupos %>% filter(Region == "Coquimbo") %>% head(5)

# ------------------------------------------

# Objetivo 2: Evolución temporal total delictual por región

# Calculamos totales anuales por región (sumando todas las familias)

totales_anuales <- tabla_resumen_familias %>%
  select(Region, `2013`:`2024`) %>%
  group_by(Region) %>%
  summarise(across(`2013`:`2024`, sum))

# Convertimos a formato largo para graficar



# ------------------------------------------

# Objetivo 3: Delitos en aumento sostenido o con repuntes recientes

# Calcular el cambio porcentual entre 2013 y 2024
tendencia <- tabla_resumen_subgrupo %>%
  select(Region, Tipos_delito, `2013`:`2024`) %>%
  rowwise() %>%
  mutate(
    CambioPorc = case_when(
      `2013` == 0 & `2024` > 0 ~ NA_real_,  # Se etiquetará como "Nuevo delito" luego
      `2013` == 0 & `2024` == 0 ~ 0,
      TRUE ~ (`2024` - `2013`) / `2013` * 100
    )
  ) %>%
  ungroup() %>%
  mutate(
    Tendencia = case_when(
      is.na(CambioPorc) ~ "Nuevo delito",
      CambioPorc > 100 ~ "Aumento fuerte",
      CambioPorc > 50  ~ "Aumento moderado",
      CambioPorc > 0   ~ "Aumento leve",
      CambioPorc == 0  ~ "Sin cambio",
      CambioPorc < 0   ~ "Disminución"
    )
  )

# Filtrar delitos que hayan tenido algún tipo de aumento o que sean nuevos
aumento <- tendencia %>%
  filter(Tendencia %in% c("Nuevo delito", "Aumento leve", "Aumento moderado", "Aumento fuerte"))

# Ver los 10 casos más notorios (ordenado por valor numérico si está disponible)
head(
  aumento %>%
    arrange(desc(CambioPorc)),
  10
)

# ------------------------------------------

# Objetivo 4: Comparar Atacama vs Coquimbo en delitos de alto impacto social

# Definir vector de delitos de alto impacto (según nomenclatura de nuestros datos)

delitos_impacto <- c("Homicidios", "Femicidios", "Violaciones", 
                     "Robos con violencia o intimidación", "Violencia intrafamiliar con lesiones físicas",
                     "Tráfico de sustancias","Porte / posesión de armas o explosivos")

# Filtrar en subgrupos para detalle

sub_impac <- tabla_resumen_subgrupo %>%
  filter(Tipos_delito %in% delitos_impacto)

# Sumar casos recientes (por ejemplo promedio últimos 3 años) por región y delito

resumen_impac <- sub_impac %>%
  select(Region, Tipos_delito, `2022`, `2023`, `2024`) %>%
  rowwise() %>%
  mutate(Promedio = mean(c(`2022`,`2023`,`2024`))) %>%
  ungroup() %>%
  select(Region, Tipos_delito, Promedio)

# ------------------------------------------

# Objetivo 5: Composición delictual por región (año más reciente)

comp_porcentaje <- tabla_resumen_familias %>%
  select(Region, Tipos_delito, `2024`) %>%
  group_by(Region) %>%
  mutate(TotalRegion = sum(`2024`),
         Porcentaje = `2024` / TotalRegion * 100) %>%
  ungroup()

"
Se crearon los diferentes objetos 'resumen_grupos', 'totales', 
'totales_largo', 'tendencia', 'aumento','resumen_impac' que 
corresponden a cada objetivo.Estos objetos serán la base del Paso 8
"
# ==========================================
# PASO 8: Estadisticas exploratorias 
# ==========================================

# 8.1 Estadísticas exploratorias para cada tabla

# 8.1.1 Resumen general

summary(tabla_resumen_familias)

summary(tabla_resumen_grupos)

summary(tabla_resumen_subgrupo)

# ------------------------------------------

# 8.1.2 Conteo de categorías por Grupo_delito

table(tabla_resumen_familias$Tipos_delito, tabla_resumen_familias$Region)

table(tabla_resumen_grupos$Tipos_delito, tabla_resumen_grupos$Region)

table(tabla_resumen_subgrupo$Tipos_delito, tabla_resumen_subgrupo$Region)

# ------------------------------------------

# 8.1.3 Suma total por año

totales_x_anio_familia <- colSums(tabla_resumen_familias[, as.character(2013:2024)], na.rm = TRUE)

totales_x_anio_grupo <- colSums(tabla_resumen_grupos[, as.character(2013:2024)], na.rm = TRUE)

totales_x_anio_subgrupo <-colSums(tabla_resumen_subgrupo[, as.character(2013:2024)], na.rm = TRUE)

# ------------------------------------------

# 8.1.4 Promedio por año

colMeans(tabla_resumen_familias[, as.character(2013:2024)], na.rm = TRUE)

colMeans(tabla_resumen_grupos[, as.character(2013:2024)], na.rm = TRUE)

colMeans(tabla_resumen_subgrupo[, as.character(2013:2024)], na.rm = TRUE)

# ------------------------------------------

# 8.1.5 Suma total por fila

tabla_resumen_familias$total <- rowSums(tabla_resumen_familias[, as.character(2013:2024)], na.rm = TRUE)
summary(tabla_resumen_familias$total)

tabla_resumen_grupos$total <- rowSums(tabla_resumen_grupos[, as.character(2013:2024)], na.rm = TRUE)
summary(tabla_resumen_grupos$total)

tabla_resumen_subgrupo$total <- rowSums(tabla_resumen_subgrupo[, as.character(2013:2024)], na.rm = TRUE)
summary(tabla_resumen_subgrupo$total)

# ------------------------------------------

# 8.2 Gráficos de las estadísticas exploratorias 

# Gráfico 1: Suma total de delitos por año (combinando las tres tablas)

plot(as.numeric(names(totales_x_anio_familia)), 
     totales_x_anio_familia, 
     type = "l", 
     col = "black",
     main = "Familias (2013-2024)",
     xlab = "Año", 
     ylab = "Total de delitos",
     lwd = 2)

plot(as.numeric(names(totales_x_anio_grupo)), 
     totales_x_anio_grupo, 
     type = "l", 
     col = "red",
     main = "Grupos (2013-2024)",
     xlab = "Año", 
     ylab = "Total de delitos",
     lwd = 2)

plot(as.numeric(names(totales_x_anio_subgrupo)), 
     totales_x_anio_subgrupo, 
     type = "l", 
     col = "blue",
     main = "Subgrupos (2013-2024)",
     xlab = "Año", 
     ylab = "Total de delitos",
     lwd = 2)

# ------------------------------------------

# Gráfico 2: Boxplot de la distribución de totales por fila

boxplot(list(Familias = tabla_resumen_familias$total,
             Grupos = tabla_resumen_grupos$total,
             Subgrupos = tabla_resumen_subgrupo$total),
        col = c("lightblue", "salmon", "lightgreen"),
        main = "Distribución de totales por categoría",
        ylab = "Total de delitos por categoría")

# ------------------------------------------

# Gráfico 3: Torta para Familias (2024):
  
datos_familias <- aggregate(tabla_resumen_familias$"2024", 
                            by = list(tabla_resumen_familias$Tipos_delito), 
                            FUN = sum, na.rm = TRUE)

pie(datos_familias$x,
    labels = paste0(datos_familias$Group.1, "\n", datos_familias$x),
    main = "Distribución de delitos por familia (2024)",
    col = rainbow(nrow(datos_familias)),  # Colores distintos
    border = "white")

# ------------------------------------------

# Gráfico 4: Torta para Grupos (2024):

datos_grupos <- aggregate(tabla_resumen_grupos$"2024", 
                          by = list(tabla_resumen_grupos$Tipos_delito), 
                          FUN = sum, na.rm = TRUE)

pie(datos_grupos$x,
    labels = paste0(datos_grupos$Group.1, "\n", datos_grupos$x),
    main = "Distribución de delitos por grupo (2024)",
    col = heat.colors(nrow(datos_grupos)),  # Paleta cálida
    border = "white")
# ------------------------------------------

# Gráfico 5: Torta para Subgrupos (2024):

datos_subgrupos <- aggregate(tabla_resumen_subgrupo$"2024", 
                             by = list(tabla_resumen_subgrupo$Tipos_delito), 
                             FUN = sum, na.rm = TRUE)
pie(datos_subgrupos$x,
    labels = datos_subgrupos$Group.1,
    main = "Distribución de delitos por subgrupo (2024)",
    col = terrain.colors(nrow(datos_subgrupos)),  # Paleta tierra
    border = "white")
legend("topright", 
       legend = paste0(round(datos_subgrupos$x/sum(datos_subgrupos$x)*100, "%"), 
                       fill = terrain.colors(nrow(datos_subgrupos))))

# ==========================================
# PASO 9: Estadisticas avanzadas
# ==========================================

"
Nos vamos a ceñir a los objetivos ya expuestos en el paso 7 y los objetos creados
para tal cometido.
"

# Objetivo 1: Identificar los grupos delictuales más frecuentes por región

# Ranking por región con percentiles

resumen_grupos <- resumen_grupos %>%
  group_by(Region) %>%
  mutate(
    Percentil = ntile(Total, 100),  # Divide en 100 percentiles
    Ranking = rank(-Total)          # Ranking descendente
  ) %>%
  ungroup()

# Mostrar top 10 por región con percentil y ranking

resumen_grupos %>%
  filter(Ranking <= 10) %>%
  arrange(Region, Ranking)

# ------------------------------------------

# Objetivo 2: Analizar la evolución temporal total delictual por región

# Tendencia con regresión lineal simple por región

modelo_atacama <- lm(Total ~ as.numeric(Año), data = filter(totales_largo, Region == "Atacama"))
modelo_coquimbo <- lm(Total ~ as.numeric(Año), data = filter(totales_largo, Region == "Coquimbo"))

summary(modelo_atacama)  # Muestra coeficiente y significancia
summary(modelo_coquimbo)

# Opcional: Graficar líneas de tendencia

plot(Total ~ as.numeric(Año), data = filter(totales_largo, Region == "Atacama"), type = "b", col = "blue")
abline(modelo_atacama, col = "red", lwd = 2)

plot(Total ~ as.numeric(Año), data = filter(totales_largo, Region == "Coquimbo"), type = "b", col = "blue")
abline(modelo_coquimbo, col = "red", lwd = 2)

# ------------------------------------------

# Objetivo 3: Detectar delitos en aumento sostenido o con repuntes recientes

# Clasificar en grupos con K-means (opcional) o con reglas

tabla_tendencias <- tendencia %>%  # Usamos tendencia para etiquetar
  group_by(Tendencia) %>%
  summarise(Casos = n()) %>%
  arrange(desc(Casos))

# También podemos contar por región:

table(tendencia$Region, tendencia$Tendencia)

# O podemos sacar top 5 delitos crecientes por región:

aumento %>%
  group_by(Region) %>%
  slice_max(order_by = CambioPorc, n = 5)

# ------------------------------------------

# Objetivo 4: Comparar Atacama vs Coquimbo en delitos de alto impacto

# Diferencia absoluta y relativa entre regiones

comparacion_impacto <- resumen_impac %>%
  pivot_wider(names_from = Region, values_from = Promedio) %>%
  mutate(
    Diferencia = Coquimbo - Atacama,
    Relativo = round((Coquimbo - Atacama) / Atacama * 100, 2)
  )

# También podemos ordenar por diferencia

comparacion_impacto %>% arrange(desc(abs(Diferencia)))

# ------------------------------------------

# Objetivo 5: Composición delictual por región (último año)
  
# Medida de concentración: índice de Herfindahl

herfindahl <- comp_porcentaje %>%
  group_by(Region) %>%
  summarise(Indice_Herfindahl = sum((Porcentaje / 100)^2)) %>%
  ungroup()

# ==========================================
# PASO 10: Visualización
# ==========================================

# Gráfico 1: líneas comparativo

# Creamos un data frame para el gráfico directamente desde las tres tablas

df_lineas <- data.frame(
  Año = as.integer(colnames(tabla_resumen_familias)[colnames(tabla_resumen_familias) %in% as.character(2013:2024)]),
  Familias = colSums(tabla_resumen_familias[, as.character(2013:2024)]),
  Grupos = colSums(tabla_resumen_grupos[, as.character(2013:2024)]),
  Subgrupos = colSums(tabla_resumen_subgrupo[, as.character(2013:2024)])
)

# Gráfico

ggplot(df_lineas, aes(x = Año)) +
  geom_line(aes(y = Familias, color = "Familias"), size = 1.2) +
  geom_line(aes(y = Grupos, color = "Grupos"), size = 1.2) +
  geom_line(aes(y = Subgrupos, color = "Subgrupos"), size = 1.2) +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) +
  labs(
    title = "Evolución total de delitos por nivel de agregación",
    x = "Año",
    y = "Total de delitos",
    color = "Nivel"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# ------------------------------------------

# Gráfico 2: barras de promedios

# Creamos un data frame con los promedios

df_promedios <- data.frame(
  Año = as.integer(colnames(tabla_resumen_familias)[colnames(tabla_resumen_familias) %in% as.character(2013:2024)]),
  Familias = colMeans(tabla_resumen_familias[, as.character(2013:2024)]),
  Grupos = colMeans(tabla_resumen_grupos[, as.character(2013:2024)]),
  Subgrupos = colMeans(tabla_resumen_subgrupo[, as.character(2013:2024)])
)

# Gráfico

ggplot(df_promedios, aes(x = factor(Año))) +
  geom_bar(aes(y = Familias, fill = "Familias"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Grupos, fill = "Grupos"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Subgrupos, fill = "Subgrupos"), stat = "identity", position = "dodge") +
  labs(
    title = "Promedio de delitos por año y nivel de agregación",
    x = "Año",
    y = "Promedio de delitos",
    fill = "Nivel"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ------------------------------------------

# Gráfico 3:  líneas con tendencia (regresión lineal)

# Usamos el mismo df_lineas creado antes

ggplot(df_lineas, aes(x = Año)) +
  geom_line(aes(y = Familias, color = "Familias"), size = 1.2) +
  geom_line(aes(y = Grupos, color = "Grupos"), size = 1.2) +
  geom_line(aes(y = Subgrupos, color = "Subgrupos"), size = 1.2) +
  geom_smooth(aes(y = Familias, color = "Familias"), method = "lm", se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = Grupos, color = "Grupos"), method = "lm", se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = Subgrupos, color = "Subgrupos"), method = "lm", se = FALSE, linetype = "dashed") +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) +
  labs(
    title = "Tendencia temporal del total de delitos (con regresión lineal)",
    x = "Año",
    y = "Total de delitos",
    color = "Nivel"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ------------------------------------------

#  Gráfico 4: Heatmap de delitos por región y año (nivel de grupo o subgrupo)

# Tomamos los datos de grupos

heatmap_data <- tabla_resumen_grupos %>%
  pivot_longer(cols = `2013`:`2024`, names_to = "Año", values_to = "Total") %>%
  mutate(Año = as.integer(Año))

# Graficamos

ggplot(heatmap_data, aes(x = Año, y = reorder(Tipos_delito, -Total), fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(~Region) +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) +
  labs(
    title = "Matriz de intensidad delictual por grupo y año",
    x = "Año",
    y = "Grupo de delito",
    fill = "Total"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# ------------------------------------------

# Gráfico 5: Barras horizontales - Top 10 delitos con mayor aumento porcentual (Objetivo 3)

aumento_top <- aumento %>%
  filter(!is.na(CambioPorc)) %>%
  group_by(Region) %>%
  slice_max(order_by = CambioPorc, n = 5) %>%
  ungroup()

ggplot(aumento_top, aes(x = reorder(Tipos_delito, CambioPorc), y = CambioPorc, fill = Region)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Region, scales = "free_y") +
  labs(
    title = "Top 5 delitos con mayor aumento porcentual (2013-2024)",
    x = "Tipo de delito",
    y = "% de aumento",
    fill = "Región"
  ) +
  theme_minimal()

# ------------------------------------------

# Gráfico 6: Gráfico de barras apiladas - Composición delictual regional (2024)

comp_grafico <- comp_porcentaje %>%
  filter(!is.na(`2024`)) %>%
  mutate(Familia = Tipos_delito)

ggplot(comp_grafico, aes(x = Region, y = Porcentaje, fill = Familia)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Composición delictual por región (2024)",
    x = "Región",
    y = "Porcentaje",
    fill = "Familia de delito"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ==========================================
# PASO 11: Conclusiones
# ==========================================

" 
Conclusiones generales del análisis:

 1. La región de Coquimbo presenta una tendencia al alza en el total de delitos reportados
    entre 2013 y 2024, mientras que Atacama muestra una tendencia levemente decreciente.

 2. Los delitos más frecuentes en ambas regiones corresponden a hurtos, robos en viviendas,
    daños y consumo de alcohol/drogas en la vía pública.

 3. Se detecta un aumento reciente en delitos como homicidios, microtráfico, y violencia intrafamiliar.

 4. Coquimbo supera a Atacama en la mayoría de los delitos de alto impacto social.

 5. La concentración delictual es alta, con pocos tipos de delitos acumulando la mayor parte de los casos.

 Estas observaciones permiten sentar bases para futuros análisis con foco comunal o temático.
"

# ==========================================
# FIN
# ==========================================

