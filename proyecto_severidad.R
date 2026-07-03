################################################################################
#                                                                              #
#                  ____________________________________                        #
#                  Proyecto I - Severidad de la Pérdida                        #
#                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                           #
#                  Por: Camacho Cruz Angel Gabriel                             #
#                       Gonzalez Hernandez Josue Zuriel                        #
#                       Halave Cubillo Salvador                                #
#                       López Villegas Saori Kinari                            #
#                     & Reyes Gabriel Ilse Valeria                             #
#                                                                              #
################################################################################

install.packages(c("data.table", "dplyr", "stringr", "ggplot2",
                   "tidyr", "magrittr", "readxl", "moments"))
install.packages(c("fitdistrplus", "actuar", "goftest"))
install.packages("flexsurv")




#                  ____________________________________                        #
#                               Paquetería                                     #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          #


#Cargamos algunas librerias
library(data.table)
library(dplyr)
library(stringi) #biblioteca para problema con acentos
library(stringr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(readxl)
library(fitdistrplus)
library(actuar)
library(goftest)
library(flexsurv)
library(moments)

#Primero cargamos los datos
url <- "http://www.atlasnacionalderiesgos.gob.mx/descargas/Basehistorica_2000_a_2024.xlsx"
# Creamos un archivo temporal
temp_file <- tempfile(fileext = ".xlsx")
# Descargamos el archivo
download.file(url, destfile = temp_file, mode = "wb")
#Leemos el archivoexcel

d1_sev <- read_excel(temp_file)




#                  ____________________________________                        #
#                       Análisis de la información                             #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          #


#Veamos la estructura de los datos

class(d1_sev)
str(d1_sev)
head(d1_sev, n=1)
tail(d1_sev, n=1)
ncol(d1_sev)
nrow(d1_sev)




#                  ____________________________________                        #
#                     Limpieza de datos ajuste fechas                          #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                         #


#Nos quedaremos unicamente con las columnas que son de nuestro interés
d2_sev <- d1_sev %>% dplyr::select(`Fecha de Inicio`, `Fecha de Fin`, Año, `Clasificación del fenómeno`, `Tipo de fenómeno` ,`Estado`, `Total de daños (millones de pesos)`)
#Ahora tenemos un nuevo dataframe con solo las columnas que nos interesan, esto nos facilita el análisis y la visualización de los datos, ademas de que nos permite enfocarnos en lo que realmente queremos analizar, que es la severidad de la pérdida

#Nuestro siguiente paso de limpieza será para quedarnos unicamente con la clasificación de fenomeno hidrometeorologica
d3_sev <- d2_sev %>% filter(`Clasificación del fenómeno` == "Hidrometeorológico")

# Si en total de daños, el costo es 0, no aporta algo a nuestros datos, porque no se
# presentó un gasto, es por ello que los eliminaremos
d4_sev <- d3_sev %>% filter(`Total de daños (millones de pesos)` > 0)

# Notemos algo rapido
nrow(d1_sev)
nrow(d3_sev)
nrow(d4_sev)




#                  ____________________________________                        #
#                   Limpieza de datos ajuste fechas                            #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          #


# Ahora, tenemos que realizar modificaciones un poco mas complejas que solo seleccionar algunos datos de nuestro dataframe
# Comencemos corroborando que todas las columnas tengan datos en fecha de inicio
anyNA(d4_sev$`Fecha de Inicio`) # Nos indica si hay datos vacios
#Como la consola regresa falso, descartamos dicha suposición

anyNA(d4_sev$`Fecha de Fin`)
#En este caso, la consola regresa TRUE, por lo que es necesario buscar una solución a ello

sum(is.na(d4_sev$`Fecha de Fin`))
#Nos indica que hay dos datos con valor nulo en fecha de fin

#La solución optima es que para esos datos, la fecha de fin sea la misma que la de inicio, por lo que usaremos un for

for (i in 1:nrow(d4_sev)) {
  if (is.na(d4_sev$`Fecha de Fin`[i])) {
    d4_sev$`Fecha de Fin`[i] <- d4_sev$`Fecha de Inicio`[i]
  }
}

#Corroboramos nuevamente, para ver si el problema ya fue solucionado
anyNA(d4_sev$`Fecha de Fin`)
#Como la consola regresa false, podemos asegurar que ahora todos los eventos tuvieron fecha de inicio y de fin





#                  ____________________________________                        #
#                           Ajuste de Estados                                  #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          #


#Hay un dato en estados que puede causar ruido: "varios estados", por lo que definiremos este termino
#antes de hacer cualquier tipo de modificacion

d5_temp <- d4_sev %>%
  mutate(
    Estado = ifelse(Estado == "Varios Estados",
                    "Sonora, Sinaloa, Chihuahua, Durango, Zacatecas, Veracruz",
                    Estado)
  )

#Hay fenomenos que se presentaron en mas de un estado, por lo que es necesario ajustar la columna de estados para que cada fila solo tenga un estado, esto con el fin de facilitar el análisis y la visualización de los datos
# Realizaremos un ajuste, donde dependiendo la cantidad de estados, será la forma en la que se dividirá el total de daños en millones de pesos

d5_sev <- d5_temp %>%
  mutate(
    Estado = str_replace_all(Estado, " y ", ", "),
    Estado = str_replace_all(Estado, " Y ", ", "), #esto es para evitar casos donde la separación no sea una coma
    n_estados = str_count(Estado, ",") + 1
  ) %>%
  separate_rows(Estado, sep = ",") %>%
  mutate(
    Estado = str_trim(Estado),
    Estado = ifelse(Estado == "México", "Estado de México", Estado),
    `Total de daños (millones de pesos)` =
      `Total de daños (millones de pesos)` / n_estados
  ) %>%
  dplyr::select(-n_estados)

#Pero hay estados que tienen acento, para evitar repeticiones, primero le quitaremos los acentos a todos

estados_sa <- d5_sev$Estado %>%
  stri_trans_general("Latin-ASCII") %>%
  unique() %>%
  sort()

#Veamos como se ven los estados, esto únicamente para corroborar que se muestren bien

estados_variacion <- d5_sev %>%
  mutate(Estado_normalizado = stri_trans_general(Estado, "Latin-ASCII")) %>%
  group_by(Estado_normalizado) %>%
  summarise(
    Estados_originales = paste(unique(Estado), collapse = ", "),
    Frecuencia = n(),
    .groups = "drop"
  ) %>%
  arrange(Estado_normalizado)

#Cambiamos la fila de estados por los estados sin acentos, esto con el fin de evitar repeticiones y facilitar el análisis
d5_sev <- d5_sev %>%
  mutate(Estado = stringi::stri_trans_general(Estado, "Latin-ASCII"))

#Observemos nuestra tabla

d5_sev
#Pequeña limpieza en fenómenos
d5_sev <- d5_sev %>%
  mutate('Tipo de fenómeno' = str_replace_all(`Tipo de fenómeno`, "lluvias", "lluvia"))
# Creamos una tabla con la inflación anual desde el año 2000 hasta el año 2024
tab_inpc <- data.table(
  Año = 2000:2024,
  INPC = c(46.47538556,
           49.43481573,
           51.92174909,
           54.28257803,
           56.8275663,
           59.09388416,
           61.23867495,
           63.6679211,
           66.9308904,
           70.47645869,
           73.40597327,
           75.90719344,
           79.02812419,
           82.03624266,
           85.33296522,
           87.65456908,
           90.12792486,
           95.57296362,
           100.2554185,
           103.9006667,
           107.43,
           113.5419167,
           122.5075,
           129.2796667,
           135.3845833
  ) # INPC promedio anual calculado con los datos capturados por el INEGI
)

#Imprimimos la tabla
tab_inpc

# Añadimos una columna mas a nuestro data table 5, que será el total de daños en MDP ajustados al precio actual
d5_sev <- d5_sev %>%
  left_join(tab_inpc, by = "Año") %>%
  mutate(`Total de Daños MDP (Ajustados a 2024)` = `Total de daños (millones de pesos)` * (tab_inpc[Año == 2024, INPC] / INPC)) %>%
  dplyr::select(-INPC) # Eliminamos la columna del INPC que solo se necesitaba en este paso

d5_sev

# En caso de ser necesario aplicamos logaritmo a los datos (algunas pruebas de hipotesis lo requieren)
# Aplicamos logaritmo al total de daños por año
d6_sev <- d5_sev %>%
  group_by(Año) %>%
  summarise(`Total de daños en el año` = sum(`Total de Daños MDP (Ajustados a 2024)`), .groups = "drop") %>%
  mutate(`log(Total de daños)` = log(`Total de daños en el año`)) %>%
  arrange(Año)
d6_sev

d5_sev <- d5_sev %>%
  mutate(`log_danos` = log(`Total de Daños MDP (Ajustados a 2024)`))




#                  ____________________________________                        #
#               Analisis grafico para la agrupacion de estados                 #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          #


#Analisis de los datos por estado
d1_visual <- d5_sev %>%
  group_by(`Estado`) %>%
  summarize(n = n(), suma = sum(`Total de Daños MDP (Ajustados a 2024)`)) %>%
  mutate(promEst = suma/n)

d1_visual

d2_visual <- d5_sev %>%
  group_by(`Tipo de fenómeno`) %>%
  summarize(n = n(), suma = sum(`Total de Daños MDP (Ajustados a 2024)`)) %>%
  mutate(promFen = suma/n)

d2_visual

d3_visual <- d5_sev %>%
  group_by(Estado, `Tipo de fenómeno`) %>%
  summarize(n = n(), suma = sum(`Total de Daños MDP (Ajustados a 2024)`)) %>%
  mutate(promFen = suma/n) %>%
  ungroup() %>%
  arrange(Estado, desc(suma))

d3_visual

hist(d5_sev$`log_danos`, breaks = 50)


#Analisis introductorio a la segmentacion de grupos

ggplot(d5_sev, aes(x = `log_danos`)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Estado, scales = "free_y")

ggplot(d5_sev, aes(x = `log_danos`)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") + 
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Estado)

ggplot(d5_sev, aes(x = `log_danos`)) +
  geom_density( fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Estado)

#Los del centro del pais tienen una media menor al 0 de log daños
ggplot(d5_sev, aes(x = `log_danos`)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Estado)

#Para explorar los valores mas extremos
ggplot(d5_sev, aes(x = `Total de Daños MDP (Ajustados a 2024)`)) +
  geom_boxplot() + # Colores para que se vea mejor
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Estado)




#                  ____________________________________                        #
#                         Clustering de Estados                                #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          #


#Clustering de Estados mediante el estadístico de Kolmogorov-Smirnov

# 1. Limpiar datos y usar el logaritmo de los daños
datos_limpios <- d5_sev %>%
  filter(!is.na(`Total de Daños MDP (Ajustados a 2024)`),
         `Total de Daños MDP (Ajustados a 2024)` > 0) %>%
  mutate(log_danos = log(`Total de Daños MDP (Ajustados a 2024)`))

# 2. Separar las distribuciones por estado
lista_estados <- split(datos_limpios$log_danos, datos_limpios$Estado)

# Filtrar estados que tengan muy pocos eventos para que la prueba matemática funcione bien
lista_estados <- lista_estados[sapply(lista_estados, length) >= 5]

# 3. Preparar la matriz vacía
n_estados <- length(lista_estados)
nombres_estados <- names(lista_estados)
matriz_ks <- matrix(0, nrow = n_estados, ncol = n_estados, dimnames = list(nombres_estados, nombres_estados))

# 4. Calcular la distancia K-S entre todos los estados
for (i in 1:n_estados) {
  for (j in 1:n_estados) {
    if (i != j) {
      matriz_ks[i, j] <- ks.test(lista_estados[[i]], lista_estados[[j]])$statistic
    }
  }
}

# 5. Agrupar los estados
distancia_ks <- as.dist(matriz_ks)
arbol_estados <- hclust(distancia_ks, method = "ward.D2")

# 6. Graficar el resultado final
plot(arbol_estados,
     main = "Agrupamiento de Estados por Similitud en Daños",
     xlab = "Estados de la República",
     ylab = "Distancia (Diferencia entre histogramas)",
     sub = "",
     cex = 0.8)

# Dibujar rectángulos para visualizar los grupos
rect.hclust(arbol_estados, k = 3, border = "blue")




#                  ____________________________________                        #
#                           Agrupacion final                                   #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          #


#Agrupación 1
d5_sev_final1 <- d5_sev %>%
  mutate(Grupo_Severidad = case_match(Estado,
                                      
                                      c("Aguascalientes", "Baja California",  "Estado de Mexico", "Ciudad de Mexico", "Guanajuato",
                                        "Hidalgo","Jalisco" , "Morelos", "Puebla", "Queretaro", "San Luis Potosi", "Tlaxcala") ~ "1",
                                      
                                      c("Coahuila",  "Chihuahua",  "Durango",   "Nuevo Leon",   "Sonora",  "Zacatecas") ~ "2",
                                      
                                      c("Chiapas","Guerrero","Oaxaca") ~ "3",
                                      
                                      c("Colima",  "Sinaloa", "Nayarit", "Michoacan") ~ "4",
                                      
                                      c("Baja California Sur", "Campeche", "Quintana Roo", "Tamaulipas", "Veracruz",  "Tabasco",  "Yucatan") ~ "5",
                                      
                                      .default = "Otro"
  ))



# #Para analizar como se conforma cada grupo mas a detalle
# chequeo01 <- agg_final %>%
#   filter(Grupo_Severidad == 1) %>%
#   arrange(Estado, desc(`Total de Daños MDP (Ajustados a 2024)`))
# 
# chequeo02 <- agg_final %>%
#   filter(Grupo_Severidad == 2) %>%
#   arrange(Estado, desc(`Total de Daños MDP (Ajustados a 2024)`))
# 
# chequeo03 <- agg_final %>%
#   filter(Grupo_Severidad == 3) %>%
#   arrange(Estado, desc(`Total de Daños MDP (Ajustados a 2024)`))
# 
# chequeo04 <- agg_final %>%
#   filter(Grupo_Severidad == 4) %>%
#   arrange(Estado, desc(`Total de Daños MDP (Ajustados a 2024)`))
# 
# chequeo05 <- agg_final %>%
#   filter(Grupo_Severidad == 5) %>%
#   arrange(Estado, desc(`Total de Daños MDP (Ajustados a 2024)`))




# Verificion visual de cada grupo
ggplot(d5_sev_final1, aes(x = log_danos, fill = Grupo_Severidad)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Grupo_Severidad) +
  labs(title = "ADN de Daños por Grupo",
       x = "Logaritmo de Daños",
       y = "Densidad")


# Verificion visual de cada grupo
ggplot(d5_sev_final1, aes(x = log_danos, fill = Grupo_Severidad)) +
  geom_histogram() +
  facet_wrap(~Grupo_Severidad) +
  labs(title = "ADN de Daños por Grupo",
       x = "Logaritmo de Daños",
       y = "Histograma")



#Agrupacion con la que trabajaremos
agg_final <- d5_sev_final1

tabla_grupos <- agg_final %>%
  group_by(Grupo_Severidad) %>%
  summarise(
    n = n(),
    media = mean(`Total de Daños MDP (Ajustados a 2024)`),
    curtosis = kurtosis(`Total de Daños MDP (Ajustados a 2024)`),
    p95 = quantile(`Total de Daños MDP (Ajustados a 2024)`, 0.95),
    p99 = quantile(`Total de Daños MDP (Ajustados a 2024)`, 0.99),
    max = max(`Total de Daños MDP (Ajustados a 2024)`),
    .groups = "drop"
  )

tabla_grupos




#                  ____________________________________                        #
#           Estimacion de parametros y Pruebas de bondad de ajuste             #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          # 


#Prueba para el grupo 1
#(Es una Burr o Pareto o Pareto generalizado, elegimos la distribución Pareto por mejor AIC)
d1 <- agg_final %>%
  filter(Grupo_Severidad == 1) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`)

p <- d1[d1 > 0]



# Burr
mod_burr <- fitdist(p, "burr", method = "mle")
print("Burr AIC")
summary(mod_burr)

qqcomp(mod_burr)
ppcomp(mod_burr)

hip_burr_ad <- ad.test(
  p,
  "pburr",
  shape1 = mod_burr$estimate["shape1"],
  shape2 = mod_burr$estimate["shape2"],
  scale  = mod_burr$estimate["scale"]
)

hip_burr_ks <- ks.test(
  p,
  "pburr",
  shape1 = mod_burr$estimate["shape1"],
  shape2 = mod_burr$estimate["shape2"],
  scale  = mod_burr$estimate["scale"]
)

print("Burr p values")
hip_burr_ad
hip_burr_ks



# Pareto
mod_pareto <- fitdist(p, "pareto", method = "mle")
print("Pareto AIC")
summary(mod_pareto)

qqcomp(mod_pareto)
ppcomp(mod_pareto)

hip_pareto_ad <- ad.test(
  p,
  null = "ppareto",
  shape = mod_pareto$estimate["shape"],
  scale = mod_pareto$estimate["scale"]
)

hip_pareto_ks <- ks.test(
  p,
  "ppareto",
  shape = mod_pareto$estimate["shape"],
  scale = mod_pareto$estimate["scale"]
)

print("Pareto p values")
hip_pareto_ad
hip_pareto_ks



# Pareto generalizada
mod_genpareto <- fitdist(p, "genpareto", method = "mle")
print("Pare Gen AIC")
summary(mod_genpareto)

qqcomp(mod_genpareto)
ppcomp(mod_genpareto)

hip_genpareto_ad <- ad.test(
  p,
  null = "pgenpareto",
  shape1 = mod_genpareto$estimate["shape1"],
  shape2 = mod_genpareto$estimate["shape2"],
  scale  = mod_genpareto$estimate["scale"]
)

hip_genpareto_ks <- ks.test(
  p,
  "pgenpareto",
  shape1 = mod_genpareto$estimate["shape1"],
  shape2 = mod_genpareto$estimate["shape2"],
  scale  = mod_genpareto$estimate["scale"]
)

print("Pare Gen p values")
hip_genpareto_ad
hip_genpareto_ks



# Lognormal
mod_lnorm <- fitdist(p, "lnorm", method = "mle")
print("LogNorm AIC")
summary(mod_lnorm)

qqcomp(mod_lnorm)
ppcomp(mod_lnorm)

hip_lnorm_ad <- ad.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

hip_lnorm_ks <- ks.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

print("LogNorm p values")
hip_lnorm_ad
hip_lnorm_ks




#Prueba para el grupo 2
#Es o Lognormal o Gamma Generalizada, nos quedamos con la LogNormal porque tiene menos parametros
d2 <- agg_final %>%
  filter(Grupo_Severidad == 2) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`)

p <- d2[d2 > 0]



# Lognormal
mod_lnorm <- fitdist(p, "lnorm", method = "mle")
print("LogNorm AIC")
summary(mod_lnorm)

qqcomp(mod_lnorm)
ppcomp(mod_lnorm)

hip_lnorm_ad <- ad.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

hip_lnorm_ks <- ks.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

print("LogNorm p values")
hip_lnorm_ad
hip_lnorm_ks



# Gamma generalizada
mod_gengamma <- fitdist(
  p, "gengamma", method = "mle",
  start = list(
    mu = mean(log(p)),
    sigma = sd(log(p)),
    Q = 0
  )
)
summary(mod_gengamma)

qqcomp(mod_gengamma)
ppcomp(mod_gengamma)

hip_gengamma_ad <- ad.test(
  p, "pgengamma",
  mu = mod_gengamma$estimate["mu"],
  sigma = mod_gengamma$estimate["sigma"],
  Q = mod_gengamma$estimate["Q"]
)

hip_gengamma_ks <- ks.test(
  p, "pgengamma",
  mu = mod_gengamma$estimate["mu"],
  sigma = mod_gengamma$estimate["sigma"],
  Q = mod_gengamma$estimate["Q"]
)

hip_gengamma_ad
hip_gengamma_ks




#Prueba para el grupo 3
#Igual, es Pareto o Burr, nos quedamos con pareto porque tiene menos parametros
d3 <- agg_final %>%
  filter(Grupo_Severidad == 3) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`)

p <- d3[d3 > 0]



# Burr
mod_burr <- fitdist(p, "burr", method = "mle")
print("Burr AIC")
summary(mod_burr)

qqcomp(mod_burr)
ppcomp(mod_burr)

hip_burr_ad <- ad.test(
  p,
  "pburr",
  shape1 = mod_burr$estimate["shape1"],
  shape2 = mod_burr$estimate["shape2"],
  scale  = mod_burr$estimate["scale"]
)

hip_burr_ks <- ks.test(
  p,
  "pburr",
  shape1 = mod_burr$estimate["shape1"],
  shape2 = mod_burr$estimate["shape2"],
  scale  = mod_burr$estimate["scale"]
)

print("Burr p values")
hip_burr_ad
hip_burr_ks



# Pareto
mod_pareto <- fitdist(p, "pareto", method = "mle")
print("Pareto AIC")
summary(mod_pareto)

qqcomp(mod_pareto)
ppcomp(mod_pareto)

hip_pareto_ad <- ad.test(
  p,
  null = "ppareto",
  shape = mod_pareto$estimate["shape"],
  scale = mod_pareto$estimate["scale"]
)

hip_pareto_ks <- ks.test(
  p,
  "ppareto",
  shape = mod_pareto$estimate["shape"],
  scale = mod_pareto$estimate["scale"]
)

print("Pareto p values")
hip_pareto_ad
hip_pareto_ks



# Pareto generalizada
mod_genpareto <- fitdist(p, "genpareto", method = "mle")
print("Pare Gen AIC")
summary(mod_genpareto)

qqcomp(mod_genpareto)
ppcomp(mod_genpareto)

hip_genpareto_ad <- ad.test(
  p,
  null = "pgenpareto",
  shape1 = mod_genpareto$estimate["shape1"],
  shape2 = mod_genpareto$estimate["shape2"],
  scale  = mod_genpareto$estimate["scale"]
)

hip_genpareto_ks <- ks.test(
  p,
  "pgenpareto",
  shape1 = mod_genpareto$estimate["shape1"],
  shape2 = mod_genpareto$estimate["shape2"],
  scale  = mod_genpareto$estimate["scale"]
)

print("Pare Gen p values")
hip_genpareto_ad
hip_genpareto_ks



# Lognormal
mod_lnorm <- fitdist(p, "lnorm", method = "mle")
print("LogNorm AIC")
summary(mod_lnorm)

qqcomp(mod_lnorm)
ppcomp(mod_lnorm)

hip_lnorm_ad <- ad.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

hip_lnorm_ks <- ks.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

print("LogNorm p values")
hip_lnorm_ad
hip_lnorm_ks




#Prueba para el grupo 4
#Lognormal por menor AIC
d4 <- agg_final %>%
  filter(Grupo_Severidad == 4) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`)

p <- d4[d4 > 0]



# Lognormal
mod_lnorm <- fitdist(p, "lnorm", method = "mle")
print("LogNorm AIC")
summary(mod_lnorm)

qqcomp(mod_lnorm)
ppcomp(mod_lnorm)

hip_lnorm_ad <- ad.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

hip_lnorm_ks <- ks.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

print("LogNorm p values")
hip_lnorm_ad
hip_lnorm_ks



# Loglogistica
mod_llogis <- fitdist(p, "llogis", method = "mle")
print("Loglogistica AIC")
summary(mod_llogis)

qqcomp(mod_llogis)
ppcomp(mod_llogis)

hip_llogis_ad <- ad.test(
  p, "pllogis",
  mod_llogis$estimate["shape"],
  mod_llogis$estimate["scale"]
)

hip_llogis_ks <- ks.test(
  p, "pllogis",
  mod_llogis$estimate["shape"],
  mod_llogis$estimate["scale"]
)

print("Loglogistica p values")
hip_llogis_ad
hip_llogis_ks



# Burr
mod_burr <- fitdist(p, "burr", method = "mle")
print("Burr AIC")
summary(mod_burr)

qqcomp(mod_burr)
ppcomp(mod_burr)

hip_burr_ad <- ad.test(
  p,
  "pburr",
  shape1 = mod_burr$estimate["shape1"],
  shape2 = mod_burr$estimate["shape2"],
  scale  = mod_burr$estimate["scale"]
)

hip_burr_ks <- ks.test(
  p,
  "pburr",
  shape1 = mod_burr$estimate["shape1"],
  shape2 = mod_burr$estimate["shape2"],
  scale  = mod_burr$estimate["scale"]
)

print("Burr p values")
hip_burr_ad
hip_burr_ks



#Paralogistica
mod_paralogis <- fitdist(
  p,
  "paralogis",
  method = "mle",
  start = list(shape = 1, scale = median(p))
)
print("Paralog AIC")
summary(mod_paralogis)

qqcomp(mod_paralogis)
ppcomp(mod_paralogis)

hip_paralogis_ad <- ad.test(
  p,
  null = "pparalogis",
  shape = mod_paralogis$estimate["shape"],
  scale = mod_paralogis$estimate["scale"]
)

hip_paralogis_ks <- ks.test(
  p,
  "pparalogis",
  shape = mod_paralogis$estimate["shape"],
  scale = mod_paralogis$estimate["scale"]
)

print("Para Log p values")
hip_paralogis_ad
hip_paralogis_ks




#Prueba para el grupo 5
#Lognormal o Gamma Gen, nos quedamos con Lognormal por menor AIC y menos parametros
d5 <- agg_final %>%
  filter(Grupo_Severidad == 5) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`)

p <- d5[d5 > 0]



# Lognormal
mod_lnorm <- fitdist(p, "lnorm", method = "mle")
print("LogNorm AIC")
summary(mod_lnorm)

qqcomp(mod_lnorm)
ppcomp(mod_lnorm)

hip_lnorm_ad <- ad.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

hip_lnorm_ks <- ks.test(
  p, "plnorm",
  mod_lnorm$estimate["meanlog"],
  mod_lnorm$estimate["sdlog"]
)

print("LogNorm p values")
hip_lnorm_ad
hip_lnorm_ks



# Loglogistica
mod_llogis <- fitdist(p, "llogis", method = "mle")
print("Loglogistica AIC")
summary(mod_llogis)

qqcomp(mod_llogis)
ppcomp(mod_llogis)

hip_llogis_ad <- ad.test(
  p, "pllogis",
  mod_llogis$estimate["shape"],
  mod_llogis$estimate["scale"]
)

hip_llogis_ks <- ks.test(
  p, "pllogis",
  mod_llogis$estimate["shape"],
  mod_llogis$estimate["scale"]
)

print("Loglogistica p values")
hip_llogis_ad
hip_llogis_ks




#                  ____________________________________                        #
#                       Costo promedio por evento                              #
#                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                          # 


#-------------------------------------------------------------------------------
# Costo promedio esperado por evento para un nuevo siniestro en 2025
# Decision metodologica:
# - G1 y G3 se reportan como Pareto en la parte de ajuste/modelacion
# - Pero para calcular E[X] (costo promedio esperado) usamos Lognormal auxiliar
#   en todos los grupos, porque da una media finita y estable
#-------------------------------------------------------------------------------


# 1) FUNCION Y VARIABLE AUXILIAR
esp_lnorm_sev <- function(meanlog, sdlog) {
  exp(meanlog + (sdlog^2) / 2)
}

inflacion_2025 <- 0.045


# 2) DATOS POR GRUPO
x_sev_g1 <- agg_final %>%
  filter(Grupo_Severidad == "1" | Grupo_Severidad == 1) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`) %>%
  .[. > 0]

x_sev_g2 <- agg_final %>%
  filter(Grupo_Severidad == "2" | Grupo_Severidad == 2) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`) %>%
  .[. > 0]

x_sev_g3 <- agg_final %>%
  filter(Grupo_Severidad == "3" | Grupo_Severidad == 3) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`) %>%
  .[. > 0]

x_sev_g4 <- agg_final %>%
  filter(Grupo_Severidad == "4" | Grupo_Severidad == 4) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`) %>%
  .[. > 0]

x_sev_g5 <- agg_final %>%
  filter(Grupo_Severidad == "5" | Grupo_Severidad == 5) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`) %>%
  .[. > 0]


# 3) AJUSTE LOGNORMAL AUXILIAR PARA ESPERANZA
# Aqui no estamos cambiando la distribucion "reportada" del ajuste principal.
# Solo usamos Lognormal para calcular una esperanza estable.

fit_esp_g1 <- if (length(x_sev_g1) > 1) fitdistrplus::fitdist(x_sev_g1, "lnorm", method = "mle") else NULL
fit_esp_g2 <- if (length(x_sev_g2) > 1) fitdistrplus::fitdist(x_sev_g2, "lnorm", method = "mle") else NULL
fit_esp_g3 <- if (length(x_sev_g3) > 1) fitdistrplus::fitdist(x_sev_g3, "lnorm", method = "mle") else NULL
fit_esp_g4 <- if (length(x_sev_g4) > 1) fitdistrplus::fitdist(x_sev_g4, "lnorm", method = "mle") else NULL
fit_esp_g5 <- if (length(x_sev_g5) > 1) fitdistrplus::fitdist(x_sev_g5, "lnorm", method = "mle") else NULL


# 4) SEVERIDAD ESPERADA POR GRUPO (PRECIOS 2024)
sev_g1_2024 <- if (!is.null(fit_esp_g1)) {
  esp_lnorm_sev(
    meanlog = unname(fit_esp_g1$estimate["meanlog"]),
    sdlog   = unname(fit_esp_g1$estimate["sdlog"])
  )
} else NA_real_

sev_g2_2024 <- if (!is.null(fit_esp_g2)) {
  esp_lnorm_sev(
    meanlog = unname(fit_esp_g2$estimate["meanlog"]),
    sdlog   = unname(fit_esp_g2$estimate["sdlog"])
  )
} else NA_real_

sev_g3_2024 <- if (!is.null(fit_esp_g3)) {
  esp_lnorm_sev(
    meanlog = unname(fit_esp_g3$estimate["meanlog"]),
    sdlog   = unname(fit_esp_g3$estimate["sdlog"])
  )
} else NA_real_

sev_g4_2024 <- if (!is.null(fit_esp_g4)) {
  esp_lnorm_sev(
    meanlog = unname(fit_esp_g4$estimate["meanlog"]),
    sdlog   = unname(fit_esp_g4$estimate["sdlog"])
  )
} else NA_real_

sev_g5_2024 <- if (!is.null(fit_esp_g5)) {
  esp_lnorm_sev(
    meanlog = unname(fit_esp_g5$estimate["meanlog"]),
    sdlog   = unname(fit_esp_g5$estimate["sdlog"])
  )
} else NA_real_


# 5) PESOS DE CADA GRUPO
pesos_sev <- agg_final %>%
  filter(Grupo_Severidad %in% c("1", "2", "3", "4", "5", 1, 2, 3, 4, 5)) %>%
  count(Grupo_Severidad) %>%
  mutate(peso = n / sum(n))

w1 <- pesos_sev %>% filter(Grupo_Severidad == "1" | Grupo_Severidad == 1) %>% pull(peso)
w2 <- pesos_sev %>% filter(Grupo_Severidad == "2" | Grupo_Severidad == 2) %>% pull(peso)
w3 <- pesos_sev %>% filter(Grupo_Severidad == "3" | Grupo_Severidad == 3) %>% pull(peso)
w4 <- pesos_sev %>% filter(Grupo_Severidad == "4" | Grupo_Severidad == 4) %>% pull(peso)
w5 <- pesos_sev %>% filter(Grupo_Severidad == "5" | Grupo_Severidad == 5) %>% pull(peso)


# 6) TABLA RESUMEN
tabla_severidad <- tibble(
  Grupo_Severidad = c("1", "2", "3", "4", "5"),
  Distribucion_Ajuste = c("Pareto", "Lognormal", "Pareto", "Lognormal", "Lognormal"),
  Distribucion_para_Esperanza = c("Lognormal", "Lognormal", "Lognormal", "Lognormal", "Lognormal"),
  Peso = c(w1, w2, w3, w4, w5),
  Severidad_Esperada_2024_MDP = c(sev_g1_2024, sev_g2_2024, sev_g3_2024, sev_g4_2024, sev_g5_2024)
) %>%
  mutate(Aportacion_2024_MDP = Peso * Severidad_Esperada_2024_MDP)

print(tabla_severidad)
cat("\n")
cat("--------------------------------------------\n")
cat("COSTO PROMEDIO ESPERADO POR GRUPO (2024)\n")
cat("--------------------------------------------\n")

for (i in 1:nrow(tabla_severidad)) {
  cat(
    "Grupo", tabla_severidad$Grupo_Severidad[i], ":",
    round(tabla_severidad$Severidad_Esperada_2024_MDP[i], 4),
    "MDP\n"
  )
}
tabla_severidad <- tabla_severidad %>%
  mutate(Severidad_Esperada_2025_MDP = Severidad_Esperada_2024_MDP * (1 + inflacion_2025))

cat("\n")
cat("--------------------------------------------\n")
cat("COSTO PROMEDIO ESPERADO POR GRUPO (2025)\n")
cat("--------------------------------------------\n")

for (i in 1:nrow(tabla_severidad)) {
  cat(
    "Grupo", tabla_severidad$Grupo_Severidad[i], ":",
    round(tabla_severidad$Severidad_Esperada_2025_MDP[i], 4),
    "MDP\n"
  )
}


# 7) COSTO PROMEDIO TOTAL
costo_promedio_2024 <- sum(tabla_severidad$Aportacion_2024_MDP, na.rm = TRUE)


costo_promedio_2025 <- costo_promedio_2024 * (1 + inflacion_2025)

cat("----------------------------------------------------\n")
cat("Costo promedio esperado por evento en 2024:", round(costo_promedio_2024, 4), "MDP\n")
cat("Costo promedio esperado por evento en 2025:", round(costo_promedio_2025, 4), "MDP\n")
cat("----------------------------------------------------\n")


# 8) INTERPRETACION
cat("\nInterpretacion metodologica:\n")
cat("Los grupos 1 y 3 se conservan como Pareto en la etapa de ajuste porque describen mejor la cola de la distribucion.\n")
cat("Sin embargo, para calcular el costo promedio por evento se uso una Lognormal auxiliar, ya que proporciona una esperanza finita y mas estable.\n")
cat("De esta manera, se mantiene la interpretacion de cola pesada en la modelacion, pero se obtiene una estimacion operativa del costo promedio esperado.\n")

