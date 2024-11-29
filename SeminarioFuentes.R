system("git init")  # Inicializa git si no lo has hecho aún
system('git remote add origin https://github.com/tu_usuario/nombre_repositorio.git')
system('git branch -M main')  # Asegúrate de que la rama principal se llame 'main'
system('git add .')  # Añade todos los archivos
system('git commit -m "Inicializar proyecto en GitHub"')  # Realiza el primer commit
system('git push -u origin main')  # Sube los archivos al repositorio remoto
#Comentario

#Instalamos los paquetes
install.packages("jsonlite")
install.packages("pxR")

#Importamos las librerías
library(jsonlite)
library(dplyr)
library(pxR)
library(tidyverse)
library(tidyr)

#Importamos los datos con cada formato correspondiente

datos_cataratas<-read.px("./DATA/INPUT/datos_cataratas.px")
datos_solar<-fromJSON("./DATA/INPUT/LuzSolarProvincias.json")

#Los transformamos a data frame y comprobamos algunas de sus filas
df_datos_solar<-as.data.frame(datos_solar)
df_datos_cataratas<-as.data.frame(datos_cataratas)

summary(df_datos_cataratas)
summary(df_datos_solar)

str(df_datos_cataratas)
str(df_datos_solar)


#De las distintas enfermedades de nuestros datos, seleccionamos las de interés, que son las cataratas

factor(df_datos_cataratas$Sexo)
df_solo_cataratas <- df_datos_cataratas %>%
  select(Comunidad.autónoma,Enfermedades,Sexo,value)%>%
  filter(Enfermedades == "Cataratas")

df_solo_cataratas
# Ver los primeros registros filtrados
head(df_solo_cataratas)
print(df_solo_cataratas)


# Datos iniciales en formato data.frame
df_datos_solar <- data.frame(
  Comunidad = c('Andalucía', 'Andalucía', 'Andalucía', 'Andalucía', 'Andalucía', 'Andalucía', 'Andalucía', 'Andalucía', 
                'Aragón', 'Aragón', 'Aragón', 'Asturias', 'Cantabria', 'Cataluña', 'Cataluña', 'Cataluña', 'Cataluña',
                'Castilla La-Mancha', 'Castilla La-Mancha', 'Castilla La-Mancha', 'Castilla La-Mancha', 'Castilla La-Mancha', 
                'Castilla y León', 'Castilla y León', 'Castilla y León', 
                'Castilla y León', 'Castilla y León', 'Castilla y León', 'Castilla y León', 'Castilla y León', 'Castilla y León',
                'Madrid', 'Comunidad Valenciana', 'Comunidad Valenciana', 'Comunidad Valenciana', 'Extremadura', 'Extremadura',
                'Galicia', 'Galicia', 'Galicia', 'Galicia', 'Islas Baleares', 'Islas Baleares', 'Islas Canarias', 'Islas Canarias', 
                'Islas Canarias', 'La Rioja', 'Murcia', 'Navarra', 'País Vasco', 'País Vasco', 'País Vasco'),
  Provincia = c('Almería', 'Cádiz', 'Córdoba', 'Granada', 'Huelva', 'Jaén', 'Málaga', 'Sevilla',
                'Huesca', 'Teruel', 'Zaragoza', 'Asturias', 'Cantabria', 'Barcelona', 'Girona', 'Lleida', 'Tarragona',
                'Albacete', 'Ciudad Real', 'Cuenca', 'Guadalajara', 'Toledo', 'Ávila', 'Burgos', 'León', 'Palencia',
                'Salamanca', 'Segovia', 'Soria', 'Valladolid', 'Zamora', 'Madrid', 'Alicante', 'Castellón', 'Valencia',
                'Badajoz', 'Cáceres', 'A Coruña', 'Lugo', 'Ourense', 'Pontevedra', 'Mallorca', 'Menorca', 'Tenerife',
                'Lanzarote', 'La Palma', 'La Rioja', 'Murcia', 'Pamplona', 'Álava', 'Guipúzcoa', 'Vizcaya'),
  Horas_de_sol = c(3305, 3316, 3316, 3328, 3527, 3289, 3248, 3526, 3099, 3011, 2620, 1962, 1639, 2453, 2800, 3031, 2620,
                   3282, 3295, 2779, 2900, 2815, 3305, 2751, 2734, 2576, 3262, 3024, 2894, 3016, 2858, 2691, 3397, 3321,
                   2808, 3224, 3365, 2453, 2820, 2800, 3031, 3098, 2981, 3098, 2924, 2800, 2708, 3348, 2285, 2147, 1906, 1694),
  Hora_solar_pico = c(6.02, 6, 5.9, 5.93, 6.02, 5.82, 5.82, 5.98, 5.67, 5.11, 5.61, 3.85, 3.97, 5.4, 4.12, 5.61, 5.59, 
                      5.72, 5.76, 5.57, 5.59, 5.78, 5.27, 4.98, 5.31, 5.11, 5.34, 5.02, 5.1, 5.39, 5.43, 5.74, 5.73, 5.59, 
                      5.73, 5.74, 5.7, 4.3, 4.3, 4.8, 4.4, 5.3, 5.3, 5.3, 5.9, 5.9, 4.54, 5.7, 4.74, 4.2, 3.6, 3.86)
)

media_horas_sol <- tapply(df_datos_solar$Horas_de_sol, df_datos_solar$Comunidad, mean)
print(media_horas_sol)


#LO SIGUIENTE NO NOS FUNCIONA, POR LO QUE HACEMOS EL DATA FRAME MANUALMENTE, PERO PROPORCIONAMOS LA ALTERNATIVA.
#Separar la columna comunidad.provincia con split y crear nombres para cada columna. Eliminar paso anterior
print(df_datos_solar)
colnames(df_datos_solar)

# Pivotar los datos a formato largo
df_sol_largo <- df_datos_solar %>%
  t() %>%  # Transponer el dataframe (filas a columnas)
  as.data.frame() %>%  # Convertirlo nuevamente en dataframe
  rownames_to_column(var = "Categoria") %>%  # Convertir las filas en columna "Categoria"
  pivot_longer(
    cols = -Categoria,  # Excluir la columna "Categoria" (que tiene los nombres de las ubicaciones)
    names_to = "Comunidad.Provincia",  # Ubicaciones en una columna
    values_to = "Horas de sol"         # Valores de "Horas de sol"
  ) %>%
  filter(Categoria == "Horas de sol") %>%  # Filtrar solo las filas correspondientes a "Horas de sol"
  #select(-Categoria) 

print(df_sol_largo)

# Separar 'Comunidad.Provincia' en dos columnas
df_sol_largo <- df_sol_largo %>%
  separate(
    col = Comunidad.Provincia,
    into = c("Comunidad", "Provincia"),
    sep = "\\."
  )

# Verificamos el dataframe después de la transformación
print(head(df_sol_largo))

# Calcular la media de las horas de sol por comunidad
media_horas_sol <- df_sol_largo %>%
  group_by(Comunidad) %>%
  summarize(MediaHorasSol = mean(`Horas de sol`, na.rm = TRUE))

# Mostrar el resultado final
print(media_horas_sol)

















df_datos_solar<- df_datos_solar %>%
  mutate(across(everything(), as.character))
print(df_datos_solar)
summary(df_datos_solar)
         
df_sol_largo <- df_datos_solar %>%
  pivot_longer(
    cols = everything(),
    names_to = "Comunidad.Provincia",
    values_to = "Valores"
  )
print(df_sol_largo)

df_sol_largo <- df_sol_largo%>%
  mutate(Valores = as.numeric(Valores))
  
print(df_sol_largo)
 
df_sol_largo <- df_sol_largo%>%
  separate(
    col = Comunidad.Provincia,
    into = c("Comunidad", "Provincia"),
    sep ="\\."
) 
print(df_sol_largo)
  

# Calcular la media de Horas de Sol por Comunidad
media_horas_sol <- df_sol_largo%>%
  group_by(Comunidad)%>%
  summarize(MediaHorasSol = mean(Valores, na.rm=TRUE))
print(media_horas_sol)

unique(df_sol_largo$Comunidad)



#CONTINUAMOS AQUÍ:
media_horas_sol <- tapply(df_sol_largo$Valores, df_sol_largo$Comunidad, mean)


# Mostrar el resultado
print(media_horas_sol)
df_media_horas_sol <- as.data.frame(media_horas_sol)
df_media_horas_sol$Comunidad <- rownames(df_media_horas_sol)
colnames(df_media_horas_sol) <- c("Media_horas_sol", "Comunidad")
df_media_horas_sol

#Creamos categorías para clasificar las horas de sol por comunidad
df_sol_clasificado <- df_media_horas_sol %>%
  mutate(
    clasificacion = factor(case_when(
      media_horas_sol < 2500 ~ "Bajo",
      media_horas_sol >= 2500 & media_horas_sol < 3000 ~ "Medio",
      media_horas_sol > 3000 ~ "Alto"
    )
  )
)

str(df_sol_clasificado)


#Transformamos esas categorías en niveles, y contamos cuantas comunidades hay por nivel
#levels(factor(df_sol_clasificado$clasificacion))
#table(df_sol_clasificado$clasificacion)


df_sol_definitivo <- df_sol_clasificado %>%
  select(Comunidad,Media_horas_sol,clasificacion)

df_sol_definitivo

#Cambiamos los nombres de las comunidades para que coincidan y poder hacer un join
df_solo_cataratas <- df_solo_cataratas %>%
  mutate(
    Comunidad.autónoma = case_when(
      Comunidad.autónoma == "01 Andalucía" ~ "Andalucía",
      Comunidad.autónoma == "02 Aragón" ~ "Aragón",
      Comunidad.autónoma == "03 Asturias, Principado de" ~ "Asturias",
      Comunidad.autónoma == "04 Balears, Illes" ~ "Islas Baleares",
      Comunidad.autónoma == "05 Canarias" ~ "Islas Canarias",
      Comunidad.autónoma == "06 Cantabria" ~ "Cantabria",
      Comunidad.autónoma == "07 Castilla y León" ~ "Castilla y León",
      Comunidad.autónoma == "08 Castilla - La Mancha" ~ "Castilla La-Mancha",
      Comunidad.autónoma == "09 Cataluña" ~ "Cataluña",
      Comunidad.autónoma == "10 Comunitat Valenciana" ~ "Comunidad Valenciana",
      Comunidad.autónoma == "11 Extremadura" ~ "Extremadura",
      Comunidad.autónoma == "12 Galicia" ~ "Galicia",
      Comunidad.autónoma == "13 Madrid, Comunidad de" ~ "Madrid",
      Comunidad.autónoma == "14 Murcia, Región de" ~ "Murcia",
      Comunidad.autónoma == "15 Navarra, Comunidad Foral de" ~ "Navarra",
      Comunidad.autónoma == "16 País Vasco" ~ "País Vasco",
      Comunidad.autónoma == "17 Rioja, La" ~ "La Rioja",
      Comunidad.autónoma == "18 Ceuta" ~ "Ceuta",
      Comunidad.autónoma == "19 Melilla" ~ "Melilla",
      Comunidad.autónoma == "Total Nacional" ~ "Total Nacional",
      TRUE ~ Comunidad.autónoma  # Dejar sin cambios si no coincide
    )
  )%>% 
  filter(!Comunidad.autónoma %in% c("Total Nacional"))


print(df_solo_cataratas)


#Unimos las dos tablas mediante la columna de la Comunidad Autónoma
df_final <- df_solo_cataratas %>%
  left_join(df_sol_definitivo, by = c("Comunidad.autónoma"="Comunidad")) #Realizamos un join sin necesidad de cambiar el nombre del atributo en una de las tablas

df_final

#Ordenamos de mayor a menor en función de las horas de sol
df_final_sol<-df_final%>%
  group_by(Comunidad.autónoma)%>%
  select(Media_horas_sol,clasificacion,Sexo,value)%>%
  arrange(desc(Media_horas_sol))

df_final_sol

#Ordenamos de mayor a menor en función de las cataratas
df_final_cataratas<-df_final%>%
  group_by(Comunidad.autónoma)%>%
  select(Media_horas_sol,clasificacion,Sexo,value)%>%
  arrange(desc(value))

df_final_cataratas


#Realizamos el estudio en función de los sexos
df_mujeres<-df_final%>%
  group_by(Comunidad.autónoma)%>%
  select(Media_horas_sol,clasificacion,Sexo,value)%>%
  filter(Sexo=="Mujeres")%>%
  arrange(desc(Media_horas_sol))

df_mujeres

df_hombres<- df_final%>%
  group_by(Comunidad.autónoma)%>%
  select(Media_horas_sol,clasificacion,Sexo,value)%>%
  filter(Sexo=="Hombres")%>%
  arrange(desc(Media_horas_sol))

df_hombres

#Incluimos un df para ambos sexos, pero haremos los estudios separando hombres y mujeres
df_ambos<-df_final%>%
  group_by(Comunidad.autónoma)%>%
  select(Media_horas_sol,clasificacion,Sexo,value)%>%
  filter(Sexo=="Ambos sexos")%>%
  arrange(desc(Media_horas_sol))

df_ambos

# Graficamos

library(ggplot2)
library(dplyr)

#Empezamos por el grafico de cataratas-horas de sol en las comunidades, solo para mujeres:

ggplot(data = df_mujeres, aes(x = reorder(Comunidad.autónoma,-value), y = value) ) +
  geom_bar(stat = "identity", aes(fill = Media_horas_sol) ) +
  labs(x = "Comunidad Autónoma", 
       y = "Incidencia de Cataratas (%)", 
       title = "Incidencia de Cataratas en Mujeres según Comunidad Autónoma", 
       colour = "Horas de Sol") +
  theme_classic() 

#Ceuta y Melilla no tienen datos de horas de sol. Además, el porcentaje de cataratas no es significativo, ya que cuentan con poca población y no es un porcentaje real, por lo que no debemos tenerlo en cuenta.

#Realizamos el mismo gráfico para hombres
ggplot(data = df_hombres, aes(x = reorder(Comunidad.autónoma,-value), y = value) ) +
  geom_bar(stat = "identity", aes(fill = Media_horas_sol) ) +
  labs(x = "Comunidad Autónoma", 
       y = "Incidencia de Cataratas (%)", 
       title = "Incidencia de Cataratas en Hombres según Comunidad Autónoma", 
       colour = "Horas de Sol") +
  theme_classic() 

#Gráfico %cataratas, horas de sol (gráfico dispersión, además una curva para cada sexo)
#Combinamos los datos de mujeres y hombres para hacer un gráfico comparativo
df_mujeres$Sexo <- "Mujeres"
df_hombres$Sexo <- "Hombres"

df_combinado <- data.frame(
  Comunidad.autónoma = c(df_mujeres$Comunidad.autónoma, df_hombres$Comunidad.autónoma),
  Media_horas_sol = c(df_mujeres$Media_horas_sol, df_hombres$Media_horas_sol),
  clasificacion = c(df_mujeres$clasificacion, df_hombres$clasificacion),
  Sexo = c(df_mujeres$Sexo, df_hombres$Sexo),
  value = c(df_mujeres$value, df_hombres$value)
)

print(df_combinado)

ggplot(df_combinado, aes(x = Media_horas_sol, y = value , color = Sexo)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  labs(
    title = "Relación entre horas de sol y porcentaje de cataratas",
    x = "Media de horas de sol",
    y = "Porcentaje de cataratas",
    color = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

