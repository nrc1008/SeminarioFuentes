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


#Separar la columna comunidad.provincia con split y crear nombres para cada columna. Eliminar paso anterior
comunidad <- colnames(df_datos_solar[1])
str(df_datos_solar)
split_columnas <- strsplit(colnames(df_datos_solar[1]), split = ".")
str(split_columnas)
df_datos_solar = strsplit(as.character(df_datos_solar[1]), split = ".")

#df_solar <- as.data.frame(df_datos_solar)
#print(df_solar)
# Calcular la media de Horas de Sol por Comunidad
media_horas_sol <- tapply(df_datos_solar$Horas_de_sol, df_datos_solar$Comunidad, mean)


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

df_final_cataratas<-df_final%>%
  group_by(Comunidad.autónoma)%>%
  select(Media_horas_sol,clasificacion,Sexo,value)%>%
  arrange(desc(value))

df_final_cataratas

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
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() 

#Gráfico %, horas de sol (gráfico dispersión, además una curva para cada sexo)
#TODO



#Graficos Cataratas-Hombres:
ggplot(data = df_hombres, aes(x = reorder(Comunidad, -value), y = value, fill = Comunidad)) +
  geom_bar(stat = "identity") +
  labs(x = "Comunidad Autónoma", y = "Valor (%)", title = "Distribución de la Incidencia de Cataratas en Hombres en España") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))


ggplot(data = df_hombres, aes(x = Comunidad, y = value, color = Horas_Sol, )) +
  geom_line() +
  labs(x = "Comunidad Autónoma", y = "Incidencia de Cataratas (%)", 
       title = "Incidencia de Cataratas en Hombres según Comunidad Autónoma", 
       color = "Horas de Sol") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() 



ggplot(data = df_mujeres, aes(x = reorder(Comunidad.autónoma, -value), y = value, fill = Comunidad.autónoma)) +
  geom_bar(stat = "identity") +
  labs(x = "Comunidad Autónoma", y = "Valor (%)")
scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))



# Ordenar por horas de sol de mayor a menor
media_horas_sol_ordenada <- sort(media_horas_sol, decreasing = TRUE)

#Mostrar los resultados ordenados
print("Ordenado por Horas de Sol (Mayor a Menor):")
print(media_horas_sol_ordenada)

# Ordenar por porcentaje de población con cataratas de mayor a menor en base a la columna 'value' (porcentaje de cataratas)
df_cataratas_ordenado <- df_solo_cataratas[order(df_solo_cataratas$value, decreasing = TRUE), ]

# Mostrar los resultados ordenados
print("Ordenado por Porcentaje de Población con Cataratas (Mayor a Menor):")
print(df_cataratas_ordenado)


#Ordenar cataratas de mayor a menor
# Filtrar el dataframe para obtener solo los datos de "Ambos sexos"
df_cataratas_ambos <- subset(df_cataratas_ordenado, Sexo == "Ambos sexos")

# Seleccionar solo las columnas de Comunidad y value, y mostrar el resultado
print(df_cataratas_ambos[, c("Comunidad", "value","Sexo")])

#Como nos salen los valores repetidos, aplicamos distinct
df_unicos <- distinct(df_cataratas_ordenado[, c("Comunidad", "value")])

# Mostrar el resultado
print(df_unicos)

#Ahora ordenamos solo los datos de hombres
df_cataratas_hombres <- subset(df_cataratas_ordenado, Sexo == "Hombres")
#df_cataratas_ordenadohombres <- df_cataratas_hombres[order(-df_cataratas_hombres$value), ]
print(df_cataratas_hombres[, c("Comunidad", "value","Sexo")])

#Repetimos el proceso con las mujeres
df_cataratas_mujeres <- subset(df_cataratas_ordenado, Sexo == "Mujeres")
#df_cataratas_ordenadomujeres <- df_cataratas_mujeres[order(-df_cataratas_mujeres$value), ]
print(df_cataratas_mujeres[, c("Comunidad", "value","Sexo")])



#Empezamos analizando la relacion con solo el sexo femenino
# Aplicamos la lista de correcciones en la tabla de cataratas
df_cataratas_mujeres$Comunidad <- nombres_correcciones[df_cataratas_mujeres$Comunidad]

df_cataratas_mujeres_corregido<-nombres_correcciones[df_cataratas_mujeres$Comunidad]
df_cataratas_mujeres_corregido

#Observamos los nombres de las columnas de ambas tablas y vemos que la columna correspondiente a la comunidad autónoma tiene distintos nombres
names(df_datos_solar)#para comprobar las columnas de los df
names(df_solo_cataratas)

# Realizamos el left join entre las dos tablas usando la columna "Comunidad.autónoma" como atributo común
df_mujeres <- df_cataratas_mujeres %>%
  left_join(df_horas_sol, by = c("Comunidad"="Comunidad.autónoma")) #Realizamos un join sin necesidad de cambiar el nombre del atributo en una de las tablas

# Ver la tabla combinada
print(df_mujeres)


#Seguimos con el genero masculino
# Aplicamos la lista de correcciones en la tabla de cataratas
df_cataratas_hombres$Comunidad <- nombres_correcciones[df_cataratas_hombres$Comunidad]

df_cataratas_hombres_corregido<-nombres_correcciones[df_cataratas_hombres$Comunidad]
df_cataratas_hombres_corregido

# Realizamos el left join entre las dos tablas usando la columna "Comunidad.autónoma" como atributo común
df_hombres <- df_cataratas_hombres %>%
  left_join(df_horas_sol, by = c("Comunidad"="Comunidad.autónoma"))

# Ver la tabla combinada
print(df_hombres)


#Por ultimo hacemos un analisis de ambos sexos:
df_cataratas_ambos$Comunidad <- nombres_correcciones[df_cataratas_ambos$Comunidad]

df_cataratas_ambos_corregido<-nombres_correcciones[df_cataratas_ambos$Comunidad]
df_cataratas_ambos_corregido

# Realizamos el left join entre las dos tablas usando la columna "Comunidad.autónoma" como atributo común
df_ambos <- df_cataratas_ambos %>%
  left_join(df_horas_sol, by = c("Comunidad"="Comunidad.autónoma"))

# Ver la tabla combinada
print(df_ambos)

#Vamos a unir los datos con su columna en común, comunidad

df_datos_combinados<- left_join(df_datos_solar_ordenado_horas_sol,df_datos_cataratas_ordenado_cataratas,by="Comunidad")

df_datos_combinados<- df_datos_combinados %>%
  mutate(sol_ordenado_categoria= case_when(
    Horas_de_sol<2500 ~ "Bajo",
    Horas_de_sol>= 2500 & Horas_de_sol<3500 ~ "Medio",
    Horas_de_sol>3500 ~ "Alto"
  ))

head(df_datos_combinados)

#Explorar cómo varía el porcentaje de cataratas en las diferentes categorías de horas de sol
df_combinado %>% 
  group_by(categoria_sol) %>% 
  summarise(promedio_cataratas = mean(value, na.rm = TRUE))

#multiplicar el porcentaje de cataratas por las horas de sol para ver qué tan impactante es esa relación
df_combinado <- df_combinado %>% 
  mutate(impacto = Horas_de_sol * value) # Ver los primeros resultados 
head(df_combinado)












#cataratas_df <- datos_cataratas %>%
#unnest_wider(Data) %>% 
#mutate(enfermedad = factor(Nombre), valor = as.numeric(valor))

#solar_df <- datos_solar %>%
#as.data.frame() %>%
#mutate(fecha = as.Date(fecha), # Asegúrate de que la fecha esté en el formato correcto
#temperatura = as.numeric(temperatura)


