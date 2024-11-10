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
df_solo_cataratas <- df_datos_cataratas %>%
  filter(Enfermedades == "Cataratas")

# Ver los primeros registros filtrados
head(df_solo_cataratas)
print(df_solo_cataratas)

#
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

# Calcular la media de Horas de Sol por Comunidad
media_horas_sol <- tapply(df_datos_solar$Horas_de_sol, df_datos_solar$Comunidad, mean)

# Mostrar el resultado
print(media_horas_sol)

# Renombrar las columnas para que coincidan, ya que en los datos comunidad se llama distinto
colnames(df_solo_cataratas)[colnames(df_solo_cataratas) == "Comunidad.autónoma"] <- "Comunidad"

names(df_datos_solar)#para comprobar las columnas de los df
names(df_solo_cataratas)

# Ordenar por horas de sol de mayor a menor

df_datos_solar_ordenado_horas_sol <- df_datos_solar[order(df_datos_solar$Horas_de_sol, decreasing = TRUE), ]

# Ordenar por porcentaje de población con cataratas de mayor a menor en base a la columna 'value' (porcentaje de cataratas)
df_datos_cataratas_ordenado_cataratas <- df_solo_cataratas[order(df_solo_cataratas$value, decreasing = TRUE), ]

# Mostrar los resultados ordenados
print("Ordenado por Horas de Sol (Mayor a Menor):")
print(df_datos_solar_ordenado_horas_sol)

print("Ordenado por Porcentaje de Población con Cataratas (Mayor a Menor):")
print(df_datos_cataratas_ordenado_cataratas)

media_horas_sol_ordenada <- sort(media_horas_sol, decreasing = TRUE)

print(media_horas_sol_ordenada)

#Ordenar cataratas de mayor a menor
# Filtrar el dataframe para obtener solo los datos de "Ambos sexos"
df_cataratas_ambos <- subset(df_datos_cataratas_ordenado_cataratas, Sexo == "Ambos sexos")

# Seleccionar solo las columnas de Comunidad y value, y mostrar el resultado
print(df_datos_cataratas_ordenado_cataratas[, c("Comunidad", "value","Sexo")])

#Como nos salen los valores repetidos, aplicamos distinct
df_unicos <- distinct(df_datos_cataratas_ordenado_cataratas[, c("Comunidad", "value")])

# Mostrar el resultado
print(df_unicos)

#Ahora ordenamos solo los datos de hombres
df_cataratas_hombres <- subset(df_datos_cataratas_ordenado_cataratas, Sexo == "Hombres")
   #df_cataratas_ordenadohombres <- df_cataratas_hombres[order(-df_cataratas_hombres$value), ]
print(df_cataratas_hombres[, c("Comunidad", "value","Sexo")])

#Repetimos el proceso con las mujeres
df_cataratas_mujeres <- subset(df_datos_cataratas_ordenado_cataratas, Sexo == "Mujeres")
    #df_cataratas_ordenadomujeres <- df_cataratas_mujeres[order(-df_cataratas_mujeres$value), ]
print(df_cataratas_mujeres[, c("Comunidad", "value","Sexo")])

df_cataratas_ambos <- subset(df_datos_cataratas_ordenado_cataratas, Sexo == "Ambos sexos")
print(df_cataratas_ambos[, c("Comunidad", "value","Sexo")])


#Como las comunidades en cataratas tienen un número alante (1-Andalucía) y además texto atrás, como por ejempo "Comunidad de Madrid" vamos a eliminar todo lo que no coincida con comunidad del otro df para poder juntarlos

nombres_correcciones <- c(
  "Andalucía" = "Andalucía",
  "Murcia, Región de" = "Murcia",
  "Navarra, Comunidad Foral de" = "Navarra",
  "Balears, Illes" = "Islas Baleares",
  "Canarias" = "Islas Canarias",
  "Castilla - La Mancha" = "Castilla La-Mancha",
  "Castilla y León" = "Castilla y León",
  "Cataluña" = "Cataluña",
  "Comunitat Valenciana" = "Comunidad Valenciana",
  "País Vasco" = "País Vasco",
  "Aragón" = "Aragón",
  "Asturias, Principado de" = "Asturias",
  "Galicia" = "Galicia",
  "Madrid, Comunidad de" = "Madrid",
  "Cantabria" = "Cantabria",
  "La Rioja" = "La Rioja",
  "Extremadura" = "Extremadura",
  "Ceuta" = "Ceuta",
  "Melilla" = "Melilla"
)

# Aplicamos la lista de correcciones en la tabla de cataratas
df_cataratas_mujeres$Comunidad <- nombres_correcciones[df_cataratas_mujeres$Comunidad]

df_cataratas_mujeres_corregido<-nombres_correcciones[df_cataratas_mujeres$Comunidad]
df_cataratas_mujeres_corregido

# Crear un data frame para horas de sol
df_horas_sol <- data.frame(
  Comunidad = names(media_horas_sol_ordenada),
  Horas_Sol = as.vector(media_horas_sol_ordenada)
)

df_horas_sol

# Realizamos el left join entre las dos tablas usando la columna "Comunidad.autónoma" como atributo común
df_ambas_tablas <- df_cataratas_mujeres %>%
  left_join(df_horas_sol, by = "Comunidad")

# Ver la tabla combinada
print(df_ambas_tablas)





#Vamos a unir los datos con su columna en común, comunidad

df_datos_combinados<- left_join(df_datos_solar_ordenado_horas_sol,df_datos_cataratas_ordenado_cataratas,by="Comunidad")

df_datos_combinados<- df_datos_combinados %>%
  mutate(sol_ordenado_categoria= case_when(
    Horas_de_sol<2500 ~ "Bajo",
    Horas_de_sol>= 2500 & Horas_de_sol<3500 ~ "Medio",
    Horas_de_sol>3500 ~ "Alto"
  ))

head(df_datos_combinados)

#xplorar cómo varía el porcentaje de cataratas en las diferentes categorías de horas de sol
df_combinado %>% 
  group_by(categoria_sol) %>% 
  summarise(promedio_cataratas = mean(value, na.rm = TRUE))

#multiplicar el porcentaje de cataratas por las horas de sol para ver qué tan impactante es esa relación
df_combinado <- df_combinado %>% 
  mutate(impacto = Horas_de_sol * value) # Ver los primeros resultados 
head(df_combinado)











library(ggplot2)
library(dplyr)

#cataratas_df <- datos_cataratas %>%
#unnest_wider(Data) %>% 
  #mutate(enfermedad = factor(Nombre), valor = as.numeric(valor))

#solar_df <- datos_solar %>%
#as.data.frame() %>%
  #mutate(fecha = as.Date(fecha), # Asegúrate de que la fecha esté en el formato correcto
         #temperatura = as.numeric(temperatura)


# Graficamos
ggplot(data = datos_cataratas, aes(x = reorder(enfermedad,-valor), y = valor, fill=enfermedad)) +
  geom_bar(stat = "identity")+
  labs(x = "", y = "Valor (%)", title = "Distribución incidencia cataratas en España") +
  scale_fill_gradient(palette = "Set1") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

ggplot(data = datos_cataratas, aes(x = fecha, y = temperatura, color=humedad)) +
  geom_line() +
  labs(x = "Fecha", y = "Temperatura (ºC)", title = "Temperatura a lo largo de tiempo") +
  scale_color_gradient(low = "blue", high = "red", name="Humedad (%)") +
  theme_classic()






