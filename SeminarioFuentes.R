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
df_cataratas <- df_datos_cataratas %>%
  filter(Enfermedades == "Cataratas")

# Ver los primeros registros filtrados
head(df_cataratas)
print(df_cataratas)

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

# Ordenar por horas de sol de mayor a menor
df_datos_solar_ordenado_horas_sol <- df_datos_solar

# Ordenar por porcentaje de población con cataratas de mayor a menor
df_datos_solar_ordenado_cataratas <- df_datos_solar[order(df_datos_solar$Porcentaje_cataratas, decreasing = TRUE), ]

# Mostrar los resultados ordenados
print("Ordenado por Horas de Sol (Mayor a Menor):")
print(df_datos_solar_ordenado_horas_sol)

print("Ordenado por Porcentaje de Población con Cataratas (Mayor a Menor):")
print(df_datos_solar_ordenado_cataratas)

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






