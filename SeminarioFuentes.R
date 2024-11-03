system("git init")  # Inicializa git si no lo has hecho aún
system('git remote add origin https://github.com/tu_usuario/nombre_repositorio.git')
system('git branch -M main')  # Asegúrate de que la rama principal se llame 'main'
system('git add .')  # Añade todos los archivos
system('git commit -m "Inicializar proyecto en GitHub"')  # Realiza el primer commit
system('git push -u origin main')  # Sube los archivos al repositorio remoto
#Comentario


install.packages("jsonlite")
install.packages("pxR")

library(jsonlite)
library(pxR)

datos_cataratas<-fromJSON("/Users/niameyreyclar/SeminarioFuentes/datos_cataratas.json")
datos_solar<-read.px("/Users/niameyreyclar/Desktop/ING/TERCERO/FUENTES DE DATOS/SeminarioFuentes/datos_clima_asturias.px")

df_datos_solar<-as.data.frame(datos_solar)

summary(datos_cataratas)
summary(df_datos_solar)

str(datos_cataratas)

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






