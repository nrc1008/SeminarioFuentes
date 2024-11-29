#PRUEBA DE CORRELACIÓN
analizar_relacion_sol_cataratas <- function(data, sexo = NULL) {
  # Filtrar los datos usando dplyr
  data_filtrado <- data %>%
    filter(if (!is.null(sexo)) Sexo == sexo else TRUE)
  
x <- df_media_horas_sol$Media_horas_sol
y <- df_final_cataratas$value

covarianza <- cov(x, y, use = "complete.obs")
correlacion <- covarianza / (sd(x, na.rm = TRUE) * sd(y, na.rm = TRUE))
}