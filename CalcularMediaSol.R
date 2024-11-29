#Realizar media de horas de sol por comunidad
calcular_media_horas_sol <- function(df_sol_largo) {
  media_horas_sol <- df_sol_largo %>%
    group_by(Comunidad) %>%
    summarise(MediaHorasSol = mean(`Horas de sol`, na.rm = TRUE))
  
  return(media_horas_sol)
}