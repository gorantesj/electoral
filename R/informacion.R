# Info de la eleccion
#' La función básica del paquete. Ayuda a comunicar toda la información necesaria para los futuros procesamientos y gráficas.
#'
#' @param partidos vector en minúscula de los partidos **sin coalición** que participan en la elección.
#' @param coaliciones lista de vectores que se encuentran coaligados.
#' @param colores lista de vectores de colores con nombre. Los nombres corresponden al partido, o coalición para el que el color es asignado. Por mejorar.
#' @param bd base de datos con con formato homologado. Combinaciones de partidos separados por  '_'
#'
#' @return
#' @export
#'
#' @examples
preparar_info_de_eleccion <- function(partidos,
                                      coaliciones,
                                      colores,
                                      bd,
                                      unidad_analisis,
                                      id_unidad_analisis,
                                      año_analisis,
                                      nombre_unidad_analisis
                                      ){
  info <- list()
  info$competidores <- c(partidos,
                         map(coaliciones,
                             ~glue::glue('coalición_{paste(.x, collapse = " ")}')) %>%
                           reduce(c)) %>% sort()
  info$coaliciones <- coaliciones
  info$colores <- colores
  info$unidad_analisis <- unidad_analisis
  info$id_unidad_analisis <- id_unidad_analisis
  info$nombre_unidad_analisis <- nombre_unidad_analisis
  info$año_analisis <- año_analisis
  # Preparar base
  # Renombrar
  bd<- rename_with(bd, tolower)
  # Volver numérica y agregar prefijo
  bd <- bd %>%
    rename_with(~glue::glue("votos_{.x}"),
                .cols=contains(c(coaliciones %>% unlist(), partidos))) %>%
    mutate(across(starts_with("votos_"), ~as.numeric(.x)))
  # Correr coaliciones
  bases <- map(coaliciones, ~votos_coalicion(bd, .x))
  info$bd <- bases %>% reduce(full_join)


  return(info)
}

# Votos coalicion
#' Title
#'
#' @param bd Base de datos de votación.
#' @param partidos Vector de partidos que se encuentran coaligados. No importa el orden.
#'
#' @return
#' @export
#'
#' @examples
votos_coalicion <- function(bd, partidos){
  coal <- paste(partidos, collapse = " ")
  bd <- bd %>%
    rowwise() %>%
    mutate("votos_coalición_{coal}":=sum(c_across(matches(paste(partidos, collapse = "|"))), na.rm = T)) %>%
    ungroup()
  return(bd)
}


