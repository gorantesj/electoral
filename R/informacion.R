# Info de la eleccion
#' Title
#'
#' @param partidos
#' @param coaliciones
#' @param colores
#' @param bd
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
                                      nombre_analisis
                                      ){
  info <- list()
  info$competidores <- c(partidos,
                         map(coaliciones, ~glue::glue('c_{paste(.x, collapse = "_")}')) %>%
                           reduce(c)) %>% sort()
  info$coaliciones <- coaliciones
  info$colores <- colores
  info$unidad_analisis <- unidad_analisis
  info$id_unidad_analisis <- id_unidad_analisis
  # Preparar base
  # Renombrar
  bd<- rename_with(bd, tolower)
  # Volver numérica y agregar prefijo
  bd <- bd %>%
    rename_with(~glue::glue("votos_{.x}"),
                .cols=contains(c(coaliciones %>% unlist(), partidos))) %>%
    mutate(across(starts_with("votos_"), ~as.numeric(.x)))
  bases <- map(coaliciones, ~votos_coalicion(bd, .x))
  info$bd <- bases %>% reduce(full_join) %>%
    filter(tipo_casilla!="S")

  return(info)
}

# Votos coalicion
#' Title
#'
#' @param bd
#' @param partidos
#'
#' @return
#' @export
#'
#' @examples
votos_coalicion <- function(bd, partidos){
  coal <- paste(partidos, collapse = "_")
  combinaciones <- map(.x = 1:length(partidos),
                       ~paste0("^votos_",combinaciones_n(partidos = partidos, .x), "$")) %>%
                         reduce(c)

  bd <- bd %>% rowwise() %>%
    mutate("votos_c_{coal}":=sum(c_across(matches(combinaciones)), na.rm = T)) %>%
    ungroup()
  return(bd)
}

# Combinaciones n
combinaciones_n <- function(partidos, n){
  partidos <- matrix(partidos,nrow = length(partidos), ncol = n) %>% as_tibble()
  partidos <- partidos %>% expand(!!!rlang::syms(names(partidos))) %>%
    unite("combinacion") %>%
    pull(combinacion)
  return(partidos)
}
