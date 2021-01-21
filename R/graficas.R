#' Title
#'
#' @param info
#'
#' @return
#' @export
#'
#' @examples
graficar_total_votacion <- function(info){
  info$bd %>%
    filter(!!sym(info$unidad_analisis)==info$id_unidad_analisis) %>%
    summarise(across(starts_with(paste("votos", info$competidores,
                                       sep="_")),
                     ~sum(.x, na.rm=T)),
              nominal=sum(nominal, na.rm = T)) %>%
    tidyr::pivot_longer(cols=starts_with("votos"),
                        names_to = "partido",
                        values_to="votacion",
                        names_prefix = "votos_") %>%
    mutate(pct=votacion/nominal) %>%
    ggplot(aes(x = reorder(partido, -votacion), y= votacion, fill=partido)) +
    geom_bar(stat="identity", position="dodge") +
    ggfittext::geom_bar_text(outside = T,
                             aes(label=glue::glue("{scales::comma(votacion, accuracy=1)}\n
                                                  ({scales::percent(pct)})")))+
    scale_fill_manual(values=info$colores) +
    labs(title ="Resultados elección",
         subtitle=info$nombre_unidad_analisis, x = "", y = "Votos",
         caption = stringr::str_wrap(glue::glue("Fuente: Elaborado por Morant Consultores con información de los Cómputos distritales {info$año_analisis} - INE"), 100),
                              fill="") +
    scale_y_continuous(labels = scales::label_comma(accuracy = 1))+
    theme_bw()+
    theme(text = element_text(color = "grey35"),
          plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
          plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
          plot.caption = element_text(size = 10),
          legend.position = "none",
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10, face = "bold"))


}

#' Title
#'
#' @param info
#' @param unidad
#' @param analisis
#'
#' @return
#' @export
#'
#' @examples
graficar_total_comparativo <- function(info, analisis){
  color <- info$colores[analisis]
  datos <- info$bd %>%
    group_by(!!sym(info$unidad)) %>%
    summarise(across(contains(analisis),
                     ~sum(.x, na.rm=T)/sum(nominal, na.rm = T)))
  letrero <- datos %>% mutate(across(contains(analisis),~rank(-.x),.names = "letrero"),
                              letrero=glue::glue("Lugar {letrero} de {max(letrero)}"),
                              orientacion=across(contains(analisis),~(.x>.1*.x)))
  datos %>%
    ggplot(aes(x = reorder(!!sym(info$unidad),!!sym(glue::glue("votos_{analisis}"))),
               y= !!sym(glue::glue("votos_{analisis}")),
               alpha=(!!sym(info$unidad_analisis)==info$id_unidad_analisis))) +
    geom_bar(stat="identity", position="dodge", fill=color) +
    geom_text(data = letrero %>% filter(orientacion),
             aes(label=letrero), hjust=1, color="white") +
    geom_text(data = letrero %>% filter(!orientacion),
              aes(label=letrero), hjust=0, color="black") +
    labs(title ="Porcentaje de votos respecto a la lista nominal",
         subtitle=glue::glue("{info$nombre_unidad_analisis} ({analisis})"),
         x = info$unidad_analisis,
         y = "Porcentaje de votación",
         caption = stringr::str_wrap(glue::glue("Fuente: Elaborado por Morant Consultores con información de los Cómputos distritales {info$año_analisis} - INE"), 100), fill="") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1))+
    scale_alpha_manual(values = c(.5, 1))+
    coord_flip()+
    theme_bw()+
    theme(text = element_text(color = "grey35"),
          plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
          plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
          plot.caption = element_text(size = 10),
          legend.position = "none",
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10, face = "bold"))


}




#' Title
#'
#' @param info
#' @param analisis
#' @param unidad
#'
#' @return
#' @export
#'
#' @examples
graficar_distibucion <- function(info=info, analisis){
  color <- info$colores[analisis]
  info$bd %>%
    mutate(across(contains(analisis), ~.x/nominal)) %>%
    ggplot(aes(x=as.factor(!!sym(info$unidad)),
               y=!!sym(glue::glue("votos_{analisis}")),
               alpha=!!sym(info$unidad_analisis)==info$id_unidad_analisis)) +
    geom_boxplot(fill=color) +
    scale_alpha_manual(values=c(1,0.1)) +
    coord_flip()+
    scale_y_continuous(labels = scales::percent_format())+
    # geom_jitter(color="grey", size=0.4, alpha=0.9) +
    labs(title ="Distribución de la participiación electoral para diputaciones federales \nTasa de votoación por cada 100 votantes registrados en secciones",
         subtitle="Coahuila 2018", x = "", y = "Tasa de votación",
         caption = "Fuente: Cómputos distritales 2018 - INE", fill="") +
    theme_bw()+
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    )
}


