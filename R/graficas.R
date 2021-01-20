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
    scale_fill_manual(values=info$colores) +
    ggfittext::geom_bar_text(outside = T,
                             aes(label=glue::glue("{scales::comma(votacion, accuracy=1)}\n
                                                  ({scales::percent(pct)})")))+
    labs(title ="Resultados elección diputación 5 federal",
         subtitle="Coahuila 2018", x = "", y = "Votos",
         caption = "Fuente: Cómputos distritales 2018 - INE", fill="") +
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
