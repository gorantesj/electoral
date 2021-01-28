#' Title
#'
#' @param info
#'
#' @return
#' @export
#'
#' @examples
graficar_total_votacion <- function(info){
  # browser()
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
    filter(votacion>0) %>%
    ggplot(aes(x = reorder(partido, -votacion), y= votacion, fill=partido)) +
    geom_bar(stat="identity", position="dodge") +
    ggfittext::geom_bar_text(outside = T,contrast = T,
                             aes(label=glue::glue("{scales::comma(votacion, accuracy=1)}\n
                                                  ({scales::percent(pct)})")))+
    scale_fill_manual(values=info$colores) +
    labs(title ="Resultados elección",
         subtitle=info$nombre_unidad_analisis, x = "",
         y = "Votos",
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
          axis.text = element_text(size = 10, face = "bold")) +
    coord_flip()


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
graficar_total_comparativo <- function(info, analisis, mostrar=7){
  color <- info$colores[analisis]
  datos <- info$bd %>%
    group_by(!!sym(info$unidad)) %>%
    summarise(across(contains(analisis),
                     ~sum(.x, na.rm=T)/sum(nominal, na.rm = T)))
  letrero <- datos %>% mutate(across(contains(analisis),~rank(-.x),.names = "posicion"),
                              letrero=glue::glue("Lugar {posicion} de {max(posicion)}"),
                              orientacion=if_else(!!sym(glue::glue("votos_{analisis}"))>.1*max(!!sym(glue::glue("votos_{analisis}"))),T,F))
  referencia <- letrero %>% filter(!!sym(info$unidad_analisis)==info$id_unidad_analisis) %>% pull(posicion)
  letrero <- letrero %>% filter(posicion>=(referencia-(mostrar-1)/2),
                                posicion<=(referencia+(mostrar-1)/2))
  letrero %>%
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
graficar_distibucion <- function(info=info, analisis, comparar=F){
  if(!comparar) info$bd <- info$bd %>% filter(!!sym(info$unidad_analisis)==info$id_unidad_analisis)
  color <- info$colores[analisis]
  g <- info$bd %>%
    mutate(across(contains(analisis), ~.x/nominal)) %>%
    ggplot(aes(x=as.factor(!!sym(info$unidad)),
               y=!!sym(glue::glue("votos_{analisis}")),
               alpha=!!sym(info$unidad_analisis)==info$id_unidad_analisis)) +
    geom_boxplot(fill=color) +
    scale_alpha_manual(values=c(0.5,1)) +
    coord_flip()+
    scale_y_continuous(labels = scales::percent_format())+
    labs(title ="Distribución del porcentaje de votación respecto a la lista nominal",
         subtitle=glue::glue("{info$nombre_unidad_analisis} (Análisis por casilla)"),
         x = "",
         y = "Tasa de votación",
         caption = stringr::str_wrap(glue::glue("Fuente: Elaborado por Morant Consultores con información de los Cómputos distritales {info$año_analisis} - INE"), 100), fill="") +
    theme_bw()+
    theme(text = element_text(color = "grey35"),
          plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
          plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
          plot.caption = element_text(size = 10),
          legend.position = "none",
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10, face = "bold"))
  return(g)
}

#' Title
#'
#' @param info
#' @param sf
#' @param analisis
#'
#' @return
#' @export
#'
#' @examples
graficar_fuerza_electoral <- function(info, sf, analisis, interactiva=F){
  color <- info$colores[analisis]
  distrito <- info$bd %>% filter(!!sym(info$unidad_analisis)==info$id_unidad_analisis)
  datos <- distrito %>%
    group_by(seccion) %>%
    summarise(across(glue::glue("votos_{info$competidores}"),
                     ~sum(.x, na.rm = T)/sum(nominal, na.rm = T)))
  referencias <- datos %>%
    rowwise() %>%
    mutate(maximos=max(c_across(starts_with("votos_")),na.rm = T)) %>%
    ungroup() %>%
    summarise(maximo=max(c_across(starts_with("votos_")), na.rm = T),
              mediana=quantile(maximos, na.rm=T, probs=.5))
  maximo <- referencias %>% pull(maximo)
  mediana <- referencias %>% pull(mediana)
  datos <- datos %>% select(seccion,glue::glue("votos_{analisis}" ))
  mapa <- sf %>% right_join(datos, by = c("SECCION"="seccion"))
  mapa <- mapa %>%
    mutate(reescala=scales::rescale_mid(scales::rescale_max(!!sym(glue::glue("votos_{analisis}")),
                                                            from = c(0,maximo)),
                                        mid = scales::rescale_max(mediana,
                                                                  from =c(0, maximo))))
  pal <- scales::gradient_n_pal(values =mapa$reescala ,
                                c(colortools::complementary(color = color, plot = F)[[2]],
                                  "white",
                                  color))
  mapa <- mapa %>% mutate(reescala=pal(reescala))
  if(!interactiva){
    ggplot() +
      geom_sf(data = mapa,
              aes(fill=reescala,
                  geometry=geometry),size=.05, colour = "black",alpha=1) +
      scale_fill_identity()+
      ggtitle(label= "Distribución de apoyo  - Total de votos entre lista nominal")+
      theme_void() +
      labs(fill="Votos entre \nlista nominal") +
      theme(text=element_text(family="Georgia"),
            plot.title = element_text(family="Georgia", size = 11, hjust = 0.5),
            legend.title = element_text(face = "bold", family = "Georgia", size=8),
            legend.text = element_text(family = "Georgia", size=8),
            plot.subtitle = element_text(size=5),
      )

  }
  else{

    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      paste("Seccion: ", mapa$SECCION),
      paste(glue::glue("{analisis}"),
            mapa %>% pull(!!sym(glue::glue("votos_{analisis}"))) %>%
              scales::percent(accuracy = 1))
    ) %>%
      lapply(htmltools::HTML)
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data=mapa,
        fillColor = ~reescala,
        weight = 2,
        opacity = 1,
        color = "grey",
        group = "Morena",
        dashArray = "1",
        fillOpacity = 0.7,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
  }

}

#' Title
#'
#' @param info
#' @param sf
#'
#' @return
#' @export
#'
#' @examples
graficar_mapa_ganadores <- function(info, sf){
  datos <- info$bd %>%
    filter(!!sym(info$unidad_analisis)==info$id_unidad_analisis) %>%
    group_by(seccion) %>%
    summarise(across(glue::glue("votos_{info$competidores}"), ~sum(.x,na.rm = T))) %>%
    rowwise() %>%
    mutate(ganador=info$competidores[which.max(c_across(glue::glue("votos_{info$competidores}")))])
  mapa <- left_join(datos,  sf,by=c("seccion"="SECCION"))
  ggplot() +
    geom_sf(data = mapa,
            aes(fill=ganador, geometry=geometry),
            size=.05, colour = "black",alpha=0.6) +
    scale_fill_manual(values = info$colores) +
    ggtitle(label= "Ganadores")+
    theme_void() +
    theme(text=element_text(family="Georgia"),
          plot.title = element_text(family="Georgia", size = 11, hjust = 0.5),
          legend.title = element_text(face = "bold", family = "Georgia", size=8),
          legend.text = element_text(family = "Georgia", size=8),
          plot.subtitle = element_text(size=5),
    )

}

graficar_correlacion_partidista <- function(partidos, info_base, info_contraste){
  b1 <- info_base$bd %>%
    group_by(seccion) %>%
    summarise(across(glue::glue("votos_{info$competidores}"),
                     ~sum(.x, na.rm = T)/sum(nominal, na.rm = T))) %>%
    rename_with(.cols=-seccion, ~glue::glue("{.x}-df"))

  b2 <- info_contraste$bd %>%
    group_by(seccion) %>%
    summarise(across(glue::glue("votos_{info2$competidores}"),
                     ~sum(.x, na.rm = T)/sum(nominal, na.rm = T)))%>%
    rename_with(.cols=-seccion, ~glue::glue("{.x}-pr"))

  bd_completa <- inner_join(b1, b2,by="seccion")
  datos <- bd_completa %>%
    select(-seccion) %>%
    corrr::correlate() %>%
    corrr::shave() %>%
    pivot_longer(cols = -term, names_prefix="votos_", values_drop_na=T) %>%
    mutate(term=stringr::str_replace(term, pattern = "votos_", replacement = "")) %>%
    separate(col = term, sep="-", into=c("partido2","cargo2")) %>%
    separate(name, sep="-", into=c("partido1","cargo1"))
  datos <- datos %>% mutate(conservar=F)

  for(i in 1:length(partidos)){
    datos <- datos %>%
      mutate(conservar=(grepl(glue::glue("(\\b|_){partidos[i]}\\b"), partido1) &
                          grepl(glue::glue("(\\b|_){partidos[i]}\\b"), partido2) | conservar))

  }
  datos %>%
    filter(conservar) %>%
    ggplot()+
    geom_segment(aes(x=1, xend=1, y=0, yend=1),
                 lineend = "round", size=6, color="#F2F7F2")+
    geom_segment(aes(x=1, xend=1, y=0, yend=value, color=partido1),
                 lineend = "round", size=3)+
    geom_text(aes(x=0, y=0, color=partido1,
                  label=glue::glue("{round(value,2)}")), size=10) +
    geom_text(aes(x=1.5, y=0, color=partido1,
                  label=glue::glue("{partido1}"), vjust=1, size=10)) +
    coord_polar(theta="y")+
    scale_y_continuous(limits = c(0,1))+
    scale_x_continuous(limits = c(0,1.5))+
    scale_color_manual(values = info$colores,guide=NULL)+
    theme_void()+
    theme(text = element_text(color = "grey35",size = 20),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
          plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
          plot.caption = element_text(size = 10),
          legend.position = "none") +
    facet_wrap(~partido1)
}

