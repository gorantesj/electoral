#' Title
#'
#' @param info base de datos procesada con los votos para cada partido y cada coalición, dividida en secciones, distritos y estado.
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr ggplot2 leaflet tidyr purrr
graficar_total_votacion <- function(info){
  names(info$colores) <- stringr::str_replace(stringr::str_to_upper(names(info$colores)),
                                              pattern="_",replacement = ": ")
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
    mutate(pct=votacion/nominal,
           partido=stringr::str_replace(stringr::str_to_upper(partido),
                                        pattern="_",replacement = ": ")) %>%
    filter(votacion>0) %>%
    ggplot(aes(x = reorder(partido, votacion),
               y= votacion,
               fill=partido)) +
    geom_bar(stat="identity", position="dodge") +
    geom_vline(xintercept = 0)+
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
          panel.grid = element_blank(),
          panel.border =   element_blank(),
          axis.title = element_text(size = 14, face = "bold"),
          axis.ticks.y=element_blank(),
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
  letrero <- letrero %>%
    mutate(unidad=stringr::str_replace(
      stringr::str_to_sentence(stringr::str_to_upper(!!sym(info$unidad))),
      "_",replacement = " "))
  letrero %>%
    ggplot(aes(x = reorder(unidad,!!sym(glue::glue("votos_{analisis}"))),
               y= !!sym(glue::glue("votos_{analisis}")),
               alpha=(!!sym(info$unidad_analisis)==info$id_unidad_analisis))) +
    geom_bar(stat="identity", position="dodge", fill=color) +
    geom_text(data = letrero %>% filter(orientacion),
              aes(label=letrero), hjust=1, color="white") +
    geom_text(data = letrero %>% filter(!orientacion),
              aes(label=letrero), hjust=0, color="black") +
    labs(title ="Porcentaje de votos respecto a la lista nominal",
         subtitle=glue::glue("{info$nombre_unidad_analisis} ({analisis})"),
         x = stringr::str_replace(string=stringr::str_to_sentence(stringr::str_to_upper(info$unidad_analisis)),
                                pattern = "_",replacement = " "),
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
          panel.grid = element_blank(),
          panel.border =   element_blank(),
          axis.title = element_text(size = 14, face = "bold"),
          axis.ticks.y=element_blank(),
          axis.text = element_text(size = 10, face = "bold"))
    # theme(text = element_text(color = "grey35"),
    #       plot.title = element_text(size = 15, face = "bold",  color = "grey35"),
    #       plot.subtitle = element_text(size = 10, face = "bold", colour = "#666666"),
    #       plot.caption = element_text(size = 10),
    #       legend.position = "none",
    #       axis.title = element_text(size = 14, face = "bold"),
    #       axis.text = element_text(size = 10, face = "bold"))


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


#' Title Gráfica en un mapa los resultados electorales por diferentes niveles de agregación.
#'
#' @param info base de datos procesada con los votos para cada partido y cada coalición, dividida en secciones, distritos y estado.
#' @param sf shapefile que se va a utilizar como la base del mapa
#' @param analisis colición para la cual se va a graficar su distribución de votos
#' @param nivel nivel cartográfico de interés para el mapa
#' @param interactiva función lógico en la que F es un mapa fijo y V un mapa interactivo
#'
#' @return
#' @export
#'
#' @examples
graficar_fuerza_electoral <- function(info, sf, analisis,nivel, interactiva=F){
  nivel_mapa <- switch (nivel,
    seccion = "SECCION", distrito="DISTRITO_F"
  )
  color <- info$colores[analisis]
  distrito <- info$bd %>% filter(!!sym(info$unidad_analisis)==info$id_unidad_analisis)
  datos <- distrito %>%
    group_by(!!sym(nivel)) %>%
    summarise(across(glue::glue("votos_{info$competidores}"),
                     ~sum(.x, na.rm = T)/sum(nominal, na.rm = T)))
  datos <- datos[is.finite(rowSums(datos)),]
  referencias <- datos %>%
    rowwise() %>%
    mutate(maximos=max(c_across(starts_with("votos_")),na.rm = T)) %>%
    ungroup() %>%
    summarise(maximo=max(maximos, na.rm = T),
              mediana=quantile(maximos, na.rm=T, probs=.5))
  maximo <- referencias %>% pull(maximo)
  mediana <- referencias %>% pull(mediana)
  datos <- datos %>% select(!!sym(nivel),glue::glue("votos_{analisis}" ))
  sf<-sf %>% rename("{nivel}":=nivel_mapa)
  mapa <- sf %>% left_join(datos)
  mapa <- mapa %>%
    mutate(reescala=scales::rescale_mid(scales::rescale(!!sym(glue::glue("votos_{analisis}")),
                                    from = c(0,maximo),
                                    to=c(0,1)),
                                    from=c(0,1),
                                    to=c(0,1),
                                    mid=scales::rescale(mediana, from=c(0, maximo), to=c(0,1))))
  pal <- scales::colour_ramp(c(colortools::complementary(color = color, plot = F)[[2]],
                       "white",
                       color),
                     na.color = "grey30", alpha = FALSE)
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
      paste(nivel, mapa %>% pull(!!sym(nivel))),
      paste(glue::glue("{analisis}"),
            mapa %>% pull(!!sym(glue::glue("votos_{analisis}"))) %>%
              scales::percent(accuracy = 1))
    ) %>%
      lapply(htmltools::HTML)
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        data=mapa,
        fillColor = ~reescala,
        weight = 1,
        opacity = 1,
        color = "grey",
        group = "Morena",
        dashArray = "1",
        fillOpacity = 0.7,
        label = labels,
        layerId = mapa[[nivel]],
        highlight = highlightOptions(weight = 1, color = "black", fillOpacity = 1, bringToFront = T, sendToBack = T),
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
  }

}

#' Title Gráfica en un mapa los resultados electorales por diferentes niveles de agregación.
#'
#' @param info base de datos procesada con los votos para cada partido y cada coalición, dividida en secciones, distritos y estado.
#' @param sf shapefile que se va a utilizar como la base del mapa
#' @param analisis colición para la cual se va a graficar su distribución de votos
#' @param nivel nivel cartográfico de interés para el mapa
#' @param interactiva función lógico en la que F es un mapa fijo y V un mapa interactivo
#'
#' @return
#' @export
#'
#' @examples

fuerza_electoral_proxy <- function(proxy, info, sf, analisis,nivel, interactiva=F){
  nivel_mapa <- switch (nivel,
                        seccion = "SECCION", distrito="DISTRITO_F"
  )
  color <- info$colores[analisis]
  distrito <- info$bd %>% filter(!!sym(info$unidad_analisis)==info$id_unidad_analisis)
  datos <- distrito %>%
    group_by(!!sym(nivel)) %>%
    summarise(across(glue::glue("votos_{info$competidores}"),
                     ~sum(.x, na.rm = T)/sum(nominal, na.rm = T)))
  datos <- datos[is.finite(rowSums(datos)),]
  referencias <- datos %>%
    rowwise() %>%
    mutate(maximos=max(c_across(starts_with("votos_")),na.rm = T)) %>%
    ungroup() %>%
    summarise(maximo=max(maximos, na.rm = T),
              mediana=quantile(maximos, na.rm=T, probs=.5))
  maximo <- referencias %>% pull(maximo)
  mediana <- referencias %>% pull(mediana)
  datos <- datos %>% select(!!sym(nivel),glue::glue("votos_{analisis}" ))
  sf<-sf %>% rename("{nivel}":=nivel_mapa)
  mapa <- sf %>% left_join(datos)
  mapa <- mapa %>%
    mutate(reescala=scales::rescale_mid(scales::rescale(!!sym(glue::glue("votos_{analisis}")),
                                                        from = c(0,maximo),
                                                        to=c(0,1)),
                                        from=c(0,1),
                                        to=c(0,1),
                                        mid=scales::rescale(mediana, from=c(0, maximo), to=c(0,1))))
  pal <- scales::colour_ramp(c(colortools::complementary(color = color, plot = F)[[2]],
                               "white",
                               color),
                             na.color = "grey30", alpha = FALSE)
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
      paste(nivel, mapa %>% pull(!!sym(nivel))),
      paste(glue::glue("{analisis}"),
            mapa %>% pull(!!sym(glue::glue("votos_{analisis}"))) %>%
              scales::percent(accuracy = 1))
    ) %>%
      lapply(htmltools::HTML)

      leaflet::addPolygons(map = proxy,
        data=mapa,
        fillColor = ~reescala,
        weight = 1,
        opacity = 1,
        color = "grey",
        group = "seccion",
        dashArray = "1",
        fillOpacity = 0.7,
        label = labels,
        layerId = mapa[[nivel]],
        highlight = highlightOptions(weight = 1, color = "black", fillOpacity = 1, bringToFront = T),
        labelOptions = leaflet::labelOptions(
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

#' Title
#'
#' @param partidos
#' @param info_base
#' @param info_contraste
#'
#' @return
#' @export
#'
#' @examples
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

