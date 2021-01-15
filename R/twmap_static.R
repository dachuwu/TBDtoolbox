#' @md
#' @title Create static maps for geo-specific measurements
#'
#' @param geo.code area code in MOI format.
#' @param x plotted variable for each area. Must be an integer or numeric vector with the same length of \code{geo.code}.
#' @param geo.level the geographic level of \code{geo.code}. One of "town", "county", "region", and "nation".
#' @param x.name the name of the plotted variable `x` which is the legend title.
#' @param col.control a list of  parameters controlling the coloring scale.
#'
#' (1) `option`: A character string indicating the colormap option to use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#'
#' (2) `dir`: Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#'
#' (3) `na.color`: the color of NA values. See also [ggplot2::scale_colour_viridis_d()].
#'
#' @param thm.control a list of  parameters controlling the theme of the map.
#'
#' (1) `border`: the color of the border lines.
#'
#' (2) `border.width`: the width of the border lines.
#'
#' (3) `back`: the filled color of the map background.
#'
#' @param show.island logical (default = T). Whether to show island counties on map.
#'
#' @return a ggplot object.
#'
#'
#' @importFrom magrittr %>%
#'
#' @export
twmap_static <- function(geo.code, x, geo.level = "county",
                         x.name = "xname",
                         col.control = list(option = "viridis", dir = 1, na.color = "grey50"),
                         thm.control = list(border = "white", border.width = .2, back = "grey90"),
                         show.island = T
                         ){

  # check validity
  if(class(geo.code) != "character") stop("geo.code must be characters")

  if(geo.level == "town"){
    bmap <- bord_town %>% rename(geo.code = town)
    }else if(geo.level == "county"){
    bmap <- bord_county %>% rename(geo.code = city)
  }else if(geo.level == "region"){
    bmap <- bord_region %>% rename(geo.code = region)
  }else if(geo.level == "nation"){
    bmap <- bord_nation %>% rename(geo.code = nation)
  # }else if(geo.level == "urbanicity"){
  #   bmap <- bord_urban %>% rename(geo.code = urb)
  }else{
    stop("fail to specify the geographic level argument : geo.level")
  }

  if(!show.island){
    bmap <- bmap %>%
      filter(!substr(geo.code,1,5) %in% c("10016","09007","09020") & geo.code != "Island")
  }
  if(!all(geo.code %in% bmap$geo.code)){
    ind <- which(geo.code %in% bmap$geo.code)
    geo.code <- geo.code[ind]
    x <- x[ind]
    warning("geo.code contains unrecognizable codes which will be ignored")
  }


  if(length(geo.code) != length(x)) stop("inconsistent lengths of geo.code and x")
  if(any(duplicated(geo.code))) stop("duplicated item(s) in geo.code")
  if(!class(x) %in% c("numeric", "integer", "factor")) stop("x must be one of numeric, integer, or factor")

  col.ctrl <- list(option = "viridis", dir = 1, na.color = "grey50")
  for(i in 1:length(col.control)){
    k <- names(col.control)[i]
    col.ctrl[[k]] <-  col.control[[k]]
  }
  thm.ctrl <- list(border = "white", border.width = .2, back = "grey90")
  for(i in 1:length(thm.control)){
    k <- names(thm.control)[i]
    thm.ctrl[[k]] <-  thm.control[[k]]
  }



  gdf <- bmap %>%
    dplyr::left_join(data.frame(
      geo.code = geo.code, xv = x, stringsAsFactors = F
    ))


  G <- ggplot2::ggplot(gdf)+
    ggplot2::geom_sf(ggplot2::aes(fill = xv, geometry = geometry),
                     color = thm.ctrl$border, size = thm.ctrl$border.width)+
    ggplot2::coord_sf(xlim = c(118.4, 123.3), ylim = c(21.85, 25.38), ndiscr = 0)+
    ggplot2::labs(x="",y="")+
    ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = thm.ctrl$back, color = thm.ctrl$back),
          plot.margin = ggplot2::unit(c(0,0,0,0), "cm"),
          panel.background = ggplot2::element_rect(fill = thm.ctrl$back, color = thm.ctrl$back),
          legend.background = ggplot2::element_blank(),
          legend.justification = c(1.1, 0),
          legend.position = c(1, 0))


  if(class(gdf$xv) %in% c("numeric", "integer")){
    G <- G +
      ggplot2::scale_fill_viridis_c(option = col.ctrl$option, direction = col.ctrl$dir,
                                    na.value = col.ctrl$na.color)+
      ggplot2::guides(fill = ggplot2::guide_colorbar(title = x.name))
  } else if(class(gdf$xv) == "factor"){
    G <- G +
      ggplot2::scale_fill_viridis_d(option = col.ctrl$option, direction = col.ctrl$dir,
                                    na.value =  col.ctrl$na.color,
                                    drop = F)+
      ggplot2::guides(fill = ggplot2::guide_legend(title = x.name))
  }


  return(G)

  }
