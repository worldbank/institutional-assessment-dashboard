## Source: https://stackoverflow.com/questions/61580973/first-and-last-facets-using-facet-wrap-with-ggplotly-are-larger-than-middle-face

fixfacets <- function(figure, facets, domain_offset){
  
  # split x ranges from 0 to 1 into
  # intervals corresponding to number of facets
  # xHi = highest x for shape
  n_facets <- length(facets)
  xHi <- seq(0, 1, len = n_facets+1)
  xHi <- xHi[2:length(xHi)]
  
  xOs <- domain_offset
  
  # Shape manipulations, identified by dark grey backround: "rgba(217,217,217,1)"
  # structure: p$x$layout$shapes[[2]]$
  shp <- figure$x$layout$shapes
  j <- 1
  for (i in seq_along(shp)){
    if (shp[[i]]$fillcolor=="rgba(217,217,217,1)" & (!is.na(shp[[i]]$fillcolor))){
      #$x$layout$shapes[[i]]$fillcolor <- 'rgba(0,0,255,0.5)' # optionally change color for each label shape
      figure$x$layout$shapes[[i]]$x1 <- xHi[j]
      figure$x$layout$shapes[[i]]$x0 <- (xHi[j] - xOs)
      #figure$x$layout$shapes[[i]]$y <- -0.05
      j<-j+1
    }
  }
  
  # annotation manipulations, identified by label name
  # structure: p$x$layout$annotations[[2]]
  ann <- figure$x$layout$annotations
  annos <- facets
  j <- 1
  for (i in seq_along(ann)){
    if (ann[[i]]$text %in% annos){
      # but each annotation between high and low x,
      # and set adjustment to center
      figure$x$layout$annotations[[i]]$x <- (((xHi[j]-xOs)+xHi[j])/2)
      figure$x$layout$annotations[[i]]$xanchor <- 'center'
      #print(figure$x$layout$annotations[[i]]$y)
      #figure$x$layout$annotations[[i]]$y <- -0.05
      j<-j+1
    }
  }
  
  # domain manipulations
  # set high and low x for each facet domain
  xax <- names(figure$x$layout)
  j <- 1
  for (i in seq_along(xax)){
    if (!is.na(pmatch('xaxis', lot[i]))){
      #print(p[['x']][['layout']][[lot[i]]][['domain']][2])
      figure[['x']][['layout']][[xax[i]]][['domain']][2] <- xHi[j]
      figure[['x']][['layout']][[xax[i]]][['domain']][1] <- xHi[j] - xOs
      j<-j+1
    }
  }
  
  return(figure)
}