library(dplyr)
library(sf)
library(tidyr)


#' scale between 0 and 1 imagej function
#' @author Sven Gastauer
range01img <- function(xy,xy2){
  dy=(max(xy$x)-min(xy$x))
  dx=(max(xy$x)-min(xy$x))
  xmin = min(xy$x)
  #ymin = min(xy$y)

  xy$y = (xy$y)/dy
  xy$x=(xy$x-xmin)/dx

  xy2$y = (xy2$y)/dy
  xy2$x=(xy2$x-xmin)/dx


  return(list(xy,xy2))}

#' getx
#' @author Sven Gastauer
getx=function(xy, n=0.01){
  xy=rbind(xy,xy[1,])
  spol = sf::st_polygon(list(as.matrix(xy)))
  ns=length(seq(0, max(xy$x), by=n))
  table_xy <- data.frame(

    x = rep(seq(0, max(xy$x), by=n), each=2),
    y = rep(c(-50,50),times=ns),
    id = rep(seq(1,ns), each=2)) %>%
    group_by(id) %>%
    st_as_sf(coords = c("x", "y")) %>% # create points
    group_by(id) %>%
    summarise() %>% # union points into lines using our created lineid
    st_cast("LINESTRING")

  i1=sf::st_intersection(table_xy,spol)

  ixy=data.frame()
  i1$geo=sapply(1:nrow(i1), FUN=function(o)class(i1$geometry[o])[1])
  i1=i1%>%filter(geo!="sfc_GEOMETRYCOLLECTION")

  for(x in 1:length(i1$geometry)){
    tmp=data.frame(st_coordinates(i1$geometry[x][1]))
    if(nrow(tmp)>0){
      tmp$L1=x
      tmp=tmp[,1:3]
      tmp=tmp%>%group_by(X)%>%
        summarise(Ymin=min(Y),
                  Ymax=max(Y))
      if(nrow(tmp)==1){tmp=rbind(tmp,tmp)}
      ixy=rbind(ixy,tmp)
    }
  }
  ixy$dy=ixy$Ymax - ixy$Ymin
  return(ixy)
}
##################################

#' ImageJ Extracts to KRM shape
#' @author Sven Gastauer
#' @description Plotly 3D plot of shape information
#' @param shp dataframe containing the shape information, with at least 3 colums, contianing x,y and descriptor of shape
#' @param dorsal names of shapes from dorsal aspect
#' @param lateral names of shapes from lateral aspect
#' @param body position of the largest body in the shape names, defaults 1
#' @param xy position of the xy coordinates in the dataframe defaults c(1,2)
#' @param nam position of name of the shape, defaults 3
#' @export
Imagej2shp =function(shp, dorsal=c("Dorsal_body","Dorsal_bladder"), lateral=c("Lateral_body", "Lateral_bladder"), body=1, xy=c(1,2), nam=3, n=0.01){

  #rename columns
  names(shp)[xy] = c("x","y")
  names(shp)[nam] = "part"

  #non body parts
  selp = 1:length(dorsal)
  selp = selp[selp!=body]

  shp$x=-shp$x
  shp$y=-shp$y

  shp$xs=NA;shp$ys=NA

  #adjust dorsal

  if(length(selp) != 0){
    for(i in selp){
      tmp = shp%>%filter(part==dorsal[body])%>%select(x,y)
      tmp2 = shp%>%filter(part==dorsal[i])%>%select(x,y)
      tmp = range01img(tmp, tmp2)
      shp[shp$part == dorsal[i], c("xs","ys")] = tmp[[2]]
    }
  }else{
    tmp = shp%>%filter(part==dorsal[body])%>%select(x,y)
    tmp2 = shp%>%filter(part==dorsal[1])%>%select(x,y)
    tmp = range01img(tmp, tmp2)
  }
  shp[shp$part == dorsal[body], c("xs","ys")] = tmp[[1]]

  #adjust lateral
  if(length(selp) != 0){
    for(i in selp){
      tmp = shp%>%filter(part==lateral[body])%>%select(x,y)
      tmp2 = shp%>%filter(part==lateral[i])%>%select(x,y)
      tmp = range01img(tmp, tmp2)
      shp[shp$part == lateral[i], c("xs","ys")] = tmp[[2]]
    }
  }else{
    tmp = shp%>%filter(part==lateral[body])%>%select(x,y)
    tmp2 = shp%>%filter(part==lateral[1])%>%select(x,y)
    tmp = range01img(tmp, tmp2)
  }
  shp[shp$part == lateral[body], c("xs","ys")] = tmp[[1]]

  get_shp=function(fbd,fbd2){
    #side view gives z, top view gives width
    xxy=getx(fbd, n=n) #z low up
    xxy2=getx(fbd2,n=n) # width
    xxy2 = xxy2%>%filter(xxy2$X %in% xxy$X)
    xxy = xxy%>%filter(xxy$X %in% xxy2$X)
    shp = data.frame(x=xxy$X, w=xxy2$dy, z_U=xxy$Ymax, z_L=xxy$Ymin)
    return(shp)
  }
  fb=get_shp(fbd=shp[shp$part==lateral[body], c("xs","ys")],
             fbd2=shp[shp$part==dorsal[body], c("xs","ys")])
  if(length(selp)>0){
    shape = list(fb)
    parts = lapply(selp, FUN = function(x) get_shp(shp[shp$part==lateral[x],
                                                       c("xs","ys")],
                                                   shp[shp$part==dorsal[x],
                                                       c("xs","ys")]))
    for(k in 1:length(selp)){
      shape[[k+1]] = parts[[k]]
    }
  }else{return(fb)}
  return(shape)
}

