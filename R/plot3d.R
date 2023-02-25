#'  generate xyz points for ellipse
#' @author Sven Gastauer
gel=function(np=100, xc=0,yc=0, h=1,w=1,phi=0){
  w=w/2
  h=h/2
  phi <- (phi + 90) * pi / 180 #degrees to radians
  t <- seq(0, 2*pi, length.out = np) # Compute angular points
  x <- xc + w*cos(t)*cos(phi) - h*sin(t)*sin(phi) #X coordinates
  y <- y <- yc + w * cos(t) * sin(phi) + h * sin(t) * cos(phi) #y coordinates
  return(cbind(x,y))
}

#####################################################################
#####################################################################
#'  get xyz points for ellipse from top and side view
#' @author Sven Gastauer
getxyz<-function(xxy,xxy2){
  pd=data.frame()
  for(i in 1:nrow(xxy)){
    o=data.frame(gel(w=xxy$dy[i],h=xxy2$dy[i], xc=xxy$X[i], yc=(xxy$Ymax[i]+xxy$Ymin[i])/2))
    o$z=xxy$X[i]
    pd=rbind(pd,o)
  }
  return(pd)
}
#####################################################################
#####################################################################
#' Get ellipse
#' @author Sven Gastauer
#' @description Get ellipse to build Plotly 3D plot of shape information
#' @param x start position x, defaults 0
#' @param y start position y, defaults 0
#' @param major radius major axis, defaults 1
#' @param minor radius minor axis, defaults 1
#' @param theta rotaiton angle, defaults 0
#' @param nv number of z points, defaults 10
#' @export
ellipse <- function (x=0, y=0, major = 1, minor = 1, theta = 0, nv = 30){
  major=major/2
  minor=minor/2
  angle <- theta * pi/180
  segment <- c(0, 360)
  segment <- segment * pi/180

  z <- seq(segment[1], segment[2], length = nv + 1)
  xx <- minor * cos(z)
  yy <- major * sin(z)
  alpha <- atan2(xx, yy)
  rad <- sqrt(xx^2 + yy^2)
  xp <- rad * cos(alpha + angle) + x
  yp <- rad * sin(alpha + angle) + y

  return(data.frame(x=xp,y=yp))
}
#####################################################################
#####################################################################
#' Get 3d shape plot
#' @author Sven Gastauer
#' @description Plotly 3D plot of shape information
#' @param shape list or dataframe containing the shape information
#' @export
#' @examples
#' get_shp3d(list(data(pil_fb), data(pil_sb)))
get_shp3d<-function(shape){
  #If shape is list, rbind into dataframe
  if(class(shape)=="list"){
    nmax=max(sapply(shape, nrow)) #maximum rows
    pad_df = function(df,namx){
      mat <- matrix(NA,ncol=ncol(df), nrow=nmax)
      mat[1:nrow(df),] <- as.matrix(df)
      out = as.data.frame(mat)
      names(out)=names(df)
      return(out)
    }

    for(x in 1:length(shape)){shape[[x]] = pad_df(shape[[x]],nmax)}
    shape = do.call("cbind", shape)
  }
  #######################################
  #Detect shapes in shape
  zL.ind = grep("z*L$", names(shape))
  zU.ind = grep("z*U$", names(shape))
  x.ind = grep("x", names(shape))
  w.ind = grep("w", names(shape))

  nbp = length(zL.ind) #number of body parts
  message(Sys.time(),":", nbp," Body parts detected")

  for(nn in 1:nbp){
    fb=do.call('rbind',lapply(1:length((shape[,x.ind[nn]])), FUN=function(i){
    ellipse(x=0,
            y=shape[i,zL.ind[nn]] + abs(shape[i,zU.ind[nn]]-shape[i,zL.ind[nn]])/2,
            major=shape[i,w.ind[nn]],
            minor=abs(shape[i,zU.ind[nn]] - shape[i,zL.ind[nn]]), nv=100) %>% mutate(z=shape[i,x.ind[nn]])
  }))
    if(nn==1){
      fig=plot_ly(data=fb, z = ~-y, y = ~x, x = ~z,
              #width=0.1,
              opacity=0.1, type = 'scatter3d', mode = 'lines', name='Shp_1')
    }else{
      fig <- fig %>% add_trace(data=fb,z = ~-y, y = ~x, x = ~z, type = 'scatter3d', mode = 'lines',name=paste0("Shp_",nn))
    }
  }
  scene = list(aspectmode='data', camera = list(eye = list(x = 1.5, y = 2.2, z = 1) ))

  return(  fig%>%layout(scene = scene))

}
