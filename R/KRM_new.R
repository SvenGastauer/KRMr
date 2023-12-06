#' Wavelength Lambda
#' @param  c Soundspeed m/s
#' @param frequency Hz
#' @export
#' @return lambda wavelength m
lambda <- function(c,f) {c/f}

#' Wavenumber k
#' @param  c Soundspeed m/s
#' @param frequency Hz
#' @export
#' @return wavenumber k

kcalc <- function(c,f){2 * pi / lambda(c,f)}

#' Plots the KRM input shape
#' @param x_sb Position x direction Swimbladder
#' @param x_fb Position x direction Fish body
#' @param w_sb Width at position x Swimbladder
#' @param w_fb Width at position x fish body
#' @param z_fbU Height fish body at position x, upper value
#' @param z_fbL Height fish body at position x, lower value
#' @param z_sbU Height swimbladder at position x, upper value
#' @param z_sbL Height swimbladder at position x, lower value
#' @param z_sbU2 Height second swimbladder at position x, upper value
#' @param z_sbL2 Height second swimbladder at position x, lower value
#' @param w_sb2 Width at position x second Swimbladder
#' @param x_sb2 Position x direction second Swimbladder

#' @export
#'
shplot <- function(x_fb=NULL, x_sb=NULL, w_fb=NULL, w_sb=NULL, z_fbU=NULL,
                   z_fbL=NULL, z_sbU=NULL,z_sbL=NULL,
                   x_sb2=NULL,w_sb2=NULL,z_sbU2=NULL,
                   z_sbL2=NULL,pa=FALSE){
  if(pa){par(mfrow=c(1,2))}
  if(length(x_fb)>1 & length(x_sb)>1){
    plot(x_fb,z_fbU, type='l', ylim=c(min(z_fbL), max(z_fbU)),
         asp=1,main='Lateral', xlab='x',ylab='z')
    lines(x_fb,z_fbL, type='l')
    lines(x_sb,z_sbU, type='l')
    lines(x_sb,z_sbL, type='l')
    if(length(x_sb2) > 1){
      lines(x_sb2,z_sbU2, type='l')
      lines(x_sb2,z_sbL2, type='l')
    }

    plot(x_fb,w_fb/2, type='l', ylim=c(min(z_fbL), max(z_fbU)),
         asp=1, main='Ventral', xlab='x', ylab='w')
    lines(x_fb,-w_fb/2, type='l')
    lines(x_sb,w_sb/2, type='l')
    lines(x_sb,-w_sb/2, type='l')
    if(length(x_sb2) > 1){
      lines(x_sb2,w_sb2/2, type='l')
      lines(x_sb2,-w_sb2/2, type='l')
    }
  }else if(length(x_sb)>1){
    plot(x_sb,z_sbU, type='l', ylim=c(min(z_sbL), max(z_sbU)),
         asp=1,main='Lateral', xlab='x',ylab='z')
    lines(x_sb,z_sbL, type='l')

    if(length(x_sb2)>1){
      lines(x_sb2,z_sbU2,
            asp=1,main='Lateral', xlab='x',ylab='z')
      lines(x_sb2, z_sbL2, type='l')
    }

    plot(x_sb,w_sb/2, type='l', ylim=c(min(z_sbL), max(z_sbU)),
         asp=1, main='Ventral', xlab='x', ylab='w')
    lines(x_sb,- w_sb / 2, type='l')

    if(length(x_sb2)>1){
      lines(x_sb2,w_sb2/2,
            asp=1, main='Ventral', xlab='x', ylab='w')
      lines(x_sb2,-w_sb2/2, type='l')}

  }else if(length(x_fb)>1){
    plot(x_fb,z_fbU, type='l', ylim=c(min(z_fbL), max(z_fbU)),
         asp=1,main='Lateral', xlab='x',ylab='z')
    lines(x_fb,z_fbL, type='l')

    plot(x_fb,w_fb/2, type='l', ylim=c(min(z_fbL), max(z_fbU)),
         asp=1, main='Ventral', xlab='x', ylab='w')
    lines(x_fb,-w_fb/2, type='l')
  }
}


###############################
pad_df = function(df,nmax){
  mat <- matrix(NA,ncol=ncol(df), nrow=nmax)
  mat[1:nrow(df),] <- as.matrix(df)
  out = as.data.frame(mat)
  names(out)=names(df)
  return(out)
}
#############AUTO
#' Soft model  - KRM
#' Based on equations found in 'Acoustic models of fish: The Atlantic cod (Gadus morhua)' by Clay and Horne, 1994
#' @author Sven Gastauer
#' @description Run soft KRM model
#' @param frequency Frequency in Hz defaults to 120000
#' @param theta incident angle in degrees, note that 90 degrees equals broadside incident, KRM is only stable for values between 65 and 115 degrees, defaults 90 degrees
#' @param c.w ambient soundspeed in m/s defaults to 1490
#' @param c.f Soundspeed in fish body m/s defautls to NULL (if NULL, ambient water c.w will be used)
#' @param c.b Soundspeed in swimbladder m/s defaults to 345
#' @param rho.w Ambient water Density kg/m3 defaults to 1030
#' @param rho.b Density gas bladder kg/m3 defaults to 1.24
#' @param rho.f Density fish body kg/m3 defaults to NULL  (if NULL, ambient water rho.w will be used)
#' @param x Position x direction Fish body
#' @param w Width at Position x direction Fish body
#' @param z_U Height at position x, upper value
#' @param z_L Heightat position x, lower value
#' @export
#' @examples
#' data(pil_sb)
#' f = krm.soft(frequency=120000, theta=90, c.w=1490, c.f=NULL, c.b=345, rho.w=1030, rho.f=NULL, rho.b=1.24, x=pil_sb$x_sb, w=pil_sb$w_sb, z_U=pil_sb$z_sbL)
#' sigma = abs(f)
#' TS = 20 * log10(sigma)
krm.soft= function(frequency=120000, theta=90, c.w=1490, c.f=NULL, c.b=345, rho.w=1030, rho.f=NULL, rho.b=1.24, x, w, z_U){
  if(is.null(c.f)){c.f=c.w}
  if(is.null(rho.f)){rho.f=c.w}
  #Start Modelling
  k <- kcalc(c.w,frequency) #wave number water
  k.fb <- kcalc(c.f,frequency) #wave number fluid body
  k.b <- kcalc(c.b,frequency) #wave number soft body
  Rbc = (rho.b * c.b - rho.f * c.f) / (rho.b * c.b + rho.f *c.f)#plane wave coefficient at sb
  Rwb = (rho.f * c.f - rho.w * c.w) /
    (rho.f * c.f + rho.w * c.w) #Plane wave reflection coefficent at fb

  T.T = 1 - Rwb^2 #Plane wave transmission coefficients

  # swim bladder
  if(length(x) > 1){
      a.b = zoo::rollsum(w,2) / 4 #radii swimbladder
      Ab = k * a.b / (k * a.b + 0.083) # Empirical factor in Kirchhoff approximation
      psi.p = k * a.b / (40 + k * a.b) - 1.05 # phase shift for a fluid cylinder in ray-Kirchhoff

      du.b = diff(x) * sin(theta * pi /180) #length of a volume element in rotated coordinates
      v.b = (zoo::rollsum(x,2) * cos(theta * pi/180) +
                zoo::rollsum(z_U,2) * sin(theta * pi/180)) / 2

      f.soft = sum(-1i * Rbc * T.T / (2 * sqrt(pi)) * Ab *
                     sqrt(( k * a.b + 1) * sin(theta * pi/180)) *
                     exp(-1i * (2 * k * v.b + psi.p)) * du.b)
    return(f.soft)
  }
}

###################################
#' Fluid model  - KRM
#' Based on equations found in 'Acoustic models of fish: The Atlantic cod (Gadus morhua)' by Clay and Horne, 1994
#' @author Sven Gastauer
#' @description Run fluid KRM model
#' @param frequency Frequency in Hz defaults to 120000
#' @param c.w ambient soundspeed in m/s defaults to 1490
#' @param theta incident angle in degrees, note that 90 degrees equals broadside incident, KRM is only stable for values between 65 and 115 degrees, defaults 90 degrees
#' @param c.b Soundspeed in fish body m/s defautls to 1570
#' @param rho.w Ambient water Density kg/m3 defaults to 1030
#' @param rho.b Density fish body kg/m3 defaults to 1070
#' @param x Position x direction Fish body
#' @param w Width at Position x direction Fish body
#' @param z_U Height fish body at position x, upper value
#' @param z_L Height fish body at position x, lower value
#' @export
#' @examples
#' data(pil_fb)
#' f = krm.fluid(frequency=120000, c.w=1490, c.b=1570, x=pil_fb$x_fb, w=pil_fb$w_fb, z_U=pil_fb$z_fbU, z_L = pil_fb$z_fbL, theta=90)
#' sigma=abs(f)
#' TS=20 * log10(sigma)

krm.fluid = function(frequency=120000, c.w=1490, c.b=1570, rho.w=1030, rho.b=1070, x, w, z_U, z_L, theta=90){
  k <- kcalc(c.w,frequency) #wave number  water
  k.b <- kcalc(c.b,frequency) #wave number  body
  Rwb = (rho.b * c.b - rho.w * c.w) /
    (rho.b * c.b + rho.w * c.w) #Plane wave reflection coefficent at fb
  T.T = 1 - Rwb^2 #Plane wave transmission coefficients

  a.b = zoo::rollsum(w,2) / 4

  du.b = diff(x) * sin(theta * pi / 180)
  v.bU = (zoo::rollsum(x,2) * cos(theta * pi/180) +
             zoo::rollsum(z_U,2) * sin(theta * pi/180)) /2
  v.bL = (zoo::rollsum(x,2) * cos(theta * pi/180) +
             zoo::rollsum(z_L,2) * sin(theta * pi/180)) /2

  psi.b = -pi * k.b * v.bU /
    (2 * (k.b * v.bU + 0.4))


  f.fluid = -1i * (Rwb / (2*sqrt(pi))) *
    sum(
      sqrt(k * a.b) *
        (exp(-2i * k * v.bU ) - T.T *
           exp(1i*(-2 * k * v.bU +
                     2 * k.b * (v.bU - v.bL) +
                     psi.b))) * du.b)
  #20*log10(abs(f.fluid))
  return(f.fluid)
}




########################
#' Run KRM model
#' Based on equations found in 'Acoustic models of fish: The Atlantic cod (Gadus morhua)' by Clay and Horne, 1994
#' @author Sven Gastauer
#' @description Run KRM model
#' @param frequency Frequency in Hz defaults to 120000
#' @param c.w ambient soundspeed in m/s defaults to 1490
#' @param rho.w Ambient water Density kg/m3 defaults to 1030
#' @param theta incident angle in degrees, note that 90 degrees equals broadside incident, KRM is only stable for values between 65 and 115 degrees, defaults 90 degrees
#' @param cs Soundspeeds in m/s for the different body parts, defautls to c(1570, 345), typical for fishbody and swimbladder
#' @param rhos Densities in kg/m3 for the different body parts, defaults to c(1070,345) , typical for fishbody and swimbladder
#' @param shape list of shapes that describe the body parts. must contain columns that start with x,w, z (z****L,z****U), describing the position along the x axis, the width, the upper and lower height.
#' @param modes which model mode should be used fluid or soft, fluid for weakly scattering nody parts and soft for stronger scattering parts, defaults to c("fluid", "soft"), typical for fish body and swimbladder
#' @param L Lengths to be computed in m, defaults to 0.21
#' @param fb Which body part is the largest, to which body part should the length be adjusted, defaults to 1 (first body part will be he largest)
#' @export
#' @examples
#' data(pil_fb)
#' data(pil_sb)
#' TS = krm(frequency=120000, c.w=1490, rho.w=1030, theta=90, cs=c(1570, 345), rhos=c(1070,1.24), shape=list(pil_fb, pil_sb), modes=c("fluid", "soft"), L=0.21, fb=1)
krm <- function(frequency =120 * 1000,
                  c.w = 1490,
                  rho.w = 1030,
                  theta=90,
                  cs = c(1570, 345),
                  rhos = c(1070,1.24),
                  shape=list(pil_fb, pil_sb),
                  modes=c("fluid", "soft"),
                  L=0.21,
                  fb=1){
    #####################################

    vars = expand.grid(frequency,c.w,rho.w,theta, L)
    names(vars) = c("frequency", "c.w", "rho.w", "theta", "L")

    # Model settings
    para = list(settings=list(frequency=vars$frequency,
                              c.w=vars$c.w,
                              rho.w=vars$rho.w,
                              theta=vars$theta,
                              L=vars$L))

    ####################################
    #If shape is list, rbind into dataframe
    if(class(shape)=="list"){
      nmax=max(sapply(shape, nrow)) #maximum rows

      for(x in 1:length(shape)){shape[[x]] = pad_df(shape[[x]],nmax)}
      shape = do.call("cbind", shape)
    }
    #######################################
    #Detect shapes in shape
    zL.ind = grep("z*L$", names(shape))
    zU.ind = grep("z*U$", names(shape))
    x.ind = grep("x", names(shape))
    w.ind = grep("w", names(shape))

    rot=0

    if(max(para$settings$theta) > 180){
      shape[,x.ind] = -shape[,x.ind]
      shape[,zL.ind] = -shape[,zL.ind]
      shape[,zU.ind] = -shape[,zU.ind]

      para$settings$theta = para$settings$theta - 180
      rot=1
    }

    nbp = length(zL.ind) #number of body parts
    message(Sys.time(),":", nbp," Body parts detected with ", length(rhos), " densities and ", length (cs)," internal soundspeeds")

    if(length(modes)< nbp){
      if(length(modes)>1){
        modes=c(modes, rep("fluid", nbp - length(modes)))
      }else{
        modes=c("fluid","soft", rep("fluid", nbp - 2))
      }
    }
    ##########################################
    #get fish body (the largest structure) for scaling
    x_fb = shape[,x.ind[fb]]
    w_fb = shape[,w.ind[fb]]
    zL_fb = shape[,zL.ind[fb]]
    zU_fb = shape[,zU.ind[fb]]

    if(is.null(L)){L = max(x_fb) - min(x_fb)}
    if(length(x_fb)>1){xv=x_fb}else{xv=x_sb}
    para$settings$scale = ifelse(para$settings$L != (max(x_fb) - min(x_fb)),para$settings$L / (max(x_fb) - min(x_fb)),1)

    ##############################################
    #Start modelling
    #loop through each shape
    sets=data.frame(para$settings)
    for(nn in 1:nbp){

      if(modes[nn] == "soft"){


        TS = data.frame(t(mapply(
          function(frequency, theta, c.w, rho.w, scale){
            krm.soft(frequency = frequency,
                     theta = theta,
                     c.w = c.w,
                     c.f = ifelse(nbp>1,cs[fb], c.w),
                     c.b = cs[nn],
                     rho.w =rho.w,
                     rho.f =ifelse(nbp>1,rhos[fb], rho.w),
                     rho.b= rhos[nn],
                     #x = na.omit(shp.sc2$x),
                     #w= na.omit(shp.sc2$w),
                     # z_U= na.omit(shp.sc2$zU))
                     x = as.numeric(unlist(na.omit(shape[x.ind[nn]]) * scale)),
                     w = as.numeric(unlist(na.omit(shape[w.ind[nn]]) * scale)),
                     z_U = as.numeric(unlist(na.omit(shape[zU.ind[nn]]) * scale)))
          },
          frequency=para$settings$frequency,
          theta=para$settings$theta,
          c.w=para$settings$c.w,
          rho.w=para$settings$rho.w,
          scale=para$settings$scale)))

      }else{
        TS = data.frame(t(mapply(
          function(frequency, theta, c.w, rho.w, scale){
            krm.fluid(frequency = frequency,
                      c.w = c.w,
                      c.b = cs[nn],
                      rho.w=rho.w,
                      rho.b=rhos[nn],
                      x = as.numeric(unlist(na.omit(shape[x.ind[nn]]) * scale)),
                      w = as.numeric(unlist(na.omit(shape[w.ind[nn]]) * scale)),
                      z_U = as.numeric(unlist(na.omit(shape[zU.ind[nn]]) * scale)),
                      z_L = as.numeric(unlist(na.omit(shape[zL.ind[nn]])  * scale)),
                      theta = theta)
            },
          frequency=para$settings$frequency,
          theta=para$settings$theta,
          c.w=para$settings$c.w,
          rho.w=para$settings$rho.w,
          scale=para$settings$scale)))

      }
      sets=cbind(sets, data.frame(t(TS)))
    }
    names(sets)[(ncol(sets)-nbp+1):ncol(sets)] = paste0("f", 1:nbp)
    row.names(sets)=1:nrow(sets)
    if(nbp>1){
      sets$sigma = abs(rowSums(sets[,paste0("f", 1:nbp)]))
    }else{sets$sigma=abs(sets$f1)}
    sets$TS <- 20*log10(sets$sigma)
    sets$L = sets$scale * max(x_fb) - min(x_fb)
    sets$theta = sets$theta + rot * 180
    return(sets)
  }
