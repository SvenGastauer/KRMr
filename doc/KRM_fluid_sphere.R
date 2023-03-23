## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
library(KRMr)

## ----wfs_parameters-----------------------------------------------------------
#library(KRMr)
#Define the the sphere parameters
freqs = 10:400 * 1000 #Hz frequencies to be considered in Hz
a = 0.01 #m radius of the sphere
cw = 1477.4 #m/s sound speed surrounding water
cfb =  1480.3 #m/s soundspeed inside the sphere
rhow = 1026.8 #kg/m3 density surrounding water
rhofb = 1028.9 #kg/m3 density fluid inside sphere


## ----Generate_Sphere_shape, fig.width=7,fig.height=6--------------------------
#generate sphere coordinates
ang = seq(-90,90,length.out=30) #angles to consider
shape=data.frame(
  xfb = sin(ang * pi / 180), #x coordinates of a sphere
  wfb = (2*cos(ang * pi / 180)), #diameter of the sphere
  zfbU = cos(ang * pi / 180), #upper z coordinates
  zfbL = -cos(ang * pi / 180)) #lower z coordiates

shplot(x_fb = shape$xfb,w_fb = shape$wfb,z_fbU = shape$zfbU, z_fbL=shape$zfbL)


## -----------------------------------------------------------------------------
get_shp3d(shape)

## ----KRM_simulation_wfs-------------------------------------------------------
kfs = krm(frequency =freqs,
              c.w = cw,
              rho.w = rhow,
              theta=90,
              cs = cfb,
              rhos = rhofb,
              modes="fluid",
              L=0.02,
              shape=shape)

## ----Analytical_Sphere_wfs----------------------------------------------------
kfs$bench=sapply(freqs,
                 function(f){
                   fsphere(f,a,cw,cfb/cw,
                             rhofb/rhow)})

## ----wfs_results, fig.width=7,fig.height=6------------------------------------
colors <- c("Analytical" = "red", "KRM" = "blue")

ggplot2::ggplot()+
  ggplot2::geom_line(data=kfs, ggplot2::aes(x=frequency/1000,y=TS, group=L,col='KRM'),lwd=1)+
  ggplot2::geom_line(data=kfs, ggplot2::aes(x=frequency/1000, group=L, y=bench,col='Analytical'), lty=2,lwd=1)+
  ggplot2::scale_color_manual(values = colors, name='')+
  ggplot2::xlab('Frequency (kHz)')+
  ggplot2::ylab('Target Strength (dB re m-2)')+
  ggplot2::theme_classic()+
  ggplot2::theme(text=ggplot2::element_text(size=16),
        legend.position='top')


## ----gfs_parameters-----------------------------------------------------------

#Define the the sphere parameters
freqs = seq(12,400,1) * 1000 #Hz frequencies to be considered in Hz
a = 0.01 #m radius of the sphere
cw = 1477.4 #m/s sound speed surrounding water
cfb =  345.0 #m/s soundspeed inside the sphere
rhow = 1026.8 #kg/m3 density surrounding water
rhofb = 1.24 #kg/m3 density fluid inside sphere


## ----KRM_simulation_gfs-------------------------------------------------------
kfs = krm(frequency =freqs,
              c.w = cw,
              rho.w = rhow,
              theta=90,
              cs = cfb,
              rhos = rhofb,
              L=0.02,
              modes="soft",
              shape=shape)

## ----Analytical_Sphere_gfs----------------------------------------------------
kfs$bench=sapply(freqs,
                 function(f){
                   fsphere(f,a,cw,cfb/cw,
                             rhofb/rhow)})

## ----gfs_results, fig.width=7,fig.height=6------------------------------------
colors <- c("Analytical" = "red", "KRM" = "blue")

ggplot2::ggplot()+
  ggplot2::geom_line(data=kfs, ggplot2::aes(x=frequency/1000,y=TS, group=L,col='KRM'),lwd=1)+
  ggplot2::geom_line(data=kfs, ggplot2::aes(x=frequency/1000, group=L, y=bench,col='Analytical'), lty=2,lwd=1)+
  ggplot2::scale_color_manual(values = colors, name='')+
  scale_y_continuous(limits=c(-50,-40))+
  ggplot2::xlab('Frequency (kHz)')+
  ggplot2::ylab('Target Strength (dB re m-2)')+
  ggplot2::theme_classic()+
  ggplot2::theme(text=ggplot2::element_text(size=16),
        legend.position='top')


## -----------------------------------------------------------------------------
#Sardine
data(pil_fb) #fish body sardine
fb=pil_fb
data(pil_sb) #swimbladder sardine
sb=pil_sb

## ----fig.width=7,fig.height=3-------------------------------------------------
shplot(x_fb = fb$x_fb, w_fb = fb$w_fb, x_sb = sb$x_sb, w_sb = sb$w_sb,z_fbU = fb$z_fbU,z_fbL = fb$z_fbL,z_sbU = sb$z_sbU,z_sbL = sb$z_sbL)

## ----fig.width=7,fig.height=4-------------------------------------------------
get_shp3d(list(pil_fb, pil_sb))

## -----------------------------------------------------------------------------
krm(shape = list(fb, sb))

## -----------------------------------------------------------------------------
TS = krm(frequency =seq(20,200.1) * 1000,
         c.w = 1490,
         rho.w = 1030,
         theta=90,
         cs = c(1570, 345),
         rhos = c(1070,1.24),
         L=0.21,
         shape=list(fb,sb))

## ----fig.width=7,fig.height=3-------------------------------------------------
plot(TS$frequency/1000, 
     TS$TS, 
     type='l',
     xlab='Frequency (kHz)', 
     ylab=expression(TS~'('~dB~re~ m^-2~')'))

## -----------------------------------------------------------------------------
TS = krm(frequency =seq(120,200.1) * 1000,
         c.w = 1490,
         rho.w = 1030,
         theta=65:115,
         cs = c(1570, 345),
         rhos = c(1070,1.24),
         L=0.21,
         shape=list(fb,sb))

## ---- fig.width=7,fig.height=6------------------------------------------------
ggplot2::ggplot(data=TS, ggplot2::aes(x= frequency/1000, y= theta, fill=TS))+
  ggplot2::geom_tile()+
  ggplot2::scale_fill_viridis_c(expression(TS~'('~dB~re~ m^-1~')'))+
  ggplot2::xlab('Frequency (kHz)')+
  ggplot2::ylab(expression(theta~'(°)'))+
  ggplot2::scale_x_continuous(expand=c(0,0))+
  ggplot2::scale_y_continuous(expand=c(0,0))+
  ggplot2::theme_classic()+
  ggplot2::theme(legend.position='top',
        legend.text=element_text(angle=45),
        text=element_text(size=16))

