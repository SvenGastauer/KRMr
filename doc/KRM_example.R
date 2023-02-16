## -----------------------------------------------------------------------------
library(dplyr)
library(KRMr)
stb_fb <- read.csv(paste0(dirname(getwd()),'/data/Sticklebacks_FB.csv'))
stb_sb <- read.csv(paste0(dirname(getwd()),'/data/Sticklebacks_SB.csv'))

## -----------------------------------------------------------------------------
library(plotly)
cyl <- function(r=c(1,2),r2=c(0.1,0.5),h=2,a=0, nt=10, nv=10){
  theta = seq(0,2*pi, length.out=nt)
  v = seq(a, a+h, length.out=nv)
  th = matrix(rep(theta,each=nv), ncol=nt)
  x = cos(th)*seq(r[1],r[2] , length.out=nv)
  y = sin(th)*seq(r2[1], r2[2] , length.out=nv)
  z = matrix(rep(v,times=nt), ncol=nt)
  return(list(x,y,z))
}

p = plot_ly()
fb0 = stb_fb[stb_fb$ind==58,]
sb0 = stb_sb[stb_sb$ind==58,]
for(i in seq(1, nrow(fb0)-1)){
  print(i)
  cc = cyl(r=c(fb0$w_fb[i], fb0$w_fb[i+1]), 
         a = fb0$x_fb[i],#abs(x_fb[i+1] - x_fb[i]), 
         h=fb0$x_fb[i+1]-fb0$x_fb[i],
         r2=c(fb0$z_fbU[i], fb0$z_fbU[i+1]))#x_fb[i])
  p = p%>%add_trace(x=cc[[3]], y=cc[[1]], z=cc[[2]], 
                    type='surface', 
                    showscale = FALSE, 
                    colorscale=list(c(0, 1), c("red", "red")),
                    opacity=0.5)
  
}
for(i in seq(1, nrow(sb0)-1)){
  print(i)
  cc = cyl(r=c(sb0$w_sb[i], sb0$w_sb[i+1]), 
         a = sb0$x_sb[i],#abs(x_fb[i+1] - x_fb[i]), 
         h=sb0$x_sb[i+1]-sb0$x_sb[i],
         r2=c(sb0$z_sbU[i], sb0$z_sbU[i+1]))#x_fb[i])
  p = p%>%add_trace(x=cc[[3]], y=cc[[1]], z=cc[[2]], 
                    type='surface', 
                    showscale = FALSE, 
                    colorscale=list(c(0, 1), c("blue", "blue")),
                    opacity=0.5)
  
}

p%>%layout(scene=list(aspectmode='data'))

## -----------------------------------------------------------------------------
for (i in unique(stb_fb$ind)){
  fb =stb_fb%>%filter(ind==i)
  sb =stb_sb%>%filter(ind==i)
  #layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  par(mfrow=c(1,2))
  KRMr::shplot(x_fb = fb$x_fb, w_fb = fb$w_fb, 
             x_sb = sb$x_sb, w_sb = sb$w_sb,
             z_fbU = fb$z_fbU, z_fbL = fb$z_fbL,
             z_sbU = sb$z_sbU, z_sbL = sb$z_sbL)
}

## -----------------------------------------------------------------------------
id = unique(stb_fb$ind)[1]
fb =stb_fb%>%filter(ind==i)
sb =stb_sb%>%filter(ind==i)

## -----------------------------------------------------------------------------
TS = KRMr::krm.sim(frequency =c(38,70,120,200) * 1000,
                   c.w = 1490,
                   rho.w = 1030,
                   theta=90,
                   c.fb = 1570,
                   c.sb = 345,
                   rho.sb = 1.24,
                   rho.fb = 1070,
                   L=0.25,
                   x_fb = fb$x_fb,
                   x_sb = sb$x_sb,
                   w_fb = fb$w_fb,
                   w_sb = sb$w_sb,
                   z_fbU = fb$z_fbU,
                   z_fbL = fb$z_fbL,
                   z_sbU = sb$z_sbU,
                   z_sbL = sb$z_sbL)

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(data=TS, 
       aes(x=frequency/1000, y=TS))+
  geom_line(size=1.2)+
  ylab(expression(TS~(~dB~re~m^2)))+
  xlab('Frequency (kHz)')+
  theme_classic()+
  theme(text=element_text(size=14),
        legend.position='top')


## -----------------------------------------------------------------------------
TS = krm.sim(frequency =c(38,70,120,200) * 1000,
                   c.w = 1490,
                   rho.w = 1030,
                   theta=seq(65,115),
                   c.fb = 1570,
                   c.sb = 345,
                   rho.sb = 1.24,
                   rho.fb = 1070,
                   L=seq(0.01,0.1,by=0.01),
                   x_fb = fb$x_fb,
                   x_sb = sb$x_sb,
                   w_fb = fb$w_fb,
                   w_sb =  sb$w_sb,
                   z_fbU = fb$z_fbU,
                   z_fbL = fb$z_fbL,
                   z_sbU = sb$z_sbU,
                   z_sbL = sb$z_sbL)

## -----------------------------------------------------------------------------
ggplot2::ggplot(data=TS, ggplot2::aes(x= theta, y=L*1000 , z=TS))+
  facet_wrap(.~frequency/1000)+
  geom_contour_filled(bins=15)+
  ggplot2::scale_fill_viridis_d(expression(TS~'('~dB~re~ m^2~')'))+
  ggplot2::xlab('Theta (degrees)')+
  ggplot2::ylab(expression(L~'(mm)'))+
  ggplot2::scale_x_continuous(expand=c(0,0))+
  ggplot2::scale_y_continuous(expand=c(0,0))+
  ggplot2::theme_classic()+
  ggplot2::theme(legend.position='top',
                 legend.text=element_text(size=10),
                 text=element_text(size=16),
                 strip.background = element_blank(),
                 strip.text = element_text(face='bold'))


## -----------------------------------------------------------------------------
TS = krm.sim(frequency =c(38,70,120,200) * 1000,
                   c.w = 1490,
                   rho.w = 1030,
                   theta=seq(65,115),
                   c.fb = 1570,
                   c.sb = 345,
                   rho.sb = 1.24,
                   rho.fb = 1070,
                   L=seq(0.01,0.1,by=0.01),
                   x_fb = fb$x_fb,
                   x_sb = NULL,
                   w_fb = fb$w_fb,
                   w_sb =  NULL,
                   z_fbU = fb$z_fbU,
                   z_fbL = fb$z_fbL,
                   z_sbU = NULL,
                   z_sbL = NULL)

## -----------------------------------------------------------------------------
ggplot2::ggplot(data=TS, ggplot2::aes(x= theta, y=L*1000 , z=TS))+
  facet_wrap(.~frequency/1000)+
  geom_contour_filled(bins=15)+
  ggplot2::scale_fill_viridis_d(expression(TS~'('~dB~re~ m^2~')'))+
  ggplot2::xlab('Theta (degrees)')+
  ggplot2::ylab(expression(L~'(mm)'))+
  ggplot2::scale_x_continuous(expand=c(0,0))+
  ggplot2::scale_y_continuous(expand=c(0,0))+
  ggplot2::theme_classic()+
  ggplot2::theme(legend.position='top',
                 legend.text=element_text(size=10),
                 text=element_text(size=16),
                 strip.background = element_blank(),
                 strip.text = element_text(face='bold'))


## ----fig.width = 8, fig.height = 8--------------------------------------------
TSrot = krm.sim(frequency =c(38,200) * 1000,
                   c.w = 1490,
                   rho.w = 1030,
                   theta=65:115,
                   c.fb = 1570,
                   c.sb = 345,
                   rho.sb = 1.24,
                   rho.fb = 1070,
                   L=0.08,
                   x_fb = fb$x_fb,
                   x_sb = NA,#sb$x_sb,
                   w_fb = fb$w_fb,
                   w_sb = NA,#sb$w_sb,
                   z_fbU = fb$z_fbU,
                   z_fbL = fb$z_fbL,
                   z_sbU = NA,#sb$z_sbU,
                   z_sbL = NA)#sb$z_sbL)
ggplot(TSrot, aes(x = theta, y = TS, group=frequency/1000, col=TS)) +
  geom_path(size=1.2) +
  facet_wrap(.~frequency/1000)+
  scale_x_continuous(limits=c(0,360), breaks=c(0,90,180))+
  coord_polar(start=-pi/2,direction=1)+
  scale_colour_viridis_c(name='', limits=c(-80,-70), oob=scales::squish)+

  ylab(expression(TS~'('~dB~re~m^-2~')' ))+
  xlab(expression(paste(theta,' (\u00B0) ')))+
  
  geom_vline(xintercept=90, lty=2)+
  geom_vline(xintercept=0,size=1)+
  geom_vline(xintercept=180,size=1)+
  
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text=element_text(size=18),
        legend.position='top',
        legend.text=element_text(angle=-15),
        legend.key.width = unit(2,'cm'),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        panel.spacing.y = unit(-5,'lines'),
        axis.title.x = element_text(vjust=27, size=16),
        axis.title.y = element_text(hjust=0.75, size=16),
        text=element_text(size=18))



