## ----pot_sardine_shp, warning=FALSE, fig.width=8------------------------------
library(KRMr)
library(knitr)

data(pil_sb)
data(pil_fb)
par(mfrow=c(1,2))
KRMr::shplot(x_fb = pil_fb$x_fb, w_fb = pil_fb$w_fb,
            z_fbU = pil_fb$z_fbU, z_fbL = pil_fb$z_fbL,
            x_sb = pil_sb$x_sb, w_sb = pil_sb$w_sb,
            z_sbU = pil_sb$z_sbU, z_sbL = pil_sb$z_sbL)

## ----krm_sardine example------------------------------------------------------
krm(
  frequency = 120 * 1000,
  c.w = 1490,
  rho.w = 1030,
  theta = 90,
  cs = c(1570, 345),
  rhos = c(1070, 1.24),
  shape = list(pil_fb, pil_sb),
  modes = c("fluid", "soft"),
  L = 0.21,
  fb = 1
)

## ----read_imj, eval=FALSE-----------------------------------------------------
#  library(RImageJROI)
#  fn = './HER.zip'
#  side = RImageJROI::read.ijzip(fn) #fn should be the filename and path to the ROI.zip file
#  shapes = lapply(side, FUN=function(x){
#    tmp=data.frame(x$coords)
#    tmp$Part=x$name
#    return(tmp)})

## ----pollock, warning=F, fig.width=12, , message=FALSE------------------------
shp1 = KRMr::Imagej2shp(shp=read.csv("./Pollock01.csv"))
KRMr::get_shp3d(shp1)


## ----pollock.krm, message=FALSE-----------------------------------------------

frequency=c(18,38,70,120,200)*1e3 #frequencies Hz
theta = seq(65,90,115) #orientations

c.w = 1490 #Ambient water m/s
rho.w = 1026.8 #density ambient water kg/m3

c.fb=1570 #soundspeed body 
c.b=345 #soundspeed swimbladder
cs = c(c.fb, c.b)

rho.fb=1070 #density body kg/m3
rho.b=1.24 #density swimbladder kg/m3
rhos = c(rho.fb, rho.b)

L = 29.7/1e2 #Lengths for pollocks in m

krm1 = krm(frequency = frequency,
      c.w = c.w,
      rho.w = rho.w,
      theta = theta,
      cs =cs,
      rhos = rhos,
      shape = shp1,
      modes = c("fluid","soft"),
      L = L[1],
      fb = 1)
knitr::kable(krm1)


## ----HERRING_rd, message=FALSE------------------------------------------------
fn = "HER.zip" # Path to the ImageJ ROI.zip file
top = read.table("./HER_body_top.txt") # Read the only avaialble top view file

library(RImageJROI)
side = RImageJROI::read.ijzip(fn)
shapes = lapply(side, FUN=function(x){ #load all side view coordinates into a list
  tmp=data.frame(x$coords)
  tmp$Part=x$name
  return(tmp)})

## ----HERRING_sav, fig.width=8, message=FALSE----------------------------------
 par(mfrow=c(3,4), mar=c(1,1,1,1)) #have a quick look at the available shapes
 for(i in seq(1,length(side), by=2)){
   plot(shapes[[i]][, 1],-shapes[[i]][, 2], asp=1, type="l")
   points(shapes[[i+1]][, 1],-shapes[[i+1]][, 2], asp=1, type="l")
 }

## ----HERRING_add.width, message=FALSE-----------------------------------------
####Add widths

names(top)=c("x", "y")
top$Part = "Body Top"
topr=data.frame(x=-top[,1], y=top[,2])
topr$Part = "Body Top"
shr = function(x){x$Part = "SwB Top";return(x)}
tops = list(top,shr(shapes[[2]]),  top,shr(shapes[[4]]),  top,shr(shapes[[6]]),  topr,shr(shapes[[8]]),
     topr,shr(shapes[[10]]),topr,shr(shapes[[12]]),topr,shr(shapes[[14]]),top,shr(shapes[[16]]),
     top,shr(shapes[[18]]), top,shr(shapes[[20]]), topr,shr(shapes[[22]]))


## ----HERRING_scale, fig.width=8, warning=FALSE, message=FALSE-----------------
#scale top
for(i in seq(1,length(side), by=2)){
  k=(i+1)/2
  message(Sys.time(), ": Processing shape ", k)
  xmin0 = min(shapes[[i]]$x); xmax0 = max(shapes[[i]]$x)
  dx0 = xmax0-xmin0
  ymin0 = min(shapes[[i]]$y)
  shapes[[i]]$x=(shapes[[i]]$x-xmin0)/dx0
  shapes[[i]]$y=(shapes[[i]]$y-ymin0)/dx0
  shapes[[i+1]]$x = (tops[[i+1]]$x - xmin0) / dx0
  shapes[[i+1]]$y = (tops[[i+1]]$y - ymin0) / dx0

  tmin=min(tops[[i]]$x); tmax=max(tops[[i]]$x)
  ymin = min(tops[[i]]$y)
  dx= tmax-tmin
  ty=min(tops[[i+1]]$x); tmax=max(tops[[i+1]]$x)
  tops[[i]]$x = (tops[[i]]$x - tmin) / dx
  tops[[i]]$y = (tops[[i]]$y - ymin) / dx

  tops[[i+1]]$x = (tops[[i+1]]$x - xmin0) / dx0
  tops[[i+1]]$y = (tops[[i+1]]$y - ymin0) / dx0

  shp = rbind(shapes[[i]],shapes[[i+1]],tops[[i]], tops[[i+1]])

  bi=sprintf("%02d", k)
  #write.csv(shp,paste0("HER_",bi,"_shp.csv"))

  shape=Imagej2shp(shp=shp,dorsal = c("Body Top","SwB Top") ,
                   lateral = c(unique(shp$Part)[1],unique(shp$Part)[2]),
                   body=1, xy=c(1,2), nam=3, n=0.01)
    saveRDS(shape,paste0("HER_",bi,"_shape.rds"))
}

## ----HERRING_sh3d, fig.height=12, fig.width=8, message=FALSE , warning=FALSE----

### Generate 3D plots

library(plotly)
library(dplyr)

p3 = lapply(1:11,FUN=function(x) {
  message(paste0("scene",x))
  get_shp3d(readRDS(paste0("HER_",sprintf("%02d", x),"_shape.rds")), scene=paste0("scene",x))
  })
fig <- subplot(p3)%>% layout(scene=list(domain=list(x=c(0,0.4),y=c(0.75,1)),
                                        aspectmode='data'))

fig <- fig %>% layout(title = "Herring Shapes",
                      scene1 = list(domain=list(x=c(0,0.4),y=c(0.75,1)),
                                   aspectmode='data'),
                      scene2 = list(domain=list(x=c(0.3,0.7),y=c(0.75,1)),
                                    aspectmode='data'),
                      scene3 = list(domain=list(x=c(0.6,1),y=c(0.75,1)),
                                    aspectmode='data'),
                      scene4 = list(domain=list(x=c(0,0.4),y=c(0.5,.75)),
                                    aspectmode='data'),
                      scene5 = list(domain=list(x=c(0.3,0.7),y=c(0.5,0.75)),
                                    aspectmode='data'),
                      scene6 = list(domain=list(x=c(0.6,1),y=c(0.5,0.75)),
                                    aspectmode='data'),
                      scene7 = list(domain=list(x=c(0,0.4),y=c(0.25,0.5)),
                                    aspectmode='data'),
                      scene8 = list(domain=list(x=c(0.3,0.7),y=c(0.25,0.5)),
                                    aspectmode='data'),
                      scene9 = list(domain=list(x=c(0.6,1),y=c(0.25,0.5)),
                                    aspectmode='data'),
                      scene10 = list(domain=list(x=c(0.3,0.7),y=c(0.,0.25)),
                                    aspectmode='data'),
                      scene11 = list(domain=list(x=c(0.6,1),y=c(0.,0.25)),
                                    aspectmode='data'))

fig

## ----her.sim------------------------------------------------------------------
set.seed(78965);L=rnorm(1,14,2.3) #random lengths drawn from a normal distribution with a mean of 14 cm and an sd of 2.3 cm

ts.all=data.frame()
for(i in 1:11){
  message(Sys.time(),": Processing Shape ", i)
  ts=krm(frequency = c(38)*1e3,
      c.w = 1490,
      rho.w = 1030,
      theta = 90,
      cs = c(1570, 345),
      rhos = c(1070, 1.24),
      shape = readRDS(paste0("HER_",sprintf("%02d", i),"_shape.rds")),
      modes = c("fluid", "soft"),
      L = L/100,
      fb = 1)
  ts$shape=i
  ts.all=rbind(ts.all,ts)
}
kable(ts.all)

