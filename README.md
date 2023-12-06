# KRMr
Kichhoff-Ray Mode Model for fish in R  

<a href="https://www.thuenen.de/de/fachinstitute/seefischerei"><img src=https://github.com/SvenGastauer/KRMr/blob/main/thuenen_logo.png width="200"> </a>    
Sven Gastauer, February 2023, [Thünen Institute for Sea Fisheries](https://www.thuenen.de/de/fachinstitute/seefischerei)  
[![DOI](https://zenodo.org/badge/602591946.svg)](https://zenodo.org/badge/latestdoi/602591946)


## Description

Based on the model description found in Clay, C. S., & Horne, J. K. (1994). 

Notes on model validity:  The KRM model is only valid for incident angles between 65° and 115° (i.e. +/- 25° from incidence).  

Current features of the package:
- Model fish with swimbladders / internal features
- Select soft or fluid model for internal features
- shiny app for easy feature extraction from X Rays or Pictures and translation into KRM shape files (experimental)
- Translate xyz (for example as extracted from ImageJ) into KRM shape files  

To be done:
- Document shiny app

| Model       | Accuracy / Type           | Range of Validity  | Limitations  | Examples  |
|---|---|---|---|---|
| KRM | Approximate| All frequencies, homogenous material; at high frequencies: high aspect ratios; at low frequencies: near-normal incidence |  Off-normal incidence, no circumferential waves, no longitudinal modes of vibration near resonance| Clay and Horne (1994), Horne et al. (2000); Macaulay et al. (2013); Gastauer et al. (2016)|

*References*
- *Clay, C. S., & Horne, J. K. (1994). Acoustic models of fish: the Atlantic cod (Gadus morhua). The Journal of the Acoustical Society of America, 96(3), 1661-1668*
- *Gastauer, S., Scoulding, B., Fässler, S. M., Benden, D. P., & Parsons, M. (2016). Target strength estimates of red emperor (Lutjanus sebae) with Bayesian parameter calibration. Aquatic Living Resources, 29(3), 301.*
- *Horne, J. K., Walline, P. D., & Jech, J. M. (2000). Comparing acoustic model predictions to in situ backscatter measurements of fish with dual‐chambered swimbladders. Journal of fish Biology, 57(5), 1105-1121.*  
- *Macaulay, G. J., Peña, H., Fässler, S. M., Pedersen, G., & Ona, E. (2013). Accuracy of the Kirchhoff-approximation and Kirchhoff-ray-mode fish swimbladder acoustic scattering models. PloS one, 8(5), e64055.*

## Licence
```
MIT License

Copyright (c) 2023 Sven Gastauer

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Package Installation  
Install package with vignettes:
```
devtools::install_github("SvenGastauer/KRMr", build_vignettes = TRUE)
```

Install package without vignettes:  
```
devtools::install_github("SvenGastauer/KRMr")
```

## Package dependencies  
- [zoo](https://cran.r-project.org/package=zoo)
- [ggplot2](https://cran.r-project.org/package=ggplot2)
- [shiny](https://cran.r-project.org/package=shiny)
- [plotly](https://cran.r-project.org/package=plotly)
- [shinythemes](https://cran.r-project.org/web/packages/shinythemes/shinythemes.pdf)
- [DT](https://cran.r-project.org/package=DT)
- [tidyverse](https://cran.r-project.org/package=tidyverse)
- [magick](https://cran.r-project.org/package=magick)
- [grid](https://cran.r-project.org/package=grid)
- [tibble](https://cran.r-project.org/package=tibble)
- [sf](https://cran.r-project.org/package=sf)

## Help files / vignettes

To get a list of all available vignettes:
```
vignette(package="KRMr")
```
It is recommended to view vignettes in your preferred browser:
```
browseVignettes("KRMr")
```

## Citation

Please notify the author if you are using this package in any presentation or publication, so we can keep an up to date list of references for the package.
Please cite this package as:

*Gastauer S (2023). _KRMr: Kirchhoff Ray Mode Model for fisheries acoustics_. R package version 0.4.7.*
  
  Bibtex:
  ```
    @Manual{,
    title = {KRMr: Kirchhoff Ray Mode Model for fisheries acoustics},
    author = {Sven Gastauer},
    year = {2023},
    note = {R package version 0.4.6},
    DOI =  {10.5281/zenodo.7795866}
  }
  ```
View citation in R:

```citation("KRMr")```

## Getting help
For KRMr specific questions make a feature request or post an issue on [GitHub](https://github.com/SvenGastauer/KRMr/issues).    
For general R questions visit [Stack Overflow](https://stackoverflow.com/questions/tagged/r).  
If none of those options seem appropriate and you are getting really desperate, contact one the authors.

## Example

### Fluid sphere  

Defining the the sphere parameters:  
```
freqs = 10:400 * 1000 #Hz frequencies to be considered in Hz
a = 0.01 #m radius of the sphere
cw = 1477.4 #m/s sound speed surrounding water
cfb =  1480.3 #m/s soundspeed inside the sphere
rhow = 1026.8 #kg/m3 density surrounding water
rhofb = 1028.9 #kg/m3 density fluid inside sphere
```

Construct the shape information of the sphere:  
```
ang = seq(-90,90,length.out=30) #angles to consider
shape=data.frame(
  xfb = sin(ang * pi / 180), #x coordinates of a sphere
  wfb = (2*cos(ang * pi / 180)), #diameter of the sphere
  zfbU = cos(ang * pi / 180), #upper z coordinates
  zfbL = -cos(ang * pi / 180)) #lower z coordiates

KRMr::shplot(x_fb = shape$xfb,w_fb = shape$wfb,z_fbU = shape$zfbU, z_fbL=shape$zfbL)
```

Plot in 3D:
```
KRMr::get_shp3d(shape)
```
Now we can run the KRM simulation:  

```
kfs = KRMr::krm(frequency =freqs,
              c.w = cw,
              rho.w = rhow,
              theta=90,
              cs = cfb,
              rhos = rhofb,
              modes="fluid",
              L=0.02,
              shape=shape)
```

### Sardine  

First we define the shape of a sardine like fish. Here we take the sardine shape profile from the [NOAA SWFSC website](https://swfscdata.nmfs.noaa.gov/AST/KRM/krm.html).  

```
#Sardine
fb = KRMr::pil_fb #fish body sardine
sb = KRMr::pil_sb #swimbladder sardine
```

Now we can have a look at the shape:  
```
KRMr::shplot(x_fb = fb$x_fb, w_fb = fb$w_fb, x_sb = sb$x_sb, w_sb = sb$w_sb,z_fbU = fb$z_fbU,z_fbL = fb$z_fbL,z_sbU = sb$z_sbU,z_sbL = sb$z_sbL)
```

```
KRMr::get_shp3d(list(fb, sb))
```
Because we have defined the fishbody coordinates and the swimbladder coordinates in a dataframe, the most simple way to run one KRM simulation in KRMr is:

```
KRMr::krm(shape = list(fb, sb))
```


Now we are ready to run a KRM simulation. Let's have a look at the TS for a 21 cm sardine from 120 to 200 kHz. Here we will define the different coordinates separately and add the parameters we want to simulate:  

```
TS = KRMr::krm(frequency =seq(20,200.1) * 1000,
         c.w = 1490,
         rho.w = 1030,
         theta=90,
         cs = c(1570, 345),
         rhos = c(1070,1.24),
         L=0.21,
         shape=list(fb,sb))
```


Plot the results:

```
plot(TS$frequency/1000, 
     TS$TS, 
     type='l',
     xlab='Frequency (kHz)', 
     ylab=expression(TS~'('~dB~re~ m^-2~')'))
```

We can also run a simulation over multiple parameters, for example frequencies from 120-200 kHz and incident angles from 65-115 degrees:  

```
TS = KRMr::krm(frequency =seq(120,200.1) * 1000,
         c.w = 1490,
         rho.w = 1030,
         theta=65:115,
         cs = c(1570, 345),
         rhos = c(1070,1.24),
         L=0.21,
         shape=list(fb,sb))
```


We can for example use ggplot2 to plot a 2D map of the results:

```
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
```


### Orientation
![KRM Orientatio](https://github.com/SvenGastauer/KRMr/blob/main/inst/extdata/orientation.png)  
90° corresponds to braodside incidence. Remember that the model is only valid for angles between 65 and 115° (+/- 25° from broadside incidence).  

## Publications using KRMr


- Yang Yang, Sven Gastauer, Roland Proud, Richard Mangeni-Sande, Inigo Everson, Robert J Kayanda, Andrew S Brierley, Modelling and in situ observation of broadband acoustic scattering from the Silver cyprinid (Rastrineobola argentea) in Lake Victoria, East Africa, ICES Journal of Marine Science, 2023;, fsad137, [https://doi.org/10.1093/icesjms/fsad137](https://doi.org/10.1093/icesjms/fsad137)
- Palermino, A., De Felice, A., Canduci, G. et al. Application of an analytical approach to characterize the target strength of ancillary pelagic fish species. Sci Rep 13, 15182 (2023). [https://doi.org/10.1038/s41598-023-42326-4](https://doi.org/10.1038/s41598-023-42326-4)
