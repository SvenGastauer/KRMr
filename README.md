# KRMr
Kichhoff-Ray Mode Model for fish in R

Sven Gastauer, Febraury 2023, [Thünen Institute for Sea Fisheries](https://www.thuenen.de/de/fachinstitute/seefischerei)

## Description

Based on the model description found in Clay, C. S., & Horne, J. K. (1994). 

Notes on model validity:  The KRM model is only valid for incident angles between 65° and 115° (i.e. +/- 25° from incidence).

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

*Gastauer S (2023). _KRMr: Kirchhoff Ray Mode Model for fisheries acoustics_. R package version 0.3.0.*
  
  Bibtex:
  ```
    @Manual{,
    title = {KRMr: Kirchhoff Ray Mode Model for fisheries acoustics},
    author = {Sven Gastauer},
    year = {2023},
    note = {R package version 0.3.0},
  }
  ```
View citation in R:

```citation("KRMr")```

## Getting help
For KRMr specific questions make a feature request or post an issue on [GitHub](https://github.com/SvenGastauer/KRMr/issues).    
For general R questions visit [Stack Overflow](https://stackoverflow.com/questions/tagged/r).  
If none of those options seem appropriate and you are getting really desperate, contact one the authors.

## Publications using KRMr

TBA
