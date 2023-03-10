% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imgej2shp.R
\name{Imagej2shp}
\alias{Imagej2shp}
\title{ImageJ Extracts to KRM shape}
\usage{
Imagej2shp(
  shp,
  dorsal = c("Dorsal_body", "Dorsal_bladder"),
  lateral = c("Lateral_body", "Lateral_bladder"),
  body = 1,
  xy = c(1, 2),
  nam = 3,
  n = 0.01
)
}
\arguments{
\item{shp}{dataframe containing the shape information, with at least 3 colums, contianing x,y and descriptor of shape}

\item{dorsal}{names of shapes from dorsal aspect}

\item{lateral}{names of shapes from lateral aspect}

\item{body}{position of the largest body in the shape names, defaults 1}

\item{xy}{position of the xy coordinates in the dataframe defaults c(1,2)}

\item{nam}{position of name of the shape, defaults 3}
}
\description{
Plotly 3D plot of shape information
}
\author{
Sven Gastauer
}
