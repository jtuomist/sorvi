\name{PlotShape}
\alias{PlotShape}
\title{Visualize the specified fields of a shape object on using
1- or 2-way color scale.}
\usage{
  PlotShape(sp, varname, type = "oneway", ncol = 10,
    at = NULL, palette = NULL, main = NULL,
    colorkey = TRUE, lwd = 0.4, border.col = "black",
    col.regions = NULL, min.color = "white",
    max.color = "red")
}
\arguments{
  \item{sp}{Shape object}

  \item{varname}{Variable name from the shape object sp to
  be visualized}

  \item{type}{String. Specifies visualization type.
  Options: "oneway", "twoway", "qualitative", "custom". See
  details.}

  \item{ncol}{Number of distinct colors shades}

  \item{at}{Color transition points}

  \item{palette}{Optional. Color palette.}

  \item{main}{Optional. Title text.}

  \item{colorkey}{Logical. Show color interpretation in a
  separate legend.}

  \item{lwd}{Optional. Line width for shape polygon
  borders.}

  \item{border.col}{Optional. Color for shape polygon
  borders.}

  \item{col.regions}{Optional. Specify color for the shape
  object regions manually.}

  \item{min.color}{Color for minimum values in the color
  scale}

  \item{max.color}{Color for maximum values in the color
  scale}
}
\value{
  ggplot2 object
}
\description{
  This function is used for fast investigation of shape
  objects; standard visualization choices are made
  automatically; fast and easy-to-use but does not
  necessarily provide optimal visualization.
}
\details{
  Visualization types include: oneway/sequential (color
  scale ranges from white to dark red, or custom color
  given with the palette argument);
  twoway/bipolar/diverging (color scale ranges from dark
  blue through white to dark red; or custom colors);
  discrete/qualitative (discrete color scale; the colors
  are used to visually separate regions); and "custom"
  (specify colors with the col.regions argument)
}
\examples{
# PlotShape(sp, varname)
}
\author{
  Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\seealso{
  \code{\link{get.hsy}}
}
\keyword{utilities}

