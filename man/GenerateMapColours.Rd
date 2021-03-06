\name{GenerateMapColours}
\alias{GenerateMapColours}
\title{Generate color indices for shape object with the aim to color}
\usage{
  GenerateMapColours(sp)
}
\arguments{
  \item{sp}{SpatialPolygonsDataFrame object}
}
\value{
  Color index vector
}
\description{
  Generate color indices for shape object with the aim to
  color
}
\examples{
# col <- GenerateMapColours(sp)
}
\author{
  Modified from the code by Karl Ove Hufthammer from
  http://r-sig-geo.2731867.n2.nabble.com/Colouring-maps-so-that-adjacent-polygons-differ-in-colour-td6237661.html;
  modifications by Leo Lahti
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

