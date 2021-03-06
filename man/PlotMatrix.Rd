\name{PlotMatrix}
\alias{PlotMatrix}
\title{Visualize a matrix with one or two-way color scale.
TODO: one-way color scale}
\usage{
  PlotMatrix(mat, type = "twoway", midpoint = 0,
    palette = NULL, colors = NULL, col.breaks = NULL,
    interval = 0.1, plot.axes = "both", row.tick = 1,
    col.tick = 1, cex.xlab = 0.9, cex.ylab = 0.9,
    xlab = NULL, ylab = NULL, limit.trunc = 0,
    mar = c(5, 4, 4, 2), ...)
}
\arguments{
  \item{mat}{matrix}

  \item{type}{String. Specifies visualization type.
  Options: "oneway" (color scale ranges from white to dark
  red; the color can be changed if needed); "twoway" (color
  scale ranges from dark blue through white to dark red;
  colors can be changed if needed)}

  \item{midpoint}{middle point for the color plot: smaller
  values are shown with blue, larger are shown with red in
  type = "twoway"}

  \item{palette}{Optional. Color palette.}

  \item{colors}{Optional. Colors.}

  \item{col.breaks}{breakpoints for the color palette}

  \item{interval}{interval for palette color switches}

  \item{plot.axes}{String. Indicates whether to plot x-axis
  ("x"), y-axis ("y"), or both ("both").}

  \item{row.tick}{interval for plotting row axis texts}

  \item{col.tick}{interval for plotting column axis texts}

  \item{cex.xlab}{use this to specify distinct font size
  for the x axis}

  \item{cex.ylab}{use this to specify distinct font size
  for the y axis}

  \item{xlab}{optional x axis labels}

  \item{ylab}{optional y axis labels}

  \item{limit.trunc}{color scale limit breakpoint}

  \item{mar}{image margins}

  \item{...}{optional parameters to be passed to function
  'image', see help(image) for further details}
}
\value{
  A list with the color palette (colors), color breakpoints
  (breaks), and palette function (palette.function)
}
\description{
  This function is used for fast investigation of matrix
  objects; standard visualization choices are made
  automatically; fast and easy-to-use but does not
  necessarily provide optimal visualization.
}
\examples{
# mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); PlotMatrix(mat, "twoway", midpoint = 3)
}
\author{
  Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

