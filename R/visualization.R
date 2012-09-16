# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.



#' Description: Draw regression curve with smoothed error bars 
#' based on the Visually-Weighted Regression by Solomon M. Hsiang; see
#' http://www.fight-entropy.com/2012/07/visually-weighted-regression.html
#' The R implementation is based on Felix Schönbrodt's code from 
#' http://www.nicebread.de/visually-weighted-watercolor-plots-new-variants-please-vote/
#'
#' Arguments:
#'   @param formula formula
#'   @param data data
#'   @param title title
#'   @param B number bootstrapped smoothers
#'   @param shade plot the shaded confidence region?
#'   @param shade.alpha shade.alpha: should the CI shading fade out at the edges? (by reducing alpha; 0 = no alpha decrease, 0.1 = medium alpha decrease, 0.5 = strong alpha decrease)
#'   @param spag: plot spaghetti lines?
#'   @param mweight should the median smoother be visually weighted?
#'   @param show.lm should the linear regresison line be plotted?
#'   @param show.median show median smoother
#'   @param median.col median color
#'   @param show.CI: should the 95% CI limits be plotted?
#'   @param method the fitting function for the spaghettis; default: loess
#'   @param bw define a default b/w-palette (TRUE/FALSE)
#'   @param slices number of slices in x and y direction for the shaded region. Higher numbers make a smoother plot, but takes longer to draw. I wouldn'T go beyond 500
#'   @param palette provide a custom color palette for the watercolors
#'   @param ylim restrict range of the watercoloring
#'   @param quantize either "continuous", or "SD". In the latter case, we get three color regions for 1, 2, and 3 SD (an idea of John Mashey)
#'   @param ... further parameters passed to the fitting function, in the case of loess, for example, "span = .9", or "family = 'symmetric'"
#'
#' Returns:
#'   @return ggplot2 object
#'
#' @export
#' @references See citation("microbiome") 
#' @author Based on the original version from Felix Schönbrodt. Contact: Leo Lahti \email{microbiome-admin@@googlegroups.com}
#' @keywords utilities

vwReg <- function(formula, data, title="", B=1000, shade=TRUE, shade.alpha=.1, spag=FALSE, mweight=TRUE, show.lm=FALSE, show.median = TRUE, median.col = "white", show.CI=FALSE, method=loess, bw=FALSE, slices=200, palette=colorRampPalette(c("#FFEDA0", "#DD0000"), bias=2)(20), ylim=NULL, quantize = "continuous",  ...) {
IV <- all.vars(formula)[2]
DV <- all.vars(formula)[1]
data <- na.omit(data[order(data[, IV]), c(IV, DV)])
if (bw == TRUE) palette <- colorRampPalette(c("#EEEEEE", "#999999", "#333333"), bias=2)(20)
print("Computing boostrapped smoothers ...")
newx <- data.frame(seq(min(data[, IV]), max(data[, IV]), length=slices))
colnames(newx) <- IV
l0.boot <- matrix(NA, nrow=nrow(newx), ncol=B)
l0 <- method(formula, data)
for (i in 1:B) {
data2 <- data[sample(nrow(data), replace=TRUE), ]
data2 <- data2[order(data2[, IV]), ]
if (class(l0)=="loess") {
m1 <- method(formula, data2, control = loess.control(surface = "i", statistics="a", trace.hat="a"), ...)
} else {
m1 <- method(formula, data2, ...)
}
l0.boot[, i] <- predict(m1, newdata=newx)
}
# compute median and CI limits of bootstrap
library(plyr)
library(reshape2)
CI.boot <- adply(l0.boot, 1, function(x) quantile(x, prob=c(.025, .5, .975, pnorm(c(-3, -2, -1, 0, 1, 2, 3))), na.rm=TRUE))[, -1]
colnames(CI.boot)[1:10] <- c("LL", "M", "UL", paste0("SD", 1:7))
CI.boot$x <- newx[, 1]
CI.boot$width <- CI.boot$UL - CI.boot$LL
# scale the CI width to the range 0 to 1 and flip it (bigger numbers = narrower CI)
CI.boot$w2 <- (CI.boot$width - min(CI.boot$width))
CI.boot$w3 <- 1-(CI.boot$w2/max(CI.boot$w2))
# convert bootstrapped spaghettis to long format
b2 <- melt(l0.boot)
b2$x <- newx[,1]
colnames(b2) <- c("index", "B", "value", "x")
library(ggplot2)
library(RColorBrewer)
p1 <- ggplot(data, aes_string(x=IV, y=DV)) + theme_bw()
if (shade == TRUE) {
quantize <- match.arg(quantize, c("continuous", "SD"))
if (quantize == "continuous") {
print("Computing density estimates for each vertical cut ...")
flush.console()
if (is.null(ylim)) {
min_value <- min(min(l0.boot, na.rm=TRUE), min(data[, DV], na.rm=TRUE))
max_value <- max(max(l0.boot, na.rm=TRUE), max(data[, DV], na.rm=TRUE))
ylim <- c(min_value, max_value)
}
# vertical cross-sectional density estimate
d2 <- ddply(b2[, c("x", "value")], .(x), function(df) {
res <- data.frame(density(df$value, na.rm=TRUE, n=slices, from=ylim[1], to=ylim[2])[c("x", "y")])
#res <- data.frame(density(df$value, na.rm=TRUE, n=slices)[c("x", "y")])
colnames(res) <- c("y", "dens")
return(res)
}, .progress="text")
maxdens <- max(d2$dens)
mindens <- min(d2$dens)
d2$dens.scaled <- (d2$dens - mindens)/maxdens
## Tile approach
d2$alpha.factor <- d2$dens.scaled^shade.alpha
p1 <- p1 + geom_tile(data=d2, aes(x=x, y=y, fill=dens.scaled, alpha=alpha.factor)) + scale_fill_gradientn("dens.scaled", colours=palette) + scale_alpha_continuous(range=c(0.001, 1))
}
if (quantize == "SD") {
## Polygon approach
SDs <- melt(CI.boot[, c("x", paste0("SD", 1:7))], id.vars="x")
count <- 0
d3 <- data.frame()
col <- c(1,2,3,3,2,1)
for (i in 1:6) {
seg1 <- SDs[SDs$variable == paste0("SD", i), ]
seg2 <- SDs[SDs$variable == paste0("SD", i+1), ]
seg <- rbind(seg1, seg2[nrow(seg2):1, ])
seg$group <- count
seg$col <- col[i]
count <- count + 1
d3 <- rbind(d3, seg)
}
p1 <- p1 + geom_polygon(data=d3, aes(x=x, y=value, color=NULL, fill=col, group=group)) + scale_fill_gradientn("dens.scaled", colours=palette, values=seq(-1, 3, 1))
}
}
print("Build ggplot figure ...")
flush.console()
if (spag==TRUE) {
p1 <- p1 + geom_path(data=b2, aes(x=x, y=value, group=B), size=0.7, alpha=10/B, color="darkblue")
}
if (show.median == TRUE) {
if (mweight == TRUE) {
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=M, alpha=w3^3), size=.6, linejoin="mitre", color=median.col)
} else {
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=M), size = 0.6, linejoin="mitre", color=median.col)
}
}
# Confidence limits
if (show.CI == TRUE) {
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=UL, group=B), size=1, color="red")
p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=LL, group=B), size=1, color="red")
}
# plain linear regression line
if (show.lm==TRUE) {p1 <- p1 + geom_smooth(method="lm", color="darkgreen", se=FALSE)}
p1 <- p1 + geom_point(size=1, shape=21, fill="white", color="black")
if (title != "") {
p1 <- p1 + opts(title=title)
}
p1  + opts(legend.position="none")
}


#' Visualize the specified fields of a shape object on using 1- or 2-way color scale. 
#' 
#' This function is used for fast investigation of shape objects; standard visualization choices are made
#' automatically; fast and easy-to-use but does not necessarily provide optimal visualization.
#'
#' @param sp Shape object 
#' @param varname Variable name from the shape object sp to be visualized
#' @param type String. Specifies visualization type. Options: "oneway", "twoway", "qualitative", "custom". See details. 
#' @param ncol Number of distinct colors shades
#' @param at Color transition points
#' @param palette Optional. Color palette.
#' @param main Optional. Title text.
#' @param colorkey Logical. Show color interpretation in a separate legend.
#' @param lwd Optional. Line width for shape polygon borders.
#' @param border.col Optional. Color for shape polygon borders.
#' @param col.regions Optional. Specify color for the shape object regions manually.
#' @param min.color Color for minimum values in the color scale
#' @param max.color Color for maximum values in the color scale
#'
#' @return ggplot2 object
#' @details Visualization types include: oneway/sequential (color scale ranges from white to dark red, or custom color given with the palette argument); twoway/bipolar/diverging (color scale ranges from dark blue through white to dark red; or custom colors); discrete/qualitative (discrete color scale; the colors are used to visually separate regions); and "custom" (specify colors with the col.regions argument)
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # PlotShape(sp, varname) 
#' @seealso \code{\link{get.hsy}}
#' @keywords utilities


PlotShape <- function (sp, varname, type = "oneway", ncol = 10, at = NULL, palette = NULL, main = NULL, colorkey = TRUE, lwd = .4, border.col = "black", col.regions = NULL, min.color = "white", max.color = "red") {

  # type = "oneway"; ncol = 10; at = NULL; palette = NULL; main = NULL; colorkey = TRUE; lwd = .4; border.col = "black"; col.regions = NULL

  # FIXME: check if we could here use standard palettes and avoid dependency
  .InstallMarginal("RColorBrewer")

  pic <- NULL

  if (is.null(main)) {
    main <- varname
  }

  if (is.factor(sp[[varname]]) && (!type %in% c("discrete", "qualitative", "custom"))) {
    warning("Discrete/custom color scale required for factors; resetting color type")
    type <- "qualitative"
  }

  if (type %in% c("oneway", "quantitative", "sequential")) {
    # Define color palette
    if (is.null(palette)) {
      palette <- colorRampPalette(c(min.color, max.color), space = "rgb")
    }

    sp[[varname]] <- as.numeric(as.character(sp[[varname]]))

    if (is.null(at)) { 
      mini <- min(sp[[varname]]) - 1
      maxi <- max(sp[[varname]]) + 1
      at <- seq(mini, maxi, length = ncol) 
    } else {
      # Override ncol if at is given
      ncol <- length(at)
    }

    if (is.null(main)) {
      main <- varname
    }

    if (is.null(col.regions)) {
      col.regions <- palette(ncol)
    }

    q <- spplot(sp, varname,
            col.regions = col.regions,
	    main = main,
	    colorkey = colorkey,
	    lwd = lwd,
	    col = border.col,
	    at = at)
           

  } else if (type %in% c("twoway", "bipolar", "diverging")) { 

    # Plot palette around the data average
    # To highlight deviations in both directions

    # Define color palette
    if (is.null(palette)) {
      palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
    }

    if (is.null(at)) { 

      # Linear color palette around the average
      mini <- min(sp[[varname]])
      maxi <- max(sp[[varname]])
      at <- seq(mini - 1, maxi + 1, length = ncol) 

    } else {
      # Override ncol if at is given
      ncol <- length(at)
    }
    # message(at)

    if (is.null(main)) {
      main <- varname
    }

    if (is.null(col.regions)) {
      col.regions <- palette(ncol)
    }

    q <- spplot(sp, varname,
            col.regions = col.regions,
	    main = main,
	    colorkey = colorkey,
	    lwd = lwd,
	    col = border.col,
	    at = at)

  } else if (type %in% c("qualitative", "discrete")) {

    vars <- factor(sp[[varname]])
    sp[[varname]] <- vars
    
    if (is.null(col.regions) && length(sp[[varname]]) == length(levels(sp[[varname]]))) {
      # Aims to find colors such that neighboring polygons have 
      # distinct colors
      cols <- sorvi::GenerateMapColours(sp) # Generate color indices
      col.regions <- RColorBrewer::brewer.pal(max(cols), "Paired")[cols]

    } else if ( is.null(col.regions) ) {
      
      # Use ncol colors, loop them to fill all regions    
      nlevels <- length(levels(vars))
      col.regions <- rep(RColorBrewer::brewer.pal(ncol, "Paired"), ceiling(nlevels/ncol))[1:nlevels]

    }

    colorkey <- FALSE

    pic <- spplot(sp, varname, col.regions = col.regions, main = main, colorkey = colorkey, lwd = lwd, col = border.col)

  } else if (type == "custom") {

    # User-defined colors for each region  
    if (is.null(col.regions)) {  
      stop("Define region colors through the col.regions argument 
      		   in the custom mode!")
    }

  }

  if (is.null(col.regions)) {
    col.regions <- palette(ncol)
  }

  if (is.null(pic)) {
    pic <- spplot(sp, varname, col.regions = col.regions, main = main, colorkey = colorkey, lwd = lwd, col = border.col, at = at)
  }
	  
  print(pic)

  pic
}


#' Visualize a matrix with one or two-way color scale. 
#' TODO: one-way color scale
#' 
#' This function is used for fast investigation of matrix objects; standard visualization choices are made
#' automatically; fast and easy-to-use but does not necessarily provide optimal visualization.
#'
#' @param mat matrix
#' @param type String. Specifies visualization type. Options: "oneway" (color scale ranges from white to dark red; the color can be changed if needed); "twoway" (color scale ranges from dark blue through white to dark red; colors can be changed if needed)
#' @param midpoint middle point for the color plot: smaller values are shown with blue, larger are shown with red in type = "twoway"
#' @param palette Optional. Color palette.
#' @param colors Optional. Colors.
#' @param col.breaks breakpoints for the color palette
#' @param interval interval for palette color switches
#' @param plot.axes String. Indicates whether to plot x-axis ("x"), y-axis ("y"), or both ("both").
#' @param row.tick interval for plotting row axis texts
#' @param col.tick interval for plotting column axis texts
#' @param cex.xlab use this to specify distinct font size for the x axis
#' @param cex.ylab use this to specify distinct font size for the y axis
#' @param xlab optional x axis labels
#' @param ylab optional y axis labels
#' @param limit.trunc color scale limit breakpoint
#' @param mar image margins
#' @param ... optional parameters to be passed to function 'image', see help(image) for further details
#' @return A list with the color palette (colors), color breakpoints (breaks), and palette function (palette.function)
#' @export
#' @references See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); PlotMatrix(mat, "twoway", midpoint = 3) 
#' @keywords utilities

PlotMatrix <- function (mat, type = "twoway", midpoint = 0, 
	      	        palette = NULL, colors = NULL, col.breaks = NULL, interval = .1, 
			plot.axes = "both",
			row.tick = 1, col.tick = 1, 
			cex.xlab = .9, cex.ylab = .9, 
			xlab = NULL, ylab = NULL,
			limit.trunc = 0, mar = c(5, 4, 4, 2), ...) {

  # Center the data and color breakpoints around the specified midpoint
  mat <- mat - midpoint

  if (length(col.breaks) == 0)  {
    m <- max(round(max(abs(mat)), limit.trunc) - interval, 0)
    mm <- m + interval/2
    vals <- seq(interval/2,mm,interval)
    # Set col.breaks evenly around zero
    col.breaks  <- c(-(m + 1e6), c(-rev(vals), vals), m+1e6)
  }
		  
  if (is.null(palette)) {
    my.palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
  } else if (palette == "blue-black-red") {
    my.palette <- colorRampPalette(c("blue", "black", "red"), space = "rgb")
  } else if (palette == "blue-white-red") {
    my.palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
  } else if (palette == "blue-white-yellow") {
    my.palette <- colorRampPalette(c("blue", "white", "yellow"), space = "rgb")
  } else if (palette == "blue-black-yellow") {
    my.palette <- colorRampPalette(c("blue", "black", "yellow"), space = "rgb")
  } else if (palette == "bw") {
    gray.palette <- function (int) {
      gray(seq(0,1,length=int))
    }
    my.palette <- gray.palette
  }

  # if mycolors is provided it overrides palette
  if (is.null(colors)) { colors <- my.palette(length(col.breaks) - 1) }
	   		      
  # transpose and revert row order to plot matrix in the same way it
  # appears in its numeric form
  par(mar = mar)
  image(t(mat[rev(seq(nrow(mat))),]), col = colors, xaxt = 'n', yaxt = 'n', zlim = range(col.breaks), breaks = col.breaks, ...)

  if (plot.axes == "both" || plot.axes == TRUE) {
    
    if (is.null(xlab)) {
      v <- seq(1, ncol(mat), col.tick) # take every nth index
      axis(1, at = seq(0,1,length = ncol(mat))[v], labels = colnames(mat)[v], cex.axis=cex.xlab, las=2, ...)    
    } else {
      axis(1, at = seq(0,1,length = ncol(mat)), labels = xlab, cex.axis=cex.xlab, las=2, ...)    
    }

    if (is.null(ylab)) {
      v <- seq(1, nrow(mat), row.tick) # take every nth index
      axis(2, at = seq(0,1,length = nrow(mat))[v], labels = rev(rownames(mat))[v], cex.axis=cex.ylab, las=2, ...)
    } else {  
      axis(2, at = seq(0,1,length = nrow(mat)), labels = ylab, cex.axis=cex.ylab, las=2, ...)
    }

  } else if (plot.axes == "x") {

    if (is.null(xlab)) {
      v <- seq(1, ncol(mat), col.tick) # take every nth index
      axis(1, at = seq(0,1,length = ncol(mat))[v], labels = colnames(mat)[v], cex.axis=cex.xlab, las=2)    
    } else {
      axis(1, at = seq(0,1,length = ncol(mat)), labels = xlab, cex.axis=cex.xlab, las=2)    
    }

  } else if (plot.axes == "y") {

    if (is.null(ylab)) {
      v <- seq(1, nrow(mat), row.tick) # take every nth index
      axis(2, at = seq(0, 1, length = nrow(mat))[v], labels = rev(rownames(mat))[v], cex.axis = cex.xlab, las = 2)
    } else {  
      axis(2, at = seq(0, 1, length = nrow(mat)), labels = ylab, cex.axis=cex.xlab, las=2)
    }
  }
  
  # Return default margins
  par(mar = c(5, 4, 4, 2) + 0.1)
 
  return(list(colors = colors, breaks = col.breaks + midpoint, palette.function = my.palette))
      	  
}


#' Visualize color scale for PlotMatrix output
#' NOTE: Experimental. To be tested thoroughly.
#' 
#' @param breaks breakpoints for colors
#' @param colors Optional. Colors.
#' @param m overrides breaks, mypalette and produces a plot that ranges (-m,m)
#' @param label.step step between label text plotting
#' @param interval interval for palette color switches
#' @param two.sided indicates one- or two-sided color palette
#' @param label.start start point for the labels
#' @param Nlab number of labels
#' @param palette.function palette color scale function
#' @param ndigits number of digits to plot
#' @param ... optional parameters to be passed to function 'axis', see help(axis) for further detai
#' @return A list with the color palette (palette), color breakpoints (breaks), and palette function (palette.function)
#' @export
#' @references See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); pm <- PlotMatrix(mat, "twoway", midpoint = 3); PlotScale(pm$colors, pm$breaks)
#' @keywords utilitie

PlotScale <- function (breaks, colors = NULL, m = NULL, label.step = 2, interval=.1, two.sided = TRUE, label.start = 1.00, Nlab = 3, palette.function = NULL, ndigits = 2, ...) {

  if (two.sided) {
    
    if (length(m)>0) {
      breaks <- sorvi::set.breaks(m, interval)
      image(t(as.matrix(seq(-mm, mm, length = 100))), col = colors, xaxt = 'n', yaxt = 'n', zlim = range(breaks), breaks=breaks)
    } else {
      image(t(as.matrix(breaks)), col = colors, xaxt = 'n',yaxt = 'n', zlim = range(breaks), breaks = breaks)
    }
  
    mm1 <- sort(breaks)[[2]]
    mm2 <- rev(sort(breaks))[[2]]
    
    tmp <- unlist(strsplit(as.character(mm1),"\\."))

    digit.step <-10^(-ndigits)
    labs <- round(seq(mm1, mm2, by = digit.step), ndigits)
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- length(labs) - 1 
    inds <- seq(start.position, end.position, length = Nlab)
      
    axis(2, at = seq(0, 1, length = Nlab), labels = labs[inds], las=2, ...)
  }

  if (!two.sided) {

    mm <- max(breaks) + 1e6 # infty
    m <- max(breaks)
 
    labs <- seq(0,m,label.step)
    #inds = sapply(labs,function(lab){min(which(lab<=breaks))})
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)  

    image(t(as.matrix(seq(0, m, length = 100))), col = colors, xaxt='n', yaxt='n', zlim=range(breaks), breaks=breaks)
    
    axis(2, at = seq(0, 1, length=Nlab), labels=labs[inds], las=2, ...)
  }
  
}


#' Set breaks for color palette. Internal function.
#'
#' @param mat data matrix or vector for which the breaks will be deterined 
#' @param interval interval of color breaks
#' @return A vector of breakpoints
#' @references See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); pm <- PlotMatrix(mat, "twoway", midpoint = 3); PlotScale(pm$colors, pm$breaks)
#' @keywords utilities

set.breaks <- function (mat, interval=.1) {
  if (max(abs(mat))>1) {
    m <- floor(max(abs(mat)))
  } else {
    m <- round(max(abs(mat)),nchar(1/interval)-1)
  }

  mm <- m + interval/2
  vals <- seq(interval/2,mm,interval)
  # Note: the first and last values mimic infinity
  mybreaks  <- c(-(m+1e6),c(-rev(vals),vals),m+1e6)
  mybreaks
}



#' Generate color indices for shape object with the aim to color 
#  neighboring objects with distinct colors.
#'
#' @param sp SpatialPolygonsDataFrame object
#' @return Color index vector
#' @references See citation("sorvi") 
#' @export
#' @author Modified from the code by Karl Ove Hufthammer from http://r-sig-geo.2731867.n2.nabble.com/Colouring-maps-so-that-adjacent-polygons-differ-in-colour-td6237661.html; modifications by Leo Lahti
#' @examples # col <- GenerateMapColours(sp)    
#' @keywords utilities


GenerateMapColours <- function(sp) {

  nb <- spdep::poly2nb(sp)   # Generate neighbours lists

  n <- length(sp)            # Number of polygons

  cols <- numeric(n)        # Initial colouring

  cols[1] <- 1              # Let the first polygon have colour 1

  cols1n <- 1:n             # Available colour indices

  for(i in 2:n)
    cols[i] <- which.min(cols1n %in% cols[nb[[i]]])

  cols

}

