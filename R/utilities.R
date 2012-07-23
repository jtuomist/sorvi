
# To call in the statistician after the experiment is done may be no more
# than asking him to perform a post-mortem examination: he may be able to
# say what the experiment died of. ~ Sir Ronald Aylmer Fisher


# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Sort data frame
#'
#' @param df data.frame to be sorted by the specified columns
#' @param sortvar variable/s according which the data.frame shall be sorted
#' @param ... Other arguments to pass
#' @return data.frame (sorted version)
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # dfsort(df, x, -z) 
#' @keywords utilities


dfsort <- function(df, sortvar, ...) {
  # Korvaa skandit a/o versiolla
  attach(df)
  df <- df[with(df,order(sortvar,...)),]
  return(df)
  detach(df)

}

#' Replace special characters with standard ones.
#'
#' @param s string from which the special chars should be removed
#' @return string with special chars replaced by standard ones
#' @export
#' @note iconv function provides better tools for these purposes and is now the main tool
#' This function is kept for compatibility with the older versions.
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # korvaa.skandit("my.string.here") # if no, special chars, the same string is returned
#' @keywords utilities

korvaa.skandit <- function (s) {

  s <- gsub("\\xe4", "a", s)
  s <- gsub("\\xC4", "A", s)
  s <- gsub("\\xD6", "O", s)
  s <- gsub("\\xf6", "o", s)
  s <- gsub("\\xE5", "a", s)
  s <- gsub("\\U3e34633c", "A", s)

  s

}

#' Check if the given object is an url string
#'
#' Arguments:
#'  @param s input object to check
#'
#' Returns:
#'  @return TRUE/FALSE indicating whether the input string is a valid URL.
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # is.url("http://aa.px")
#' @keywords utilities
is.url <- function (s) {
  (class(s) == "character" && substr(s,1,7) == "http://")
}


#' Retrieve shape objects by their file names.
#'  
#' Takes list of shape file names (or IDs without the .shp ending).
#' Returns a corresponding list of shape objects from the working directory, 
#' or from the directory path specified as part of the file name.
#'
#' @param files vector of input files
#' @param proj4string projection information
#' @return shape object, or a list of shape objects, depending on the length of function argument (a single file name vs. multiple file names)
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # ReadShape(files)
#' @keywords utilities

ReadShape <- function (files, proj4string = NA) {

  .InstallMarginal("maptools")

  ids <- unlist(sapply(files, function (x) {strsplit(x, "\\.")[[1]][[1]]}))
   
  shapedata <- list()

  for (id in ids) {
    print(id)
    shapedata[[id]] <- try(maptools::readShapePoly(id, 
                                         proj4string=CRS(as.character(proj4string))))
  }

  # If just one file converted, give directly the shape file as out put
  # (and not a list)
  if (length(files) == 1) {
    shapedata <- shapedata[[1]]
  }

  shapedata

}

#' Remove spaces from a string (single string or vector/list of strings).
#'
#' @param s string or vector/list of strings
#' @return string without spaces
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # strstrip("a b") # returns "ab"
#' @keywords utilities


strstrip <- function (s) {

  if (length(s) == 1) {
    stripped <- strstrip.single(s)
  } else {
    stripped <- sapply(s, strstrip.single)
  }

  stripped
}


#' Remove spaces from a single string
#'
#' @param s string
#' @return string without spaces
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # strstrip.single("a b") # returns "ab"
#' @keywords utilities

strstrip.single <- function (s) {

  # Remove spaces from a string

  # (C) Leo Lahti 2008-2011
  # FreeBSD license (keep this notice)

  # Strip string i.e. remove spaces from the beginning and end
  while (substr(s,1,1)==" ") {
    s <- substr(s,2,nchar(s))
  }
  while (substr(s,nchar(s),nchar(s))==" ") {
    s <- substr(s,1,nchar(s)-1)
  }
  s
}

#' Strip string i.e. remove spaces from the beginning and end
#' @param s string or character vector
#'
#' @return Stripped string
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples 
#' #s2 <- Strip(s) 
#' @keywords utilities
Strip <- function (s) {

  ss <- c()
  for (i in 1:length(s)) {
    si <- s[[i]]

    # Strip string i.e. remove spaces from the beginning and end
    while (substr(si,1,1)==" ") {
      si <- substr(si, 2, nchar(si))
    }
    while (substr(si, nchar(si), nchar(si))==" ") {
      si <- substr(si, 1, nchar(si) - 1)
    }
    ss[[i]] <- si
  }
  ss
}




#' Shift the data matrix column means to a specified value
#'
#' @param X data matrix
#' @param rm.na logical: remove NAs
#' @param meanvalue mean for columns (by default 0)
#'
#' @return shifted data matrix
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

CenterData <- function (X, rm.na = TRUE, meanvalue = 0) {

  if (!rm.na) {
    xcenter <- colMeans(X)
    X2 <- X - rep(xcenter, rep.int(nrow(X), ncol(X)))
  } else {	
    X2 <- array(NA, dim = c(nrow(X), ncol(X)), dimnames = dimnames(X))
    for (i in 1:ncol(X)) {
      x <- X[,i]
      nainds <- is.na(x)
      xmean <- mean(x[!nainds])
      X2[!nainds,i] <- x[!nainds] - xmean 	
    }
    dimnames(X2) <- dimnames(X)
  }

  # Shift the data to a specified value
  X2 <- X2 + meanvalue
  
  X2
}


#' Scale data matrix columns to unit variance
#'
#' @param X data matrix
#' @param rm.na logical: remove NAs
#' @param sd.value standard deviation for columns (by default 1)
#'
#' @return scaled data matrix
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

UnitScale <- function(X, rm.na = TRUE, sd.value = NULL) {

  #remove col means from matrix X
  # FIXME: use centerData function here
  if (rm.na) {
    X2 <- matrix(NA,nrow=nrow(X),ncol=ncol(X))
    for (i in 1:ncol(X)) {
      x <- X[,i]
      nainds <- is.na(x)
      x <- x[!nainds]
      X2[!nainds,i] <- x/sd(x)
    }
  }
  if (!rm.na) {
    X2 <- apply(X,2,function(x){x/sd(x)})
  }
  if (length(sd.value)>0) {
    # Scale to predefined sd
    X2 <- apply(X2,2,function(x){x*sd.value})
  }

  dimnames(X2) <- dimnames(X)
  
  X2
}

#' Install marginal dependencies on-demand from other functions.
#' 
#' Function is always supposed to be called from a parent function and if the
#' marginal depedency package is not installed, the function will report the 
#' name of the parent function requiring the package. Note that the whole call
#' stack is not reported, only the immediate parent.
#'
#' @param package String name for the name to be installed
#' @param ... further arguments passed on to install.packages
#'
#' @return Invisible NULL
#' 
#' @note meant for package internal use only
#' @author Joona Lehtomaki \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @keywords utilities

.InstallMarginal <- function(package, ...) {
  if (suppressWarnings(!require(package, character.only=TRUE, quietly=TRUE))) { 
    parent.function <- sys.calls()[[1]][1]
    message(paste("Function ", parent.function, " requires package: ", package,
                  ". Package not found, installing...", sep=""))
    install.packages(package, ...) # Install the packages
    require(package, character.only=TRUE) # Remember to load the library after installation
  }
}