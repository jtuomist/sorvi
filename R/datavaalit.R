# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Load data sets from datavaalit.fi web service
#'
#' @param data.id Data set ID
#'
#' @return rjson object
#' 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @export

ReadDatavaalit <- function (data.id) {

  # Read election info
  if (data.id ==  "election.data") { 

    f <- "http://beta.datavaalit.fi/api/v1/election/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))
    
  } else if (data.id == "municipality.data") {

    f <- "http://beta.datavaalit.fi/api/v1/municipality/?format=json&limit=500"
    dat <- fromJSON(paste(readLines(f), collapse = ""))

  } else if (data.id == "hel.council.members") {
    f <- "http://beta.datavaalit.fi/api/v1/council_member/?format=json&limit=85"
    # FIXME: Extremely bad idea to have the function to return different value
    # types depending on the data.id
    dat <- new("council", f)
  }
  dat  
}
