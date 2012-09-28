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


#' Description:
#' Function for reading in Finnish Municipal Election candidate data published
#' by Ministry of justice. As of 27-09-2012, the data and descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#'
#' Candidate data comes in divided into 14 Election districts (vaalipiiri).
#'
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadCandidates <- function(district.id, cache=NA) {

  ReadElectionData("candidates", district.id, cache)
  
}

#' Description:
#' Wrapper function for ReadCandidates that gets all 14 districts and returns
#' all data in a single data frame.
#'
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadAllCandidates <- function(cache=NA) {
  
  election.district.ids  <- 1:15
  # Remember, there is no id 5!
  election.district.ids  <- election.district.ids[-c(5)]

  # Determine the cache dir if needed
  # cache = "."  
  all.districts <- lapply(election.district.ids, 
                          function(x) {ReadCandidates(x, cache)})
  
  # Bind everything into a single data frame
  candidates <- do.call("rbind", all.districts)
  
  return(candidates)
}

#' Description:
#' Wrapper function for ReadParties that gets all 14 districts and returns
#' all data in a single data frame.
#'
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadAllParties <- function(cache=NA) {
  
  election.district.ids  <- 1:15
  # Remember, there is no id 5!
  election.district.ids  <- election.district.ids[-c(5)]

  # Determine the cache dir if needed
  # cache = "."  
  all.districts <- lapply(election.district.ids, 
                          function(x) {ReadParties(x, cache)})
  
  # Bind everything into a single data frame
  parties <- do.call("rbind", all.districts)
  
  return(parties)
}


# Private functions -------------------------------------------------------

.readCommonData <- function() {
  require(rjson)
  data.file <- system.file("extdata/common_data.json", package = "sorvi")
  return(fromJSON(paste(readLines(data.file), collapse = "")))
} 

.readCommonData2 <- function() {
  require(rjson)
  data.file <- system.file("extdata/common_data2.json", package = "sorvi")
  return(fromJSON(paste(readLines(data.file), collapse = "")))
} 

# ---------------------------------------------------------------


#' Description:
#' Function for reading in Finnish Municipal Election political party data 
#  published by Ministry of justice. As of 27-09-2012, the data and 
#  descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#  
#' Party data comes divided into 14 Election districts (vaalipiiri).
#'
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadParties <- function(district.id, cache=NA) {
  
  ReadElectionData("parties", district.id, cache)

}


#' Description:
#' Function for reading in Finnish Municipal Election data 
#  published by Ministry of justice. As of 27-09-2012, the data and 
#  descriptions are
#' available from http://192.49.229.35/K2012/s/ehd_listat/kokomaa.htm#ladattavat
#  
#' Data comes divided into 14 Election districts (vaalipiiri).
#' @param which.data Options: "candidates", "parties"
#' @param district.id integer marking the election district ID. Options: [1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#' @param cache character directory path to location where files are cached
#'
#' @return Data frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples # 
#' @keywords utilities

ReadElectionData <- function(which.data, district.id, cache=NA) {
  
  # Body of the filename is always the same
  if (which.data == "parties") { 
    file.name.body <- "puo_"
  } else if (which.data == "candidates") { 
    file.name.body <- "ehd_"
  }

  # Coerce the disrict id into a character for building file paths / urls
  district.id.char <- as.character(district.id)

  # Padding with leading zeros if needed
  if (nchar(district.id.char) == 1) {
    district.id.char <- paste("0", district.id.char, sep="")
  }
  
  # Construct the file name
  file.name <- paste(file.name.body, district.id.char, ".csv", sep="")
  
  # Either use the cached files or fetch over network
  if (is.na(cache)) {
                          
    data.source <- paste("http://192.49.229.35/K2012/s/ehd_listat/",
                          file.name, sep="")

    message(paste("Reading data from URL", data.source))
    
  } else {
    
    if (file.exists(cache)) {
      data.source <- file.path(cache, file.name)
      
      # Check if the actual file exists
      if (!file.exists(data.source)) {
        stop(paste("File", data.source, "does not exist."))
      } else {
        message(paste("Using cached version", data.source))
      }
      
    } else {
      stop("Cache requested, but not found")
    }
    
    # Read the table over network, use the encodign provided by MoJ
  }
  # Read the data from selected data source
  raw.data <- read.table(data.source, sep=";", as.is=TRUE, strip.white=TRUE,
                         fileEncoding="iso-8859-1")
  
  # The the suitable header names from common_data.json
  header <- .readCommonData()
  
  # In the original csv file, there is also a trailing ";" -> there really is
  # only 29 / 35 columns (as of 27.9.2012); more columns will appear 
  # on the election day
  if (which.data == "parties") {
    raw.data <- raw.data[1:35]
    header <- header$OMpuolueet$header
  } else if (which.data == "candidates") {
    raw.data <- raw.data[1:29]
    header <- header$OMehdokkaat$header
  }

  # Set the header
  colnames(raw.data)  <- header[1:length(raw.data)]
  
  # TODO: make a separate preprocessing function
  # 	  and possibly get all conversions from a JSON database
  # 	  created for this purpose
  #
  # Column pre-processing
  if (which.data == "candidates") {
    raw.data$Sukupuoli <- factor(raw.data$Sukupuoli, labels=c("Mies", "Nainen"))
  }

  return(raw.data)
  
}