# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  
#' GetParliamentaryElectionData
#'
#' Get parliamentary election data at selected regional level.
#' 
#' @param level Indicate whether to get data at the level of municipality or election.region
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # 
#' @keywords utilities
GetParliamentaryElectionData <- function (level) {

  if (!try(require(reshape))) { 
    message("Function GetParliamentaryElectionData requires package 'reshape' Package not found, installing...")
    install.packages(reshape) # Install the packages
    require(reshape) # Remember to load the library after installation
  }


  if (level == "municipality") {

    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_fi.asp

    # 2.2 Äänioikeutetut ja äänestäneet sekä ennakolta äänestäneet sukupuolen mukaan kunnittain eduskuntavaaleissa 2011 ja 2007
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_104_fi.px"
    px <- read.px(url)
    df <- try(as.data.frame(px))
    kaava <- as.formula("Vaalipiiri.ja.kunta~Äänestystiedot~Lukumäärätiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    # Separate tables and preprocess
    tab1 <- tmp[,,"Lukumäärä 2007"]
    tab2 <- tmp[,,"Lukumäärä 2011"]
    tab3 <- tmp[,,"-Osuus äänistä"]
    tab4 <- tmp[,,"- Osuus äänistä"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumäärä 2007"]), "(Lukumäärä 2007)")
    colnames(tab2) <- paste(colnames(tmp[,,"Lukumäärä 2011"]), "(Lukumäärä 2011)")
    colnames(tab3) <- paste(colnames(tmp[,,"-Osuus äänistä"]), "(Osuus 2011)")
    colnames(tab4) <- paste(colnames(tmp[,,"- Osuus äänistä"]), "(Osuus 2007)")
    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Niistä Ruotsissa", rnams)]
    rnams <- rnams[-grep("Suomessa asuvat Suomen kansalaiset", rnams)]
    rnams <- rnams[-grep("Ulkomailla asuvat Suomen kansalaiset", rnams)]
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # Parse municipality codes and names
    v <- ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})

    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]

    # TODO
    #8.2 Pienin äänimäärä ja vertausluku, jolla ehdokas on tullut valituksi 
    # puolueittain ja vaalipiireittäin eduskuntavaaleissa 2011
    #url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/186_evaa_tau_102_fi.px"
    #Alue~Puolue~Pienimmät.luvut

  } else if (level == "election.region") {

    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/evaa_fi.asp

    #2.3 Hylätyt äänestysliput hylkäysperusteen ja vaalipiirin mukaan 
    # eduskuntavaaleissa 2011
    #http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_105_fi.px

    # 8.1 Vaaliliitot ja niiden äänimäärät vaalipiireittäin eduskuntavaaleissa 2011
    #url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/185_evaa_tau_101_fi.csv.gz"  
    #Vaaliliitto.Puolue.Vaalipiiri~Lukumäärä
  
    #2.1 Äänioikeutetut ja äänestäneet sekä ennakolta äänestäneet sukupuolen 
    # mukaan vaalipiireittäin eduskuntavaaleissa 2011
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_103_fi.px"

    # Read election data from Statistics Finland			 
    px <- read.px(url) 
    df <- try(as.data.frame(px))
    kaava <- as.formula("Vaalipiiri~Äänestystiedot~Lukumäärätiedot")
    tmp <- reshape::cast(df, kaava, value="dat")

    # Separate the tables
    tab1 <- tmp[,,1]
    tab2 <- tmp[,,2]
    colnames(tab1) <- paste(colnames(tmp[,,"Lukumäärä"]), "(Lukumäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus äänistä"]), "(Osuus äänistä)")
    tab <- cbind(tab1, tab2)

    # Keep only election.region level data
    rnams <- rownames(tab)
    rnams <- rnams[grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    colnames(tab) <- paste("Eduskuntavaalit 2011", colnames(tab))

    tab$Vaalipiiri <- sapply(rnams, function (s) {ss <- strsplit(s, " ")[[1]]; paste(ss[-1], collapse = " ")})
    tab$Vaalipiiri.Koodi <- sapply(rnams, function (s) {strsplit(s, " ")[[1]][[1]]})

    # Read more election data from Statistics Finland			 
    px <- read.px("http://pxweb2.stat.fi/database/StatFin/vaa/evaa/120_evaa_tau_105_fi.px") 
    df <- try(as.data.frame(px))
    kaava <- as.formula("Vaalipiiri~Hylkäysperuste")
    tab2 <- reshape::cast(df, kaava, value="dat")

    # Keep only election.region level data
    rownames(tab2) <- as.character(tab2[,1])
    rnams <- rownames(tab2)
    rnams <- rnams[grep("vaalipiiri", rnams)]
    tab2 <- as.data.frame(tab2[rnams, ])

    colnames(tab2) <- paste("Eduskuntavaalit 2011", colnames(tab2))

    tab2$Vaalipiiri <- sapply(rnams, function (s) {ss <- strsplit(s, " ")[[1]]; paste(ss[-1], collapse = " ")})
    tab2$Vaalipiiri.Koodi <- sapply(rnams, function (s) {strsplit(s, " ")[[1]][[1]]})


    tab <- cbind(tab, tab2[match(tab$Vaalipiiri, tab2$Vaalipiiri),])
  
  }

  rownames(tab) <- tab$Kunta
  colnames(tab) <- paste("Eduskuntavaalit_2007_2011", colnames(tab))
  

  tab

}

##########################################################################