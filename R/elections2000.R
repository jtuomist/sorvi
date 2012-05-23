# This file is a part of the soRvi program
# louhos.github.com/sorvi/

# Copyright (C) 2012 Leo Lahti, Juuso Parkkinen and Joona Lehtomäki. 
# All rights reserved.
# Contact: <leo.lahti@iki.fi>

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
#' GetMunicipalElectionData2000
#'
#' Get municipal election data from Statistics Finland (C) 2012
#' http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/kvaa_2000_fi.asp
#' 
#' @param which Indicate which of the available Statistics Finland data sets to parse. Options: election.statistics, candidates, selected.candidates.by.region, selected.candidates.all, parties, all.municipality.level.data
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2000 <- function (which = "election.statistics") {
  
  if (which == "election.statistics") {

    #Kunnallisvaalit 2000, äänestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/010_kvaa_2000_2008-10-17_tau_101_fi.px"
    px <- read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Alue ~ Äänestystiedot")
    tab <- reshape::cast(df, kaava, value="dat")
    rownames(tab) <- as.character(tab$Alue)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(as.character(tab$Alue), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available

    # Parse municipality codes and names
    v <- ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "candidates") {

    #Ehdokkaat puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/020_kvaa_2000_2008-10-17_tau_102_fi.px"
    px <- read.px(url)
    df <- as.data.frame(px)
    tmp <- reshape::cast(df, Alue ~ Puolue ~ Ehdokastiedot, value="dat")

    tab1 <- tmp[,,"Ehdokkaiden lkm"]
    tab2 <- tmp[,,"Ehdokkaiden osuus (%)"]
    tab3 <- tmp[,,"Naisehdokkaiden lkm"]
    tab4 <- tmp[,,"Naisten osuus ehdokkaista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Ehdokkaiden lkm"]), "(Ehdokkaiden lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ehdokkaiden osuus (%)"]), "(Ehdokkaiden osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus ehdokkaista (%)"]), "(Naisten osuus ehdokkaista)")
    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.by.region") {

    #Valitut puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/030_kvaa_2000_2008-10-17_tau_103_fi.px"

    px <- read.px(url)
    df <- as.data.frame(px)
    tmp <- reshape::cast(df, Alue ~ Puolue ~ Valittujen.tiedot, value="dat")

    tab1 <- tmp[,,"Valittujen lkm"]
    tab2 <- tmp[,,"Valittujen osuus (%)"]
    tab3 <- tmp[,,"Valittujen naisten lkm"]
    tab4 <- tmp[,,"Naisten osuus valituista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valittujen lkm"]), "(Valittujen lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Valittujen osuus (%)"]), "(Valittujen osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen naisten lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus valituista (%)"]), "(Naisten osuus valituista)")
    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "parties") {

    #Kunnallisvaalit 2000, puolueiden kannatus
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/040_kvaa_2000_2008-10-17_tau_104_fi.px"
    px <- read.px(url)
    df <- as.data.frame(px)

    tmp <- reshape::cast(df, Alue ~ Puolue ~ Kannatustiedot, value="dat")

    tab1 <- tmp[,,"Ääniä yhtensä"]
    tab2 <- tmp[,,"Ennakkoäänet"]
    tab3 <- tmp[,,"Naisehdokkaiden äänimäärä"]
    tab4 <- tmp[,,"Naisehdokkaiden osuus äänistä (%)"]
    tab5 <- tmp[,,"Osuus äänistä (%)"]
    tab6 <- tmp[,,"Osuus ennakkoäänistä (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Ääniä yhtensä"]), "(Ääniä yhtensä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ennakkoäänet"]), "(Ennakkoäänet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden äänimäärä"]), "(Naisehdokkaiden äänimäärä)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisehdokkaiden osuus äänistä (%)"]), "(Naisehdokkaiden osuus äänistä (%))")
    colnames(tab5) <- paste(colnames(tmp[,,"Osuus äänistä (%)"]), "(Osuus äänistä (%))")
    colnames(tab6) <- paste(colnames(tmp[,,"Osuus ennakkoäänistä (%)"]), "(Osuus ennakkoäänistä (%))")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- ldply(strsplit(ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.all") {

    #Kunnallisvaalit 2000, valitut ehdokkaat
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/050_kvaa_2000_2008-10-17_tau_105_fi.px"

    px <- read.px(url)
    df <- as.data.frame(px)
    tab <- reshape::cast(df, Ehdokas ~ Ehdokastiedot, value="dat")

  } else if (which == "all.municipality.level.data") {

    tab1 <- GetMunicipalElectionData2000("election.statistics")
    tab2 <- GetMunicipalElectionData2000("candidates")
    tab3 <- GetMunicipalElectionData2000("selected.candidates.by.region")
    tab4 <- GetMunicipalElectionData2000("parties")

    municipalities <- sort(rownames(tab1))
    tab <- cbind(tab1[municipalities, ],
             tab2[municipalities, ],
      	     tab3[municipalities, ],
      	     tab4[municipalities, ])
  }

  tab

}
