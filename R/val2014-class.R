#' @title The \code{val2014} Reference Class
#'
#' @description A Reference Class to calculate the 2014 Swedish election results for all counties. It calls the Valmyndigheten API, reads the semi-colon separated values file, extracts all the votes for each party in every municipality, and calculates the percentage of votes for each political party in every county. These results are stored in the respective county name field.
#'
#' @field Blekinge Blekinge county
#' @field Dalarna Dalarna county
#' @field Gotland Gotland county
#' @field Gavleborg Gavleborg county
#' @field Halland Halland county
#' @field Jamtland Jamtland county
#' @field Jonkoping Jonkoping county
#' @field Kalmar Kalmar county
#' @field Kronoberg Kronoberg county
#' @field Norrbotten Norrbotten county
#' @field Skane Skane county
#' @field Stockholm Stockholm county
#' @field Sodermanland Sodermanland county
#' @field Uppsala Uppsala county
#' @field Varmland Varmland county
#' @field Vasterbotten Vasterbotten county
#' @field Vasternorrland Vasternorrland county
#' @field Vastmanland Vastmanland county
#' @field vastergotland Vastra Gotaland county
#' @field Orebro Orebro county
#' @field Ostergotland Ostergotland county
#'
#' @return A \code{val2014} class generator object.
#' @export val2014
#' @name val2014
#'
#' @examples
#' val = val2014()
#' print(val$county("Blekinge"))
val2014 = setRefClass(
  "val2014",
  fields = list(
    County = "vector",
    Blekinge = "vector",
    Dalarna = "vector",
    Gotland = "vector",
    Gavleborg = "vector",
    Halland = "vector",
    Jamtland = "vector",
    Jonkoping = "vector",
    Kalmar = "vector",
    Kronoberg = "vector",
    Norrbotten = "vector",
    Skane = "vector",
    Stockholm = "vector",
    Sodermanland = "vector",
    Uppsala = "vector",
    Varmland = "vector",
    Vasterbotten = "vector",
    Vasternorrland = "vector",
    Vastmanland = "vector",
    vastergotland = "vector",
    Orebro = "vector",
    Ostergotland = "vector"
  ),
  methods = list(
    initialize = function() {
      "Calls the Valmyndigheten API to get the 2014 election results of all municipality, then calculating all the election results for every county and storing them in this object."
      val = read.csv2("https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.skv")
      stopifnot(
        length(val) == 32,
        length(rownames(val)) == 290,
        colnames(val) == c(
          "LAN",
          "KOM",
          "L\u00C4N",
          "KOMMUN",
          "M.tal",
          "M.proc",
          "C.tal",
          "C.proc",
          "FP.tal",
          "FP.proc",
          "KD.tal",
          "KD.proc",
          "S.tal",
          "S.proc",
          "V.tal",
          "V.proc",
          "MP.tal",
          "MP.proc",
          "SD.tal",
          "SD.proc",
          "FI.tal",
          "FI.proc",
          "OVR.tal",
          "OVR.proc",
          "BL.tal",
          "BL.proc",
          "OG.tal",
          "OG.proc",
          "Rost.Giltiga",
          "Rostande",
          "Rostb",
          "VDT"
        )
      )
      lan = unique(val$LAN)
      names(lan) = c(
        "Blekinge",
        "Dalarna",
        "Gotland",
        "Gavleborg",
        "Halland",
        "Jamtland",
        "Jonkoping",
        "Kalmar",
        "Kronoberg",
        "Norrbotten",
        "Skane",
        "Stockholm",
        "Sodermanland",
        "Uppsala",
        "Varmland",
        "Vasterbotten",
        "Vasternorrland",
        "Vastmanland",
        "vastergotland",
        "Orebro",
        "Ostergotland"
      )
      .self$County = names(lan)
      for (i in lan) {
        tmp_df = val[val$LAN == i,]
        votes = sum(tmp_df$Rostande)
        tmp_vc = c(
          (sum(tmp_df$M.tal) / votes) * 100,
          (sum(tmp_df$C.tal) / votes) * 100,
          (sum(tmp_df$FP.tal) / votes) * 100,
          (sum(tmp_df$KD.tal) / votes) * 100,
          (sum(tmp_df$S.tal) / votes) * 100,
          (sum(tmp_df$V.tal) / votes) * 100,
          (sum(tmp_df$MP.tal) / votes) * 100,
          (sum(tmp_df$SD.tal) / votes) * 100,
          (sum(tmp_df$FI.tal) / votes) * 100,
          (sum(tmp_df$OVR.tal) / votes) * 100
        )
        names(tmp_vc) = c(
          "M", "C", "FP", "KD", "S", "V", "MP", "SD", "FI", "OVR"
        )
        tmp_ln = names(lan)[match(i, lan)]
        .self$field(tmp_ln, tmp_vc)
      }
    },
    county = function(county) {
      "Returns a named vector with percent values of all political parties for a given county."
      stopifnot(is.character(county), county %in% County)
      return(.self$field(county))
    }
  )
)
