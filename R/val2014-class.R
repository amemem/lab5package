val2014 = setRefClass(
  "val2014",
  fields = list(
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
      val = read.csv2("https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.skv")
      stopifnot(
        length(val) == 32,
        length(rownames(val)) == 290,
        colnames(val) == c(
          "LAN",
          "KOM",
          "LÄN",
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
      return(.self$field(county))
    }
  )
)
