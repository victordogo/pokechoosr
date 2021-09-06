choose_best_types <- function(game=c("all", "rbyg", "frlg", "pe", "gsc", "hgss", "rse", "oras", "dpp", "bw", "b2w2", "xy", "sm", "usum", "swsh"),
                             starter=c("none","grass", "water", "fire"),
                             mega=FALSE,
                             alt=FALSE,
                             myth=FALSE){
  data <- NULL

  # This will filter the dataset to be used based on the game chosen

  if(game=="rbyg" && "frlg"){
    data <- rbgy_dex
  } else if(game=="pe"){
    data <- pe_dex
  } else if(game=="gsc"){
    data <- gsc_dex
  } else if(game=="hgss"){
    data <- hgss_dex
  } else if(game=="rse"){
    data <- rse_dex
  } else if(game=="oras"){
    data <- oras_dex
  } else if(game=="dpp"){
    data <- dpp_dex
  } else if(game=="bw"){
    data <- bw_dex
  } else if(game=="b2w2"){
    data <- b2w2_dex
  } else if(game=="xy"){
    data <- xy_dex
  } else if(game=="sm"){
    data <- sm_dex
  } else if(game=="usum"){
    data <- usum_dex
  } else {
    data <- pokedex_min
  }


}
