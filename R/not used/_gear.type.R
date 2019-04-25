#' Converts numeric gear codes to gear type
#' 
#' Uses the gear codes from an observR extraction and returns a vector
#'   of gear names combining those codes
#' 
#' @param df A dataframe with a column called 'gear' containing codes
#' @param twl "S" or "B". "S" separates bottom trawl from midwater trawl.
#'             "B" combines bottom trawl and midwater trawl.
#' @param fg "S" or "B". "S" separates hook & line from pot.
#'             "B" combines hook & line and pot.

#' @author Jason Jannot and Allan Hicks
#' @export
#' @seealso \code{\link{}}, 
#' @keywords gear
#EXAMPLE
# ob$gear <- gear.type(ob, "S", "S")
# "Bottom and Midwater trawls are separate"
# "Hook & line and pot gears are separate"
# unique(ob$gear)
# "Bottom Trawl"   "Pot"            "Hook & Line"    "Midwater Trawl"
# "Shrimp Trawl"

gear.type <- function(df, twl="S", fg="S"){
  #thse are set Gear codes defined by JJ
  btm.twl<-c(1,2,4,5,17)
  mid.twl<-3
  all.twl<-c(btm.twl,mid.twl)
  shmp.twl<-c(11:13)
  hl<-c(6:9,15,19,20)
  pot<-c(10,18)
  fixed.gear<-c(hl,pot)

  x <-rep(NA, nrow(df))
  if(twl=="S"){
    message("Bottom and Midwater trawls are separate")
    x[df$gear %in% btm.twl] <- "Bottom Trawl"
    x[df$gear %in% mid.twl] <- "Midwater Trawl"
  }else{
    message("Bottom and Midwater trawls are combined")
    x[df$gear %in% all.twl] <- "Bottom & Midwater Trawl"
  }
  if(fg=="S"){
    message("Hook & Line and Pot gears are separate")
    x[df$gear %in% hl] <- "Hook & Line"
    x[df$gear %in% pot] <- "Pot"
  }else{
    message("Hook & Line and Pot gears are combined")
    x[df$gear %in% fixed.gear] <- "Fixed gears"
  }

  x[df$gear %in% shmp.twl] <- "Shrimp Trawl"
  return(x)
}
