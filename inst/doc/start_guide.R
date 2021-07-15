## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  tidy.opts=list(width.cutoff=80), 
  tidy = 'formatR', 
  comment = "#>"
)

## ---- warning=FALSE, message=FALSE, error=FALSE-------------------------------
library(ausplotsR)

## ---- echo=FALSE--------------------------------------------------------------
oldpar <- par(no.readonly = TRUE)

## -----------------------------------------------------------------------------
#See ?get_ausplots to explore all data modules available

## -----------------------------------------------------------------------------
my.ausplots.data <- try(get_ausplots(bounding_box = c(125, 140, -40, -10)))

## ---- echo=FALSE--------------------------------------------------------------
if(class(my.ausplots.data) != "list") {
  message("Vignette aborted due to database connection issue.")
  knitr::knit_exit()
}


