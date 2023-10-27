## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  tidy.opts=list(width.cutoff=80), 
  tidy = 'formatR', 
  comment = "#>"
)

## ----warning=FALSE, message=FALSE, error=FALSE--------------------------------
library(ausplotsR)

## ----echo=FALSE---------------------------------------------------------------
oldpar <- par(no.readonly = TRUE)

## -----------------------------------------------------------------------------
#See ?get_ausplots to explore all data modules available

## -----------------------------------------------------------------------------
my.ausplots.data <- try(get_ausplots(veg.PI=TRUE, veg.vouchers=TRUE, bounding_box = c(125,140,-40,-10)))

## ----echo=FALSE---------------------------------------------------------------
if(class(my.ausplots.data) != "list") {
  message("Vignette aborted due to database connection issue.")
  knitr::knit_exit()
}


## -----------------------------------------------------------------------------
names(my.ausplots.data)

## -----------------------------------------------------------------------------
head(my.ausplots.data$site.info[,c("site_location_name", "site_unique", "longitude", "latitude", "bioregion_name")])

## -----------------------------------------------------------------------------
#count plot visits per Australian States:
summary(as.factor(my.ausplots.data$site.info$state))

## ----fig1, fig.height = 4, fig.width = 6--------------------------------------
#Sites are coded by IBRA bioregion by default. 
map_ausplots(my.ausplots.data)

## -----------------------------------------------------------------------------
head(subset(my.ausplots.data$veg.PI, !is.na(herbarium_determination)))

## ----warning=FALSE------------------------------------------------------------
sites100 <- my.ausplots.data$veg.PI[which(my.ausplots.data$veg.PI$site_unique  %in% sample(my.ausplots.data$site.info$site_unique, 100)), ]
my.fractional <- fractional_cover(sites100)

head(my.fractional)

## -----------------------------------------------------------------------------
my.fractional <- merge(my.fractional, my.ausplots.data$site.info, by="site_unique")[,c("site_unique", "bare", "brown", "green", "other", "longitude", "latitude")]

my.fractional <- na.omit(my.fractional)

head(my.fractional)

## ----fig.height = 4, fig.width = 6--------------------------------------------
plot(bare ~ latitude, data=my.fractional, pch=20, bty="l")

## ----fig.height = 4, fig.width = 6--------------------------------------------
my.fractional$quadratic <- my.fractional$latitude^2

LM <- lm(bare ~ latitude + quadratic, data=my.fractional)
summary(LM)

#generate predicted values for plotting:
MinMax <- c(min(my.fractional$latitude), max(my.fractional$latitude))
ND <- data.frame(latitude=seq(from=MinMax[1], to=MinMax[2], length.out=50), quadratic=seq(from=MinMax[1], to=MinMax[2], length.out=50)^2)
ND$predict <- predict(LM, newdata=ND)
#
plot(bare ~ latitude, data=my.fractional, pch=20, bty="n")
points(ND$latitude, ND$predict , type="l", lwd=2, col="darkblue")

## -----------------------------------------------------------------------------
#The species_table function below can also take the `$veg.voucher` module as input, but `m_kind="PA"` must be specified to get a sensible presence/absence output.

## -----------------------------------------------------------------------------
#The 'species_name' argument below specifies use of the "standardised_name" field to identify species, which is based on herbarium_determination names (i.e., "HD" option in species_name) matched to accepted scientific name according to a standard (APC: https://www.anbg.gov.au/cpbr/program/hc/hc-APC.html).

## -----------------------------------------------------------------------------
my.sppBYsites <- species_table(my.ausplots.data$veg.PI, m_kind="percent_cover", cover_type="PFC", species_name="SN")

#check the number of rows (plots) and columns (species) in the matrix
dim(my.sppBYsites)

#look at the top left corner (as the matrix is large)
my.sppBYsites[1:5, 1:5] 

## -----------------------------------------------------------------------------
rev(sort(colSums(my.sppBYsites)))[1:10]

## ----fig.height = 4, fig.width = 6--------------------------------------------
#Whittaker plots for some selected AusPlots with alternative relative abundance models fitted to the plant community data:
par(mfrow=c(2,2), mar=c(4,4,1,1))
for(i in c(1:4)) {
  plot(vegan::radfit(round(my.sppBYsites[9+i,], digits=0), log="xy"), pch=20, legend=FALSE, bty="l")
  legend("topright", legend=c("Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"), lwd=rep(1, 5), col=c("black", "red", "green", "blue", "cyan"), cex=0.7, bty="n")
}

## -----------------------------------------------------------------------------
#The species_list function is designed to take $veg.voucher as input but can also take $veg.PI

## -----------------------------------------------------------------------------
#print a list of genus_species-only records from selected plots (for demonstration we print only part):
species_list(my.ausplots.data$veg.vouch, grouping="by_site", species_name="GS")[1:2]

#overall species list ordered by family (for demonstration we print only part):
species_list(my.ausplots.data$veg.vouch, grouping="collapse", species_name="SN", append_family=TRUE)[1:50]

## ----echo=FALSE---------------------------------------------------------------
par(oldpar)

