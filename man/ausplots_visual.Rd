\name{ausplots_visual}
\alias{ausplots_visual}
\alias{map_ausplots}
\title{Graphical illustration of sample TERN AusPlots data
}
\description{Generates a set of graphical displays representing TERN AusPlots data based on geographic locations and vegetation attributes from the plot-based point intercepts (as generated by \code{\link{get_ausplots}} and other pre-processing functions).
}
\usage{
ausplots_visual(my.ausplots.object = NULL, map = TRUE, map.attribute = TRUE, 
fraction.pie = TRUE, growthform.pie = TRUE, cumulative.cover = TRUE, 
whittaker = TRUE, outfile=NULL, max.plots = 5)

map_ausplots(my.ausplots.object)
}
\arguments{
  \item{my.ausplots.object}{Optional (but required for \code{map_ausplots}) input raw data object as generated in a call from the \code{\link{get_ausplots}} function. The default graphical outputs require $site.info (for locations) and $veg.PI (for vegetation attributes) to be included in the AusPlots data extract.
}
  \item{map}{Logical, whether to include a basic map of site locations in the context of Australia.
}
  \item{map.attribute}{Logical, whether to include a map depicting site locations in Australia coded by cover of trees over 5 m high.
}
\item{fraction.pie}{Logical, whether to include pie charts depicting \code{\link{fractional_cover}}.
}
\item{growthform.pie}{Logical, whether to include pie charts depicting relative cover of plant growth forms.
}
\item{cumulative.cover}{Logical, whether to include graphs of cumulative cover estimates by species as point intercepts are taken across each AusPlot.
}
\item{whittaker}{Logical, whether to include Whittaker plots depicting  species abundance distributions.
}
\item{outfile}{Optional file path and name to which the output pdf file is written.
}
\item{max.plots}{Maximum number of sites for which to display individual graphics panels. By default, \code{max.plots} sites are selected randomly. If you provide existing AusPlots data, \code{max.plots} sites are processed (in order) whenever \code{fractional.pie} or \code{cumulative.cover} are selected, because those calculations can be slow across a large number of plots. 
}
}
\details{By default, extracts sample data, allowing the user to explore the dataset in a single call, or to easily visualise an extracted AusPlots dataset. Included in the default demonstration plots are maps of plot locations with and without coding by tree cover, and then a set of four panels for a smaller sample of AusPlots, depicting data from the main vegetation tables in the package, i.e., fractional cover, growth form cover, species cover and species relative abundance. This function is intended as a simple demonstration of the data available. Refer to TERN tutorials for many other ways to visualise and explore AusPlots.
}
\value{Plots are written to a pdf file to ensure consistent formatting, saved by default in the working directory.
}
\references{Blanco-Martin, B. (2019) Tutorial: Understanding and using the 'ausplotsR' package and AusPlots data. Terrestrial Ecology Research Network. Version 2019.04.0, April 2019.
https://github.com/ternaustralia/TERN-Data-Skills/

Guerin, G.R., Sparrow, B., Tokmakoff, A., Smyth, A., Leitch, E., Baruch, Z., Lowe, A.J. (2017) Opportunities for integrated ecological analysis across inland Australia with standardised data from AusPlots Rangelands. \emph{PLoS ONE} \strong{12}, e0170137.
	
Whittaker, R.H. (1965) Dominance and diversity in land plant communities: numerical relations of species express the importance of competition in community function and evolution. \emph{Science}, \strong{147}, 250-260.
}
\author{Greg Guerin, Bernardo Blanco-Martin
}

\seealso{\code{\link{get_ausplots}}   
	\cr
	\cr
	\code{\link{fractional_cover}}   
	\cr
	\cr
	\code{\link{growth_form_table}}   
	\cr
	\cr
	\code{\link{single_cover_value}}   
	\cr
	\cr
	\code{\link{species_table}}
	\cr
	\cr
	\code{\link{ausplots_trajectory}}
}
\examples{
\dontrun{
#Default output
#Plot locations; visualisations for 5 random plots (including revisits):
ausplots_visual()

#Map tree cover only for all sites:

#Extract data first:
my.data <- get_ausplots(veg.PI=TRUE)

#Set maximum number of plots to all available in the veg.PI data:
ausplots_visual(my.data, map = FALSE, map.attribute = TRUE, 
fraction.pie = FALSE, growthform.pie = FALSE, cumulative.cover = FALSE, 
whittaker = FALSE, outfile = "AusPlots_treeCover.pdf", 
max.plots=length(unique(my.data$veg.PI$site_location_name))) 
}
}
