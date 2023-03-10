% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{plot_radar}
\alias{plot_radar}
\title{create a radar plot}
\usage{
plot_radar(data, splitby, value, plot_height = "200px")
}
\arguments{
\item{value}{to An object of class "string", filter on a category in the split_by column}

\item{dataset}{An object of class "data.frame", the dataset to plot}

\item{split_by}{An object of class "string", name of the variable used in prepare_radar_data (must be categorical)}
}
\value{
Returns an object of class "plotly". the radar plot for the split_by category
}
\description{
create a radar plot using the data from prepare_radar_data()
grouping by the split_by var and filtering on value
It has an option 'no_split' in case the used want a general profil
}
\examples{
plot_radar(dataset, "gender", "M")
}
