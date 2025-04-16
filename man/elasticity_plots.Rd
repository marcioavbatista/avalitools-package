\name{elasticity_plots}
\alias{elasticity_plots}
\title{Elasticity plots.}
\description{Returns a list with the elasticity plots of the model.}
\usage{
elasticity_plots(df, modelo, trans, vari)
}
\arguments{
\item{df}{
df is a data.frame with the transformed variables represented in the columns.
}

\item{modelo}{
modelo is an lm model object of the avaliation model.
}

\item{trans}{
trans is an string vector with the respective transformations used atthe model
}

\item{vari}{
vari is an string vector with the names of the variables used at the model.

}

}

\value{
A list with the elasticity plots for each independent variable of the model.
}
\examples{
lista <- elasticity_plots(df, modelo, trans, vari)
# prints every elasticity plots 
for (i in 1:(ncol(df)-1)) {
lista[[i]]

}
}