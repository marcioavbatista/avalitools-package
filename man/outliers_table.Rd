\name{outliers_table}
\alias{outliers_table}
\title{Outliers Table.}
\description{Creates an data.frame with a table for checking outliers in the residuals.}
\usage{
outliers_table(modelo)
}
\arguments{
\item{modelo}{
modelo is an lm object.
}

}

\value{
A data.frame with a table for looking at the outliers of the model.
}
\examples{
out_table <- outliers_table(modelo)
}