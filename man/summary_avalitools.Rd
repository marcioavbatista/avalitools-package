\name{summary_avalitools}
\alias{summary_avalitools}
\title{Summary Avalitools.}
\description{Creates an data.frame with a summary about the given linear model}
\usage{
summary_avalitools(modelo)
}
\arguments{

\item{modelo}{
modelo is an lm object.
}

}

\value{
A data.frame with summary statistics about the given model.
}
\examples{
summary_df <- summary_avalitools(modelo)
}