\name{as_avaliation_anova}
\alias{as_avaliation_anova}
\title{As avaliation anova}
\description{Transform standard anova object into a avaliation anova table.}
\usage{
as_avaliation_anova(anova)
}
\arguments{
\item{anova}{
anova is an anova object
}

}

\value{
An anova table for avaliations.
}
\examples{

knitr::kable(as_avaliation_anova(anova), type = "pipe")
}