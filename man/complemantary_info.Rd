\name{complemantary_info}
\alias{complemantary_info}
\title{Compemantary info.}
\description{Returns a data.frame with complemantary info about the avaliation.}
\usage{
complementary_info(num_v, num_vu, num_d, num_du)
}
\arguments{
\item{num_v}{
num_v is an numeric that represants the number of numeric variables in the research.
}

\item{num_vu}{
num_vu is an numeric that represants the number of variables used in the model.
}

\item{num_d}{
num_d is the number of samples of the research.
}

\item{num_du}{
num_du is the number of samples used in the model.
}


}

\value{
A data frame with complementary info about an avaliation model.
}
\examples{
df_complementary <- complementary_info(num_v, num_vu, num_d, num_du)
}