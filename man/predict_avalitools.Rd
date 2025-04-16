\name{predict_avalitools}
\alias{redict_avalitools}
\title{Predict for the avaliation model.}
\description{Creates an data.frame with a predict table.}
\usage{
predict_avalitools(df_novo, modelo, trans)
}
\arguments{
\item{df_novo}{
df_novo is an data.frame with the new observations for the independent variables.
}

\item{modelo}{
modelo is an lm object.
}

\item{trans}{
trans is an string object that represents the transformations for all the variables.
}

}

\value{
A data.frame with a table with the predictions for the given values for the independent variables.
}
\examples{
estimates <- predict_avalitools(df_novo, modelo, trans)
}