\name{P1}
\alias{P1}
\docType{data}
\title{
An example time series for the EmiStatR package
}
\description{
This dataset is a list that contains a data.frame with two columns: Time [y-m-d h:m:s] and Precipitation P [mm]. 
The station of measuring, Dahl, is located close to the catchment of the combined sewer overflow tank at Goesdorf, Grand-Duchy of Luxembourg.
}
\usage{data("P1")}
\format{
  A data frame with 4464 observations on the following 2 variables.
  \describe{
    \item{\code{time}}{a POSIXct}
    \item{\samp{P [mm]}}{a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
\url{http://agrimeteo.lu/}
}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data("P1")

plot(P1[,1], P1[,2], col="blue", typ="l", xlab = "time", ylab = "Precipitation [mm]") 

}
\keyword{datasets}
