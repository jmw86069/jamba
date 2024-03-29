% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-string.r
\name{igrepl}
\alias{igrepl}
\title{case-insensitive logical grepl}
\usage{
igrepl(..., ignore.case = TRUE)
}
\arguments{
\item{..., ignore.case}{parameters sent to \code{base::grep()}}
}
\value{
\code{logical} vector indicating pattern match
}
\description{
case-insensitive logical grepl
}
\details{
This function is a simple wrapper around \code{base::grepl()} which
runs in case-insensitive mode simply by adding default \code{ignore.case=TRUE}.
It is mainly used for convenience.
}
\examples{
V <- paste0(LETTERS[1:5], LETTERS[4:8]);
ig1 <- grepl("D", V);
ig2 <- igrepl("D", V);
ig3 <- grepl("d", V);
ig4 <- igrepl("d", V);
data.frame(V,
   grepl_D=ig1,
   grepl_d=ig3,
   igrepl_D=ig2,
   igrepl_d=ig4);

}
\seealso{
Other jam grep functions: 
\code{\link{grepls}()},
\code{\link{igrepHas}()},
\code{\link{igrep}()},
\code{\link{provigrep}()},
\code{\link{unigrep}()},
\code{\link{unvigrep}()},
\code{\link{vgrep}()},
\code{\link{vigrep}()}
}
\concept{jam grep functions}
