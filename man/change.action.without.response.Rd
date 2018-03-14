\name{change.action.without.response}
\alias{change.action.without.response}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{changes kind of actions for all action without response}
\description{changes kind of actions for all action without response}
\usage{
change.action.without.response(data.set,action,response,newaction, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.set}{
  data.frame f.e imported from a data sheet containing\cr  
                                "Name","item.number"\cr 
                                "action.from.","action.to","kind.of.action"\cr
                                "name.of.action","action.number","classification","weighting"\cr   
  }                              
  \item{action}{
  action normally with response
  }
  \item{response}{
  the normal response to the action 
  }
  \item{newaction}{
  data.frame("name.of.action"="test","action.number"=1,"classification"=2,"weighting"=3)')
  }

 \item{...}{
  workbook :  the XlConnect Workbook for the Excel file to be changed\cr
  note: The workbook must be opened before \cr
   \cr 
   sheet: the sheet name (some random numbers will be added to be sure not to delete any data\cr
   \cr 

}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
change.action.without.response returns the new data.set\cr
}
\author{
 Knut Krueger
}
%%\note{
%%  ~~further notes~~
%%}
%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{

data(data_ADI)
new_data=change.action.without.response(data_ADI,3,9,
          newaction=data.frame("name.of.action"="test",
          "action.number"=10,"classification"=2,"weighting"=3))
new_data
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
