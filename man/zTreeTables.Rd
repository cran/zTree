\name{zTreeTables}
\alias{zTreeTables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read '.xls' files generated by 'z-Tree' into R}
\description{
Read '.xls' files which are written by the Microsoft Windows program
'z-Tree'. The latter is a software for developing and carrying out
economic experiments (see <http://www.ztree.uzh.ch/> for more information).}
\usage{
zTreeTables(files, tables = c("globals", "subjects"), sep = "\\t",
	zTree.silent=getOption("zTree.silent"),
	zTree.encoding=getOption("zTree.encoding"),ignore.errors=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{a vector of 'z-Tree' (xls-) filenames.}
  \item{tables}{a vector of table names. If you leave this empty, globals and subjects will be read. If you set tables=NULL, all tables will be read.}
  \item{sep}{Field separator. 'z-Tree' uses "\\t" as a separator. If you have a manipulated file, you
    might need to change the separator.}
  \item{zTree.silent}{If \code{TRUE}, generates less verbose output.}
  \item{zTree.encoding}{zTree writes text files and seems to use latin1
    (windows-1252) encoding for these files.}
  \item{ignore.errors}{Usually, \code{zTreeTables} stops with an error when
    it detects a wrong encoding or a manipulated file.
    With \code{ignore.errors=TRUE} it only generates a warning.}
}
\value{
  A list of all tables (e.g. "globals", "subjects", etc.)
}
\references{Urs Fischbacher. "z-Tree: Zurich Toolbox for Ready-made Economic Experiments."
  Experimental Economics (2) 171-178, 2007.
}
\author{Oliver Kirchkamp}
\seealso{See also \code{zTreeSbj}}
\examples{
\dontrun{
zTT <- zTreeTables(c("080712_1412.xls","080712_1818.xls"))
with(zTT$subjects, table(Treatment,Period))
To read all tables from all sessions in directories below the current one:
sessions<-list.files(".","[0-9]{6}_[0-9]{4}.xls",recursive=TRUE)
zTT<-zTreeTables(sessions,tables=NULL)
}}
\keyword{data}
\keyword{manip}
