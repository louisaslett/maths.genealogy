% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_genealogy.R
\name{get_genealogy}
\alias{get_genealogy}
\title{Retrieve genealogy tree by mathematician ID}
\usage{
get_genealogy(id, ancestors = TRUE, descendants = TRUE)
}
\arguments{
\item{id}{\code{integer} vector of IDs of mathematicians for whom the genealogy should be retrieved}

\item{ancestors}{\code{logical} indicating whether to include the genealogy backward to include all ancestors, defaults to \code{TRUE}.
This can be a single \code{logical(1)} which then applies to all mathematicians referenced in the \code{id} argument, or it can be a vector of the same length as \code{id} providing different selection for each individual.}

\item{descendants}{\code{logical} indicating whether to include the genealogy forward to include all descendants, defaults to \code{TRUE}.
This can be a single \code{logical(1)} which then applies to all mathematicians referenced in the \code{id} argument, or it can be a vector of the same length as \code{id} providing different selection for each individual.}
}
\value{
A list object of class \code{genealogy}.
Each element of the list represents a mathematician in the genealogical tree.
The name of the element is the mathematician's ID in the \href{https://mathgenealogy.org/}{Mathematics Genealogy Project}.
Each element of the object is list with containing:
\describe{
\item{\code{id}}{\code{integer(1)} with Mathematician's ID;}
\item{\code{name}}{\code{character(1)} containing the full name of the mathematician;}
\item{\code{institution}}{\code{character(1)} containing the institution at which PhD was obtained;}
\item{\code{year}}{\code{integer(1)} with the year their PhD was completed;}
\item{\code{descendants}}{\code{integer} vector of IDs of any mathematicians who were supervised by this individual for their PhD;}
\item{\code{advisors}}{\code{integer} vector of IDs of any mathematicians who were supervisors of this individual for their PhD.}
}

In addition, there is an attribute named \code{start_nodes} which contains an \code{integer} vector of IDs indicating the origin nodes used in the genealogical tree search that produced this object.
In other words, the \code{id} argument as passed to this function.
}
\description{
Queries the genealogy of a single or set of mathematicians by their ID in the \href{https://mathgenealogy.org/}{Mathematics Genealogy Project}.
}
\examples{
\dontshow{if (interactive() && curl::has_internet()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# First, you need to use search_id() to find the mathematician ID for the
# individual(s) you wish to plot, or visit https://mathgenealogy.org/ to look
# up in the browser. Once you have these IDs the get_genealogy() function will
# retrieve the genealogical tree.

# For example, to find the package author would search for themselves using
search_id("Aslett", "Louis")

# Then, use the id to retrieve the genealogy
g <- get_genealogy(171971)

# With that genealogy, you can then plot using plot_grviz() or other plotting
# functions.
\dontshow{\}) # examplesIf}
}
\references{
Alber, D. (2024). “'geneagrapher-core' package”, \url{https://github.com/davidalber/geneagrapher-core}

Jackson, A. (2007). “A Labor of Love: The Mathematics Genealogy Project”, \emph{Notices of the AMS}, \strong{54}(8), 1002-1003. \url{https://www.ams.org/notices/200708/tx070801002p.pdf}

Mulcahy, C. (2017). “The Mathematics Genealogy Project Comes of Age at Twenty-one”, \emph{Notices of the AMS}, \strong{64}(5), 466-470. \url{https://www.ams.org/journals/notices/201705/rnoti-p466.pdf}
}
