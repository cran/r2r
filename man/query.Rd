% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/hashmap.R, R/hashset.R
\name{query}
\alias{query}
\alias{query.r2r_hashmap}
\alias{query.r2r_hashset}
\title{Query keys from an hash table.}
\usage{
query(x, key)

\method{query}{r2r_hashmap}(x, key)

\method{query}{r2r_hashset}(x, key)
}
\arguments{
\item{x}{an \code{hashset} or \code{hashmap}.}

\item{key}{an arbitrary R object. Key to be queried from the hash table.}
}
\value{
\code{TRUE} or \code{FALSE}, for \code{hashset}s. For
\code{hashmap}s, if the queried key exists in the hash table, returns the
associated value (an a priori arbitrary R object); otherwise, behaves as
specified by \code{\link{on_missing_key}(x)}
(see also \link[r2r]{hashtable}).
}
\description{
These generics are used for querying a single key from an
\code{hashset} or \code{hashmap}, respectively. For vectorized queries,
see the \link[r2r]{subsetting_hashtables} documentation page.
}
\examples{
s <- hashset(1, 2, 3)
query(s, 3)
}
\author{
Valerio Gherardi
}
