% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_range.R
\name{network_range}
\alias{network_range}
\title{Network range (diversity) scores for nodes in a network}
\usage{
network_range(network, attr, directed = TRUE)
}
\arguments{
\item{network}{Either an \emph{n} by \emph{n} adjacency matrix, a \code{igraph} object, or \code{network} object.}

\item{attr}{A vector of node attributes indicating group membership.}

\item{directed}{Boolean indicating if the network is directed or undirected.}
}
\value{
A vector of range scores for nodes in network.
}
\description{
Network range (diversity) scores for nodes in a network
}
\examples{
# A sample vector of group membership
set.seed(385)
grp <- sample(1:3, 20, replace = TRUE)
# A sample network of the same order (20)
require(sna)
net <- rgraph(20, 1, 0.2)
range(net, grp)
}
