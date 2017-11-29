#' Finds the density of ego networks in either a list of ego networks or sociocentric data.
#'
#' Finds the density of ego networks in either a list of ego networks or sociocentric data.
#' If a sociometric network is provided, the function will find the ego networks using \code{find_ego_networks}.
#'
#' @param networks Either an \emph{n} by \emph{n} adjacency matrix, a \code{igraph} object, \code{network} object, or list thereof.
#' @param type The type of object passed to the \code{network} argument. Either "igraph", "matrix", or "network".
#' @param mode Either "graph" or "digraph" depending on if the  network is undirected or directed.
#' @return A vector of network density scores for each node in the network.
#' @examples
#' require(igraph)
#' set.seed(6436)
#' g <- sample_gnp(20, 0.25)
#' ego_density(g, type = "igraph")
#'
#' require(sna)
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' ego_density(net, type = "network")
#'
#' require(sna)
#' set.seed(6436)
#' net <- rgraph(20, 1, 0.25)
#' mats <- find_ego_networks(nets, return = "matrix")
#' ego_density(mats, type = "matrix")
#'
#' \dontrun{
#'
#' # Will return error if class of "networks" does not match argument for type
#' require(sna)
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' ego_density(net, type = "igraph")
#' }

ego_density <- function(networks, type = "matrix", mode = "digraph") {
  if (missing(type)) {
    stop("Type must be specified--either \"matrix\", \"network\", or \"igraph\".\n", call. = FALSE)
  }
  if (class(networks) != "list" & class(networks) != type) {
    stop("Type must match the object passed to the \"networks\" object.\n", call. = FALSE)
  }
  if (class(networks) == "list" & !all(sapply(networks, class) == type)) {
    stop("Type must match the object passed to the \"networks\" object.\n", call. = FALSE)
  }
  if (class(networks) != "list") {
    networks <- find_ego_networks(networks, return = type)
  }
  if (type == "matrix") {
    edge_count <- sapply(networks, sum, na.rm = T)
    n <- sapply(networks, nrow)
    max_count <- switch(mode, digraph = n * (n - 1), graph = (n * (n - 1))/2)
    dens <- edge_count/max_count
    return(dens)
  }
  if (type == "igraph") {
    edge_count <- sapply(networks, ecount)
    n <- sapply(networks, vcount)
    max_count <- switch(mode, digraph = n * (n - 1), graph = (n * (n - 1))/2)
    dens <- edge_count/max_count
    return(dens)
  }
  if (type == "network") {
    edge_count <- sapply(networks, network.edgecount)
    n <- sapply(networks, network.size)
    max_count <- switch(mode, digraph = n * (n - 1), graph = (n * (n - 1))/2)
    dens <- edge_count/max_count
    return(dens)
  }
}
