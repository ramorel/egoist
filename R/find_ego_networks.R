#' Fleixbly finds ego networks from sociocentric data in several formats.
#'
#' @param network Either an \emph{n} by \emph{n} adjacency matrix, a \code{igraph} object, or \code{network} object.
#' @param order Either 0 (ego network contains only those edges that include the ego) or 1 (the ego's first step neighborhood).
#' @param type (Optional) The type of object passed to the \code{network} argument. Either "igraph", "matrix", or "network". If Not provided, will determine type from network provided.
#' @param return Determines what object type the function should return. Either "igraph", "matrix", or "network".
#' @return A list (of either \code{igraph} objects, \code{network} objects, or adjacency matrices) containing the ego networks for each node in the network`
#' @examples
#' require(igraph)
#' set.seed(6436)
#' g <- sample_gnp(20, 0.25)
#' find_ego_network(g, type = "igraph", order = 1, return = "igraph")
#'
#' require(sna)
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' # Accepts `network` objects, returns list of `igraph` objects.
#' find_ego_network(net, type = "network", order = 1, return = "igraph")

find_ego_networks <- function(network, order = 1, type = NULL, return = "matrix") {
  if (class(network) %in% c("igraph", "network", "matrix") == FALSE) {
    stop("Requires an igraph object, network object, or adjacency matrix.\n", .call = FALSE)
  }
  if ((class(network) == "matrix") && (nrow(network) != ncol(network))) {
    stop("Matrix must be an n by n adjacency matrix.\n")
  }
  order <- order
  if (is.null(type)) {
    type <- class(network)
  }
  if (type == "igraph") {
    require(igraph)
    ego_graphs <- make_ego_graph(network, 1, nodes = V(network))
    ego_mats <- lapply(ego_graphs, as_adjacency_matrix, sparse = F)
    names(ego_mats) <- V(network)$name
    if (order == 0) {
      for (i in 1:length(ego_mats)) {
        n <- which(colnames(ego_mats[[i]]) == V(network)$name[i])
        ego_mats[[i]] <- ego_mats[[i]][c(n, (1:nrow(ego_mats[[i]]))[-n]), c(n,
                                                                            (1:nrow(ego_mats[[i]]))[-n])]
        ego_mats[[i]][2:nrow(ego_mats[[i]]), 2:ncol(ego_mats[[i]])] <- 0
      }
      names(ego_mats) <- V(network)$name
    }
  }
  if (type == "network") {
    require(sna)
    if (order == 0) {
      ego_mats <- ego.extract(network, neighborhood = "combined")
      for (i in 1:length(ego_mats)) {
        ego_mats[[i]][2:nrow(ego_mats[[i]]), 2:ncol(ego_mats[[i]])] <- 0
      }
    } else {
      ego_mats <- ego.extract(network, neighborhood = "combined")
    }
  }
  if (type == "matrix") {
    if (is.null(rownames(network))) {
      rownames(network) <- 1:nrow(network)
    }
    if (is.null(colnames(network))) {
      colnames(network) <- 1:ncol(network)
    }
    enames <- rownames(network)
    ego_list <- lapply(1:nrow(network), function(x) {
      c(enames[x], enames[which(network[x, ] == 1 | network[, x] == 1)])
    })
    ego_mats <- list()
    for (i in 1:length(ego_list)) {
      ego_mats[[i]] <- network[which(enames %in% ego_list[[i]]), which(enames %in%
                                                                         ego_list[[i]])]
      names(ego_mats)[i] <- enames[i]
      n <- which(colnames(ego_mats[[i]]) == names(ego_mats)[i])
      ego_mats[[i]] <- ego_mats[[i]][c(n, (1:nrow(ego_mats[[i]]))[-n]), c(n, (1:nrow(ego_mats[[i]]))[-n])]
    }
    if (order == 0) {
      for (i in 1:length(ego_mats)) {
        ego_mats[[i]][2:nrow(ego_mats[[i]]), 2:ncol(ego_mats[[i]])] <- 0
      }
    }
  }
  if (return == "igraph") {
    ego_igraph <- lapply(ego_mats, graph_from_adjacency_matrix)
    return(ego_igraph)
  }
  if (return == "network") {
    ego_net <- lapply(ego_mats, as.network)
    return(ego_net)
  }
  if (return == "matrix") {
    return(ego_mats)
  }
}
