#' Create subgroup networks containing only within group ties
#'
#' @param network Either an \emph{n} by \emph{n} adjacency matrix, a \code{igraph} object, or \code{network} object.
#' @param grouping_variable Either a vector of node attributes indicating group membership or a data frame with vertex names in column 1 and group in column 2
#' @param type (Optional) The type of object passed to the \code{network} argument. Either "igraph", "matrix", or "network". If Not provided, will determine type from network provided.
#' @param return Determines what object type the function should return. Either "igraph", "matrix", or "network".
#' @return A list of two lists. List 1 contains all within-group networks. List 2 contain all between group networks.
#' @examples
#' # A sample vector of group membership
#' set.seed(385)
#' grp <- sample(1:3, 20, replace = TRUE)
#' require(sna)
#' set.seed(5)
#' net <- rgraph(20, 1, 0.2)
#'
#' # Enter a \code{network} object and return \code{igraph} objects
#' n <- network(net)
#' find_group_networks(n, grp, return = "igraph")
#'
#' # Enter a \code{igraph} object and return \code{network} objects
#' g <- graph_from_adjacency_matrix(net)
#' find_group_networks(g, grp, return = "network")
#'
#' # Enter a \code{matrix} object and return \code{igraph} objects
#' find_group_networks(net, grp, return = "network")
#'
#' # Default settings
#' find_group_networks(g, grp)
#' find_group_networks(net, grp)
#' find_group_networks(n, grp)


find_group_networks <- function(network, grouping_variable,
                                type = NULL, return = "matrix") {
  if (class(network) %in% c("igraph", "network", "matrix") == FALSE) {
    stop("Requires an igraph object, network object, or adjacency matrix.\n", .call = FALSE)
  }
  if ((class(network) == "matrix") && (nrow(network) != ncol(network))) {
    stop("Matrix must be an n by n adjacency matrix.\n")
  }
  if (is.null(type)) {
    type <- class(network)
  }
  if (type == "network" | class(network) == "network") {
    require(intergraph)
    if (!"vertex.names" %in% network::list.vertex.attributes(network)){
      network %v% "vertex.names" <- seq(network::network.size(network))
    }
    net <- asIgraph(network)
    V(net)$name <- V(net)$vertex.names
  } else {
    if (type == "matrix") {
      net <- graph_from_adjacency_matrix(network)
    } else {
      net <- network
    }
  }
  if (is.null(V(net)$name)) {
    V(net)$name <- 1:length(V(net))
  }
  if (class(grouping_variable) == "data.frame") {
    group <- grouping_variable
    names(group) <- c("name", "group")
  } else {
    group <- data.frame(name = V(net)$name,
                        group = grouping_variable,
                        stringsAsFactors = F)
  }

  require(dplyr)
  el <- as_edgelist(net, names = T) %>%
    as.data.frame(stringsAsFactors = F) %>%
    rename(ego = V1, alter = V2) %>%
    mutate(ego_group = group$group[match(ego, group$name)],
           alter_group = group$group[match(alter, group$name)]) %>%
    mutate(same_group = if_else(ego_group == alter_group, 1, 0))
  within_group <- el %>%
    filter(same_group == 1)
  between_group <- el %>%
    filter(same_group == 0)

  no_within <- group$name[!group$name %in% sort(unique(c(within_group$ego,
                                                         within_group$alter)))]
  no_between <- group$name[!group$name %in% sort(unique(c(between_group$ego,
                                                          between_group$alter)))]

  require(purrr)
  within_group %>%
    split(.$ego_group) %>%
    map(`[`, c("ego", "alter")) %>%
    map(~ graph_from_data_frame(.x)) -> within_group_networks
  no_within %>%
    map(~ make_empty_graph() %>%
          add_vertices(1) %>%
          set_vertex_attr(., "name", value = .x)) -> no_within_networks

  names(no_within_networks) <- no_within

  within_group_networks <- c(within_group_networks, no_within_networks)

  between_group %>%
    split(.$ego_group) %>%
    map(`[`, c("ego", "alter")) %>%
    map(~ graph_from_data_frame(.x)) -> between_group_networks
  no_between %>%
    map(~ make_empty_graph() %>%
          add_vertices(1) %>%
          set_vertex_attr(., "name", value = .x)) ->  no_between_networks

  names(no_between_networks) <- no_between

  between_group_networks <- c(between_group_networks, no_between_networks)

  between_group_networks <- between_group_networks %>%
    map(~ set_vertex_attr(.x, "sector", value = group$group[match(V(.x)$name, group$name)]))

  if (return == "igraph") {
    list_networks <- list(within_group_networks, between_group_networks)
    names(list_networks) <- c("within_group_networks", "between_group_networks")
    return(list_networks)
  }
  if (return == "network") {
    within_group_networks %>%
      map(~ intergraph::asNetwork(.x)) -> within_group_networks
    between_group_networks %>%
      map(~ intergraph::asNetwork(.x)) -> between_group_networks
    list_networks <- list(within_group_networks, between_group_networks)
    names(list_networks) <- c("within_group_networks", "between_group_networks")
    return(list_networks)
  }
  if (return == "matrix") {
    within_group_networks %>%
      map(~ as_adjacency_matrix(.x, sparse = F)) -> within_group_networks
    between_group_networks %>%
      map(~ as_adjacency_matrix(.x, sparse = F)) -> between_group_networks
    list_networks <- list(within_group_networks, between_group_networks)
    names(list_networks) <- c("within_group_networks", "between_group_networks")
    return(list_networks)
  }
  if (return == "edgelist") {
    within_group_networks %>%
      map(~ as_edgelist(.x)) -> within_group_networks
    between_group_networks %>%
      map(~ as_edgelist(.x)) -> between_group_networks
    list_networks <- list(within_group_networks, between_group_networks)
    names(list_networks) <- c("within_group_networks", "between_group_networks")
    return(list_networks)
  }
}


