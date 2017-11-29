#' Visualization of network the nodes arranged by group
#'
#' @param network Either an \emph{n} by \emph{n} adjacency matrix, a \code{igraph} object, or \code{network} object.
#' @param group A vector of node attributes indicating group membership or a data frame of two columns, one with node names and one with group membership. Group membership should be of class character.
#' @param layout Either "fr" or "kk".
#' @param weight_within Determines how strongly groups cluster together. Default is 10.
#' @param weight_between Determines how weak between group ties cluster. Lower values spread groups further apart. Default is 1.
#' @return A vector of range scores for nodes in network.
#' @examples
#' # A sample vector of group membership
#' set.seed(385)
#' grp <- sample(1:3, 20, replace = TRUE)
#' # A sample network of the same order (20)
#' require(sna)
#' net <- rgraph(20, 1, 0.2)
#' group_plot(net, grp)

group_plot <- function(network, group, 
                       weight_within = 10, 
                       weight_between = 1,
                       layout = "fr") 
  {
  require(igraph)
  if (class(group) != "data.frame")
  if (class(network) == "network") {
    network <- intergraph::asIgraph(network)
    V(network)$name = V(network)$vertex.names
  } 
  if (class(network) == "matrix") {
    network <- graph_from_adjacency_matrix(network)
  }
  
  if (is.null(V(network)$name)) {
    V(network)$name <- seq(gorder(network))
  }
  if (class(group) != "data.frame") {
    group <- data.frame(name = V(network)$name,
                        group = group,
                        stringsAsFactors = F)
  }
  
  if (class(group$group) != "character") {
    group$group <- as.character(group$group)
  }
  
  all_possible_within_ties <- t(combn(V(network)$name, 2)) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    rename(ego = V1, alter = V2) %>% 
    mutate(ego_group = group$group[match(ego, group$name)],
           alter_group = group$group[match(alter, group$name)]) %>%
    mutate(theoretical_tie = if_else(ego_group == alter_group, 1, 0)) %>% 
    select(ego, alter, theoretical_tie) %>% 
    filter(theoretical_tie == 1)
  
  edgelist <- as_edgelist(network, names = T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    transmute(ego = V1,
              alter = V2,
              theoretical_tie = 0) %>% 
    mutate(ego_group = group$group[match(ego, group$name)],
           alter_group = group$group[match(alter, group$name)]) %>%
    mutate(same_group = if_else(ego_group == alter_group, 1, 0)) %>% 
    select(ego, alter, theoretical_tie, same_group)
  
  combined_edgelists <- bind_rows(edgelist, all_possible_within_ties)
  
  # Match names to group affiliation
  require(dplyr)
  weighted_network <- combined_edgelists %>%
    mutate(weight = if_else(theoretical_tie == 1, weight_within, weight_between)) %>% 
    graph_from_data_frame()
  
  V(weighted_network)$group <- group$group[match(V(weighted_network)$name, group$name)]
    
  require(ggraph)
  if (layout == "fr") {
    layout <- create_layout(weighted_network, layout = 'fr', weights = E(weighted_network)$weight)
  } else {
    layout <- create_layout(weighted_network, layout = 'kk', weights = E(weighted_network)$weight)
  }

  weighted_network <- delete_edges(weighted_network, 
               E(weighted_network)[which(E(weighted_network)$theoretical_tie == 1)])
  
  attributes(layout)$graph <- weighted_network
  edge_alpha <- if_else(E(weighted_network)$same_group == 1, 0.5, 0.25)
  
  ggraph(layout) + 
    geom_edge_link(aes(colour = as.character(same_group), alpha = edge_alpha)) + 
    scale_edge_color_manual("Edges", values = c("darkred", "slategray"),
                            labels = c("Between group", "Within group")) +
    scale_edge_alpha(guide = F) +
    geom_node_point(aes(colour = group)) +
    theme_void()
    
}
