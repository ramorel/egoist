#' Visualization of all ego networks in a network or list of ego networks.
#'
#' @param networks Either an \emph{n} by \emph{n} adjacency matrix, a \code{igraph} object, or \code{network} object.
#' @param method Method to use to produce visualizations. Either 'igraph', 'network', or 'ggnet2'.
#' @param order 0 to include only edges sent and received by the ego. 1 to include the ego's first-step neigborhood.
#' @param ncol Integer indicating the number of columns for displaying the visualizations.
#' @param nrow Integer indicating the number of rows for displaying the visualizations.
#' @param pdf Boolean. If TRUE, will output a pdf of visualiztions.
#' @param filename String indicating the file name to save PDF as.
#' @param gg_args Additional \code{ggplot2} layers called with the \code{+} operator.
#' @param ... Additional arguments to pass to \code{method}.
#' @return A list (of either \code{igraph} objects, \code{network} objects, or adjacency matrices) containing the ego networks for each node in the network`
#' @examples
#' #Default settings
#' require(sna)
#' set.seed(6436)
#' net <- rgraph(20, 1, 0.25)
#' ego_plot(net)
#'
#' #Pass an network object, return igraph visualizations
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' ego_plot(net, method = 'igraph')
#'
#' #Pass additional arguments from visualization method
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' Use `?gplot` to see possible additional arguments
#' ego_plot(net, method = 'gplot', vertex.col = 'pink', edge.col = 'gray50')
#' Use ?igraph.plotting to see possible additional arguments
#' ego_plot(net, method = 'igraph', vertex.color = 'slategray', vertex.size = 15, vertex.label = NA)
#'
#' #Pass additional `ggplot2` arguments
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' ego_plot(net, method = "ggnet2", color = "blue", gg_args = list(ggtitle("ggnet2 plot"), coord_equal()))
#'
#' Save plots to a PDF file
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' ego_plot(net, method = "gplot", pdf = TRUE, filename = "ego networks")
#'
#' See each ego's "star" network
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' ego_plot(net, order = 0)
#'
#' # Don't pass arguments to gg_args unless method = "ggnet2"!
#' \dontrun{
#' set.seed(6436)
#' net <- as.network(rgraph(20, 1, 0.25))
#' ego_plot(net, method = "gplot", gg_args = list(ggtitle("No way!")))
#' }


ego_plot <- function(networks, method = NULL, order = 1, ncol = 1, nrow = 1, pdf = FALSE,
                     filename = NULL, gg_args = NULL, ...) {
  if (!is.null(gg_args) && method != "ggnet2") {
    stop("You can only specify \"gg_args\" with method = \"ggnet2\".", call. = FALSE)
  }
  if (class(networks) != "list" & !(class(networks) %in% c("igraph", "network", "matrix"))) {
    stop("Requires an igraph object, network object, adjacency matrix, or list thereof.\n",
         call. = FALSE)
  }
  if (class(networks) == "list" & (length(unique(sapply(networks, class))) > 1 | !all(sapply(networks,
                                                                                             class) %in% c("igraph", "network", "matrix")))) {
    stop("List must contain all the same class, one of \"gplot\", \"igraph\", or \"matrix\".\n",
         call. = FALSE)
  }
  if (!is.null(method) && !(method %in% c("igraph", "gplot", "ggnet2"))) {
    stop("Method must be one of the following: \"gplot\", \"igraph\", or \"ggnet2\".", call. = FALSE)
  }
  if (is.null(method) & class(networks) != "list") {
    if (class(networks) %in% c("matrix", "network")) {
      method <- "gplot"
    } else {
      method <- class(networks)
    }
    message(paste("Method", "=", method, sep = " "))
  }
  if (is.null(method) & class(networks) == "list") {
    if (unique(sapply(networks, class)) == "matrix") {
      method <- "gplot"
    } else {
      method <- unique(sapply(networks, class))
    }
    message(paste("Method", "=", method, sep = " "))
  }

  if (method == "gplot") {
    require(sna)
    if (class(networks) == "list" & !(unique(sapply(networks, class)) %in% c("network",
                                                                             "matrix"))) {
      stop("If method = \"gplot\", list must only contain \"network\" objects or matrices.",
           call. = FALSE)
    }
    if (class(networks) != "list") {
      nets <- find_ego_networks(networks, return = "network", order = order)
    } else {
      nets <- networks
    }
    par(mfrow = c(nrow, ncol))
    if (pdf == TRUE) {
      pdf(paste0(filename, ".pdf"), onefile = TRUE)
      lapply(1:length(nets), function(x) gplot(nets[[x]], ...))
      dev.off()
    } else {
      lapply(1:length(nets), function(x) gplot(nets[[x]], ...))
    }
  }
  if (method == "igraph") {
    require(igraph)
    if (class(networks) == "list" & unique(sapply(networks, class)) != "igraph") {
      stop("If method = \"igraph\", list must only contain \"igraph\" objects.",
           call. = FALSE)
    }
    if (class(networks) != "list") {
      nets <- find_ego_networks(networks, return = "igraph", order = order)
    } else {
      nets <- networks
    }
    par(mfrow = c(nrow, ncol))
    if (pdf == TRUE) {
      pdf(paste0(filename, ".pdf"), onefile = TRUE)
      lapply(1:length(nets), function(x) plot(nets[[x]], ...))
      dev.off()
    } else {
      lapply(1:length(nets), function(x) plot(nets[[x]], ...))
    }
  }
  if (method == "ggnet2") {
    require(ggplot2)
    require(GGally)
    if (class(networks) != "list") {
      nets <- find_ego_networks(networks, order = order)
    } else {
      nets <- networks
    }
    if (ncol > 1 | nrow > 1) {
      if (ncol > 1 & nrow > 1) {
        require(gridExtra)
        if (!is.null(gg_args)) {
          plots <- list()
          for (i in seq_along(nets)) {
            plots[[i]] <- ggnet2(nets[[i]])
            j <- 1
            while (j <= length(gg_args)) {
              plots[[i]] <- plots[[i]] + gg_args[[j]]
              j <- j + 1
            }
          }
        } else {
          plots <- lapply(1:length(nets), function(x) ggnet2(nets[[x]], ...))
        }
        plots <- marrangeGrob(plots, top = NULL, nrow = nrow, ncol = ncol)
        if (pdf == TRUE) {
          ggsave(paste0(filename, ".pdf"), plots)
        } else {
          print(plots)
        }
      }
      if (ncol > 1 & nrow == 1) {
        require(gridExtra)
        if (!is.null(gg_args)) {
          plots <- list()
          for (i in seq_along(nets)) {
            plots[[i]] <- ggnet2(nets[[i]])
            j <- 1
            while (j <= length(gg_args)) {
              plots[[i]] <- plots[[i]] + gg_args[[j]]
              j <- j + 1
            }
          }
        } else {
          plots <- lapply(1:length(nets), function(x) ggnet2(nets[[x]], ...))
        }
        plots <- do.call("grid.arrange", c(plots, ncol = ncol))
        if (pdf == TRUE) {
          ggsave(paste0(filename, ".pdf"), plots)
        }
      }
      if (nrow > 1 & ncol == 1) {
        require(gridExtra)
        if (!is.null(gg_args)) {
          plots <- list()
          for (i in seq_along(nets)) {
            plots[[i]] <- ggnet2(nets[[i]])
            j <- 1
            while (j <= length(gg_args)) {
              plots[[i]] <- plots[[i]] + gg_args[[j]]
              j <- j + 1
            }
          }
        } else {
          plots <- lapply(1:length(nets), function(x) ggnet2(nets[[x]], ...))
        }
        plots <- do.call("grid.arrange", c(plots, nrow = nrow))
        if (pdf == TRUE) {
          ggsave(paste0(filename, ".pdf"), plots)
        }
      }
    } else {
      if (!is.null(gg_args)) {
        plots <- list()
        for (i in seq_along(nets)) {
          plots[[i]] <- ggnet2(nets[[i]])
          j <- 1
          while (j <= length(gg_args)) {
            plots[[i]] <- plots[[i]] + gg_args[[j]]
            j <- j + 1
          }
        }
      } else {
        plots <- lapply(1:length(nets), function(x) ggnet2(nets[[x]], ...))
        if (pdf == TRUE) {
          pdf(paste0(filename, ".pdf"), onefile = TRUE)
          invisible(lapply(plots, print))
          dev.off()
        } else {
          invisible(lapply(plots, print))
        }
      }
    }
  }
}
