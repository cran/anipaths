#' Synchronous interpolation of network using piece-wise constant interpolation
#'
#' @param network array of network observations of dimension (\code{n.indiv, n.indiv, length(network.times)})
#' @param network.times vector of times at which network observations are made
#' @param time.grid times at which network will be interpolated
#'
#' @return array of dimension \code{n.indiv, n.indiv, length(time.grid))}
network_interp <- function(network = NULL, network.times, time.grid){
  if(is.null(network)) return(NULL)
  time.grid.network <- time.grid[time.grid >= min(network.times) & time.grid <= max(network.times)]
  network.interp <- apply(network, 1:2, function(w_ij){
    network.interp.ij <- rep(NA, length(time.grid))
    network.interp.ij[time.grid %in% time.grid.network] <- 
      approx(
        x = network.times, y = w_ij,
        xout = as.numeric(time.grid.network), method = "constant"
      )$y
    return(network.interp.ij)
  })
  return(aperm(network.interp, c(2, 3, 1)))
}

#' get_network_colors()
#' Finds all maximal cliques in the network at each time point and tries to assign them a useful coloring
#' @keywords internal
#'
#' @param binary.network a 3D array giving the time-varying adjecency matrix of a dynamic network.
#' @param network.color.options vector of colors
#'
#' @return a list of two elements: a list of the maximal cliques at each time, and c list with colors for each clique at each time
#' @importFrom igraph max_cliques graph_from_adjacency_matrix
#' @importFrom RColorBrewer brewer.pal
#'
get_network_colors <- function(binary.network, network.color.options = NULL) {

  ## unique cliques ----
  cliques <- sapply(1:dim(binary.network)[3], function(t) {
    rev(lapply(max_cliques(graph_from_adjacency_matrix(binary.network[, , t],
      mode = "undirected",
      diag = F
    ), min = 2), sort))
  },
  simplify = F
  )
  unique.cliques <- NULL
  clique.lengths <- unlist(lapply(cliques, length))
  for (t in (1:dim(binary.network)[3])[which(clique.lengths > 0)]) {
    unique.cliques <- unique(c(unique.cliques, cliques[[t]]))
  }
  if (is.null(network.color.options)) {
    network.color.options <- unique(c(
      brewer.pal(8, "Dark2"),
      brewer.pal(9, "GnBu")[3:9],
      brewer.pal(9, "OrRd")[3:9],
      brewer.pal(9, "BuPu")[3:9],
      brewer.pal(9, "YlGn")[3:9],
      brewer.pal(9, "PuBu")[3:9],
      brewer.pal(9, "YlOrBr")[3:9],
      brewer.pal(9, "YlGnBu")[3:9],
      brewer.pal(9, "PuRd")[3:9],
      brewer.pal(9, "PuBuGn")[3:9],
      brewer.pal(9, "Purples")[3:9],
      brewer.pal(9, "Oranges")[3:9]
    ))
  }
  if (length(unique.cliques) > length(network.color.options)) {
    network.color.options <- c(
      network.color.options,
      rep("wheat4", length(unique.cliques) - length(network.color.options))
    )
  }
  available.colors <- network.color.options
  clique.colors <- vector("list", dim(binary.network)[3])

  ## paint network ----
  for (t in (1:dim(binary.network)[3])) {
    if (t == 1) {
      clique.colors[[t]] <- rep("black", length(cliques[[t]]))
      for (c in 1:length(cliques[[t]])) {
        clique.colors[[1]][c] <- available.colors[1]
        available.colors <- available.colors[-1]
      }
    } else {
      if (clique.lengths[t] == 0) {
        cliques[[t]] <- NA
        clique.colors[[t]] <- NA
        next
      }
      available.colors <- available.colors[!(available.colors %in% clique.colors[[t - 1]])]
      clique.colors[[t]] <- rep("black", length(cliques[[t]]))
      for (c in 1:length(cliques[[t]])) {
        from.b4 <- lapply(cliques[[t - 1]], function(c.tm1) {
          intersect(as.numeric(c.tm1), as.numeric(cliques[[t]][[c]]))
        })

        ## (1) check to see if this clique is nested inside a previous one ----
        nested.b4 <- which(unlist(lapply(from.b4, function(intersection) {
          identical(as.numeric(intersection), as.numeric(cliques[[t]][[c]]))
        })))
        nested.b4 <- nested.b4[which(!(clique.colors[[t - 1]][nested.b4] %in% clique.colors[[t]]))]
        if (length(nested.b4) > 0) {
          clique.colors[[t]][c] <- clique.colors[[t - 1]][min(nested.b4)]
        } else {

          ## (2) check to see if a previous clique is nested inside this one ----
          rev.nested.b4 <- which(sapply(1:length(from.b4), function(cl) {
            identical(as.numeric(from.b4[[cl]]), as.numeric(cliques[[t - 1]][[cl]]))
          }))
          rev.nested.b4 <- rev.nested.b4[which(!(clique.colors[[t - 1]][rev.nested.b4] %in% clique.colors[[t]]))]
          if (length(rev.nested.b4) > 0) {
            clique.colors[[t]][c] <- clique.colors[[t - 1]][min(rev.nested.b4)]
          } else {

            ## laste resort: add a new color ----
            if (!is.na(available.colors[1])) {
              clique.colors[[t]][c] <- available.colors[1]
              available.colors <- available.colors[-1]
            } else {

              ## if I ran out of colors, use network.color.options[1]
              clique.colors[[t]][c] <- network.color.options[1]
            }
          }
        }
      }
    }
  }
  return(list("colors" = clique.colors, "cliques" = cliques))
}
