convert <- function(nodes) {
  lift_lambda(nodes)
}

lift_lambda <- function(nodes, index = 1, n_func = 0, acm = list(functions = list(), nodes = list())) {
  if (length(nodes) < index) {
    acm
  } else {
    node <- nodes[[index]]
    if (is(node, "node_call") && is(node$func, "node_function")) {
      n_func <- n_func + 1
      name <- paste0("_f", n_func)
      acm$functions[[name]] <- node$func
      node$func <- node_ident(name)
    }
    acm$nodes <- c(acm$nodes, list(node))
    lift_lambda(nodes, index + 1, n_func, acm)
  }
}
