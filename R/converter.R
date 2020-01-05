convert <- function(nodes) {
  extract_functions(nodes)
}

extract_functions <- function(nodes, index = 1, acm = list(functions = list(), nodes = list())) {
  if (length(nodes) < index) {
    acm
  } else {
    node <- nodes[[index]]
    if (node$op == "=" && is(node$rhs, "node_function")) {
      name <- val(node$lhs)
      acm$functions[[name]] <- node$rhs
    } else {
      acm$nodes <- c(acm$nodes, list(node))
    }
    extract_functions(nodes, index + 1, acm)
  }
}
