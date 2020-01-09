convert <- function(nodes) {
  extract_functions(nodes)
}

extract_functions <- function(nodes) {
  reduce(nodes, function(acm, node) {
    if (node$op == "=" && is(node$rhs, "node_function")) {
      name <- val(node$lhs)
      acm$functions[[name]] <- node$rhs
    } else {
      acm$nodes <- c(acm$nodes, list(node))
    }
    acm
  },
  .init = list(functions = list(), nodes = list()))
}
