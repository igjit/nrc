test_that("extract_functions", {
  input <- parse(tokenize("a <- 2"))
  expected <- list(functions = list(), nodes = input)
  expect_equal(extract_functions(input), expected)

  input <- parse(tokenize("add2 <- function(x) x + 2"))
  expected <- list(functions = list("add2" = node_function(list(node_ident("x")), node("+", node_ident("x"), node_num(2)))),
                   nodes = list())
  expect_equal(extract_functions(input), expected)
})
