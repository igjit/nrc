test_that("lift_lambda", {
  input <- parse(tokenize("foo(1)"))
  expected <- list(functions = list(), nodes = input)
  expect_equal(lift_lambda(input), expected)

  input <- parse(tokenize("(function(a) a + 2)(40)"))
  expected <- list(functions = list("_f1" = node_function(list(node_ident("a")), node("+", node_ident("a"), node_num(2)))),
                   nodes = list(node_call(node_ident("_f1"), list(node_num(40)))))
  expect_equal(lift_lambda(input), expected)
})
