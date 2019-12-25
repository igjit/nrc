context("nrc.R")

test_that("token", {
  tk <- token("1")
  expect_equal(ty(tk), TK_NUM)
  expect_equal(val(tk), 1)

  tk <- token("a")
  expect_equal(ty(tk), TK_IDENT)
  expect_equal(val(tk), "a")

  tk <- token("+")
  expect_equal(ty(tk), "+")

  tk <- token("<-")
  expect_equal(ty(tk), "=")

  tk <- token(".")
  expect_equal(ty(tk), TK_IDENT)
  expect_equal(val(tk), ".")

  tk <- token(".a")
  expect_equal(ty(tk), TK_IDENT)
  expect_equal(val(tk), ".a")

  tk <- token("var_name1")
  expect_equal(ty(tk), TK_IDENT)
  expect_equal(val(tk), "var_name1")
})

test_that("tokenize", {
  expect_equal(tokenize("1"), list(token(1)))
  expect_equal(tokenize("1+2"), list(token(1), token("+"), token(2)))
  expect_equal(tokenize("1*2/3"), list(token(1), token("*"), token(2), token("/"), token(3)))
  expect_equal(tokenize("(1+2)*3"),
               list(token("("), token(1), token("+"), token(2), token(")"), token("*"), token(3)))
  expect_equal(tokenize("a=1"), list(token("a"), token("="), token(1)))
  expect_equal(tokenize("a=1;"), list(token("a"), token("="), token(1), token(";")))
  expect_equal(tokenize("b <- 2"), list(token("b"), token("<-"), token(2)))
  expect_equal(tokenize("a==1"), list(token("a"), token("=="), token(1)))
  expect_equal(tokenize("a!=1"), list(token("a"), token("!="), token(1)))
  expect_equal(tokenize("foo()"), list(token("foo"), token("("), token(")")))
  expect_equal(tokenize("foo(1, 2)"), list(token("foo"), token("("), token(1), token(","), token(2), token(")")))
})

test_that("as.character.node", {
  expect_equal(as.character(node_num(1)), "1")
  expect_equal(as.character(node("+", node("*", node_num(1), node_num(2)), node_num(3))), "(+ (* 1 2) 3)")
  expect_equal(as.character(node("=", node_ident("a"), node_num(1))), "(= a 1)")
})

test_that("compile", {
  expect_error(compile("a + 1 = 2;"), "invalid lvalue")

  # test only on Linux
  skip_on_os(c("windows", "mac", "solaris"))

  expect_equal(execute(assemble(compile("0"))), 0)
  expect_equal(execute(assemble(compile("42"))), 42)
  expect_equal(execute(assemble(compile("5+20-4"))), 21)
  expect_equal(execute(assemble(compile(" 12 + 34 - 5 "))), 41)
  expect_equal(execute(assemble(compile("5+6*7"))), 47)
  expect_equal(execute(assemble(compile("5*(9-6)"))), 15)
  expect_equal(execute(assemble(compile("(3+5)/2"))), 4)
  expect_equal(execute(assemble(compile("a = 2 + 3; a * 4"))), 20)
  expect_equal(execute(assemble(compile("a = b = 2 + 2; a + b"))), 8)
  expect_equal(execute(assemble(compile("x <- 1 + 2; x * 2"))), 6)
  expect_equal(execute(assemble(compile("1+2==3"))), 1)
  expect_equal(execute(assemble(compile("2==3"))), 0)
  expect_equal(execute(assemble(compile("2!=3"))), 1)
  expect_equal(execute(assemble(compile("foo <- 3; bar <- 2; foo - bar"))), 1)
  expect_equal(execute(assemble(compile("(function () 2)()"))), 2)
})
