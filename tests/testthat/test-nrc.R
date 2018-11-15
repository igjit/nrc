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
})

test_that("tokenize", {
    expect_equal(tokenize("1"), list(token(1)))
    expect_equal(tokenize("1+2"), list(token(1), token("+"), token(2)))
    expect_equal(tokenize("1*2/3"), list(token(1), token("*"), token(2), token("/"), token(3)))
    expect_equal(tokenize("(1+2)*3"),
                 list(token("("), token(1), token("+"), token(2), token(")"), token("*"), token(3)))
    expect_equal(tokenize("a=1"), list(token("a"), token("="), token(1)))
})

test_that("as.character.node", {
    expect_equal(as.character(node_num(1)), "1")
    expect_equal(as.character(node("+", node("*", node_num(1), node_num(2)), node_num(3))), "(+ (* 1 2) 3)")
})
