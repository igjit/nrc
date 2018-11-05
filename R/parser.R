parse <- function(tokens) {
    ref <- as.environment(list(pos = 1))
    expr(tokens, ref)
}

expr <- function(tokens, ref) {
    lhs <- mul(tokens, ref)
    if (ref$pos > length(tokens)) {
        lhs
    } else {
        token <- tokens[[ref$pos]]
        op <- ty(token)
        if (op %in% c("+", "-")) {
            ref$pos <- ref$pos + 1
            node(op, lhs, expr(tokens, ref))
        } else {
            lhs
        }
    }
}

mul <- function(tokens, ref) {
    lhs <- term(tokens, ref)
    if (ref$pos > length(tokens)) {
        lhs
    } else {
        token <- tokens[[ref$pos]]
        op <- ty(token)
        if (op %in% c("*", "/")) {
            ref$pos <- ref$pos + 1
            node(op, lhs, mul(tokens, ref))
        } else {
            lhs
        }
    }
}

term <- function(tokens, ref) {
    token <- tokens[[ref$pos]]
    if (is_num(token)) {
        ref$pos <- ref$pos + 1
        node_num(val(token))
    } else if (ty(token) == "(") {
        ref$pos <- ref$pos + 1
        node <- expr(tokens, ref)
        next_token <- tokens[[ref$pos]]
        if (ty(next_token) != ")") {
            stop("missing ): ", ty(next_token))
        }
        node
    } else {
        stop("unexpected token: ", ty(token))
    }
}
