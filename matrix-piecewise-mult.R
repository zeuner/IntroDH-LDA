## Description: piecewise matrix multiplication for lower memory consumption
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

## multiply matrices `x` and `y` by piecewise slices of size `size`
matrix_mult <- function(x, y, size) {
    dim_along <- attributes(x)$Dim[[2]]
    result <- Matrix(0, attributes(x)$Dim[[1]], attributes(y)$Dim[[2]])
    while (size < dim_along) {
        head_dim <- 1 : size
        head <- x[, head_dim, drop = FALSE] %*% y[head_dim,, drop = FALSE]
        result <- result + head
        tail_dim <- (size + 1) : dim_along
        x <- x[, tail_dim, drop = FALSE]
        y <- y[tail_dim,, drop = FALSE]
        dim_along <- attributes(x)$Dim[[2]]
    }
    tail <- x %*% y
    result + tail
}

## multiply matrix `x` with the matrix `y` defined by the slice generator `g`
##  and range `r` ##  by piecewise slices of size `size`
##  `g` satisfies `y[a:b,, drop = FALSE] == g(a:b)` for ranges `a:b`
##  and `y == g(r)`
matrix_mult_generated <- function(x, g, r, size) {
    dim_along <- attributes(x)$Dim[[2]]
    result <- Matrix(0, attributes(x)$Dim[[1]], attributes(g(r[1:1]))$Dim[[2]])
    while (size < dim_along) {
        head_dim <- 1 : size
        head <- x[, head_dim, drop = FALSE] %*% g(r[head_dim])
        result <- result + head
        tail_dim <- (size + 1) : dim_along
        x <- x[, tail_dim, drop = FALSE]
        r <- r[tail_dim]
        dim_along <- attributes(x)$Dim[[2]]
    }
    tail <- x %*% g(r)
    result + tail
}
