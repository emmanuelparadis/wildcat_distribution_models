## 'lcc' must be loaded in the workspace
buildHabitatIndex <- function(x, the_lcc, denom = NULL) {
    if (is.vector(x) && !is.list(x)) dim(x) <- c(length(x), 1L)
    if (is.matrix(x)) {
        res <- numeric(n <- nrow(x))
        if (is.null(denom)) denom <- ncol(x) * ncol(lcc)
        for (i in 1:n) res[i] <- sum(lcc[x[i, ], ] %in% the_lcc)
        res <- res / denom
    }
    if (is.list(x)) {
        res <- numeric(n <- length(x))
        denoms <- lengths(x)
        for (i in 1:n) res[i] <- sum(lcc[x[[i]], ] %in% the_lcc)
        res <- res / denoms
    }
    res
}

buildHabitatIndex2 <- function(x, the_lcc, denom = NULL) {
    if (!is.matrix(x)) stop("'x' must be a matrix")
    if (length(the_lcc) != 1) stop("'the_lcc' must be a scalar")
    res <- numeric(n <- nrow(x))
    if (is.null(denom)) denom <- ncol(x) * ncol(lcc)
    for (i in 1:n) res[i] <- sum(lcc[x[i, ], ] == the_lcc)
    res <- res / denom
    res
}
