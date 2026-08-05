## These two functions do the same thing but with different output
## formats in case of out-of-range pixels.

## 'rmskb' must be loaded in the workspace
getNei <- function(cell, order = 1, quiet = FALSE)
{
    NR <- nrow(rmskb)
    NC <- ncol(rmskb)
    j <- (cell - 1L) %% NC + 1L
    i <- (cell - j) / NC + 1L
    mor <- max(order)
    v <- -mor:mor
    n <- length(v)
    offset_i <- rep(v, each = n)
    offset_j <- rep(v, n)
    ii <- i + offset_i
    jj <- j + offset_j
    if (!all(sort(order) == 0:mor)) {
        aoi <- abs(offset_i)
        aoj <- abs(offset_j)
        o <- ifelse(aoi > aoj, aoi, aoj)
        ##o <- apply(cbind(abs(offset_i), abs(offset_j)), 1, max)
        s <- which(o %in% order)
        ii <- ii[s]
        jj <- jj[s]
    }
    d <- ii < 1 | ii > NR | jj < 1 | jj > NC
    if (any(d)) {
        ii <- ii[!d]
        jj <- jj[!d]
        if (!quiet)
            warning("dropping ", sum(d), " pixels out-of-range")
    }
    (ii - 1L) * NC + jj # unsorted
}

getNei2 <- function(cells, order = 1)
{
    NR <- nrow(rmskb)
    NC <- ncol(rmskb)
    j <- (cells - 1L) %% NC + 1L
    i <- (cells - j) / NC + 1L
    v <- -order:order
    n <- length(v)
    N <- n^2
    offset_i <- rep(v, each = n)
    offset_j <- rep(v, n)
    ii <- rep(i, each = N) + offset_i
    jj <- rep(j, each = N) + offset_j
    ii[ii < 1 | ii > NR] <- NA_integer_
    jj[jj < 1 | jj > NC] <- NA_integer_
    res <- (ii - 1L) * NC + jj # unsorted
    matrix(res, N, length(cells))
}
