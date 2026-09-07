getTOC_Rscript <- function(file) {
    x <- scan(file, "", sep = "\n", quiet = TRUE, blank.lines.skip = FALSE)
    fmt <- paste0("%", as.integer(log10(length(x))) + 1L, "d : %s")
    i <- grep("^### {1}", x)
    cat(sprintf(fmt, i, gsub("^### ", "", x[i])), sep = "\n")
}
