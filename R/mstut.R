##' This is the constructor function to generate a set of ions that
##' can later be analysed with `analyse()` and detected with
##' `detect()`. 
##'
##' @title Creates an object of class `ions`.
##' @param npeaks A `numeric` scalar defining the number of unique
##'     peaks (M/Z values). Default is 10.
##' @param mzrange A `numeric` of length 2 defining the range of
##'     possible M/Z values. Default is `c(100, 1000)`.
##' @param nimg A `numeric` scalar. When analysing the ions, their
##'     separation along their M/Z values will be split along a
##'     sequence of length `nimg`. Default is 100.
##' @return An object of class `ions`.
##' @author Laurent Gatto
##' @examples
##' x <- ions()
##' x
ions <- function(npeaks = 10,
                 mzrange = c(100, 1000),
                 nimg = 100) {
    ## peaks
    mzs <- runif(npeaks, min = min(mzrange), max = max(mzrange))    
    maxint <- 10
    k <- sample(maxint, npeaks, replace = TRUE)    
    mzs <- sample(rep(mzs, k))
    N <- length(mzs)

    ## visuals
    cex <- mzs / max(mzs) * 2

    ## ms data
    ys <- seq(0.05, 0.95, length = N)
    x0 <- rep(100, N) ## start
    msdata <- mapply(seq, x0, mzs, length = nimg)

    ## spectrum
    smzs <- sort(mzs)
    sp <- data.frame(MZ = unique(smzs),
                     Intensity = as.vector(table(smzs))/maxint)

    structure(list(mzrange = range(mzs),
                   ys = ys,
                   msdata = msdata,
                   size = cex,
                   N = N,
                   spectrum = sp),
              class = "ions")
}

print.ions <- function(x, ...) {
    cat("Object of class 'ions':\n")
    cat("# analyze(.); detect(.); spectrum(.)\n")
}

analyse <- function(x, sleep = 0.1) {
    stopifnot(inherits(x, "ions"))
    apply(x$msdata, 1,
          function(xi) {
              plot(xi, x$ys, 
                   xlim = x$mzrange, ylim = c(0, 1),
                   yaxt = "n", xlab = "M/Z", ylab = "Analytes",
                   main = "Analyser", cex = x$size)
              Sys.sleep(sleep)
          })
    invisible(TRUE)    
}

detect <- function(x, new = FALSE) {
    stopifnot(inherits(x, "ions"))
    if (new)
        plot(x$msdata[nrow(x$msdata),], x$ys, 
             xlim = x$mzrange, ylim = c(0, 1),
             yaxt = "n", xlab = "M/Z", ylab = "Analytes",
             main = "Analyser", cex = x$size)        
    grid()
    lines(x$spectrum$MZ, x$spectrum$Intensity,
          col = "red", type = "h", lwd = 2) 

}

spectrum <- function(x, ...) {
    plot(x$spectrum, type = "h",
         ylim = c(0, 1),
         lwd = 2, ...)
    grid()
}

## empty_ms <- function(main = "Analyser") {
##     plot(NA, type = "n", xlim = c(100, 1000), ylim = c(0, 1),
##          yaxt = "n", xlab = "M/Z", ylab = "Analytes",
##          main = main)
## }





