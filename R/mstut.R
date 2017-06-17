set.seed(1L)

mzrange = c(100, 1000)
npeaks = 10
nimg = 100

ions <- function() {

    mzs <- runif(npeaks, min = min(mzrange), max = max(mzrange)) 
    maxint <- 10
    k <- sample(maxint, npeaks, replace = TRUE)
    mzs <- sample(rep(mzs, k))
    N <- length(mzs)



    cls0 <- colorRampPalette(c("black", "steelblue"))(100)
    cls0 <- paste0(cls0, "AA")
    cls <- cls0[round(mzs, -2)/10]

    ys <- seq(0.05, 0.95, length = N)
    x0 <- rep(100, N) ## start
    xs <- mapply(seq, x0, mzs, length = nimg)
    cex <- mzs / max(mzs) * 2

    list(mzrange = mzrange,
         mzs = xs,
         cex = cex,
         cls = cls)


}

empty_ms <- function(main = "Analyser") {
    plot(NA, type = "n", xlim = c(100, 1000), ylim = c(0, 1),
         yaxt = "n", xlab = "M/Z", ylab = "Analytes",
         main = main)
}

apply(xs, 1,
      function(xi) {
          plot(xi, ys, 
               xlim = c(100, 1000), ylim = c(0, 1),
               yaxt = "n", xlab = "M/Z", ylab = "Analytes",
               pch = 19, col = cls,
               main = "Analyser", cex = cex)
          Sys.sleep(0.1)          
      })

plot(xs[l, ], ys, 
     xlim = c(100, 1000), ylim = c(0, 1),
     yaxt = "n", xlab = "M/Z", ylab = "Intensity",
     pch = 19, col = cls,
     main = "Detector", cex = cex)

grid()
smzs <- sort(mzs)
lines(unique(smzs), table(smzs)/maxint, type = "h", lwd = 2)

plot(xs[l, ], ys, type = "n",
     xlim = c(100, 1000), ylim = c(0, 1),
     yaxt = "n", xlab = "M/Z", ylab = "Intensity",
     pch = 19, col = cls,
     main = "Spectrum", cex = cex)
grid()
lines(unique(smzs), table(smzs)/maxint, type = "h", lwd = 2)


dat <- tibble(MZ = unique(smzs),
              Intensity = as.vector(table(smzs)))
print(dat)

