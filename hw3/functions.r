calculateSampleSize <- function(n) {
    return (((qt(0.975, df = n-1) * 4.56)/(0.1 * 16.10))^2)
}

dataframe.plot <- function(samp) {
    return (data.frame(
        x=floor(samp/10),
        y=floor(samp%%10)))
}

doplot <- function(problem, dataframe, main) {
    filename <- paste(problem, "pdf", sep=".")
    
    pdf(file=filename, width=5, height=5)
    plot(dataframe, main=main, xlim = c(0, 12), ylim = c(10, 1))
    dev.off()
}

calculate.statistic <- function(samples, samp) {
    samp.choose <- samples[match(samp, samples$number), ]

    samp.mean <- mean(samp.choose$stock)
    samp.sd <- sd(samp.choose$stock)

    samp.interval <-
        qt(0.975, df=choosedSampleSize-1) * samp.sd / sqrt(choosedSampleSize)
    samp.ci <- c((samp.mean - samp.interval) * 120,
                 (samp.mean + samp.interval) * 120)

    samp.stock <- samp.mean * 120

    samp.st <- c(samp.mean,
                 samp.sd,
                 samp.stock,
                 samp.ci)
    names(samp.st) <- c("Mean",
                        "Standard deviation",
                        "Stock",
                        "Confidence interval lower",
                        "Confidence interval upper")
    
    return (samp.st)
}
