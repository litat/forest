source("functions.r")
samples <- read.csv("sample.csv")

## Problem 1
num <- c(2:120)
calculatedSampleSize <- calculateSampleSize(num)
sampleSizes <- data.frame(number=num, calculateSampleSize=calculatedSampleSize)

choosedSampleSize <- ceiling(sampleSizes$calculateSampleSize[nrow(sampleSizes)])

pdf(file="problem1.pdf", width=5, height=5)
plot(sampleSizes, type="l", main="Sample Size Estimate")
lines(x=num, y=rep(choosedSampleSize-1, length(num)), lty=2)
legend(x=30, y=1000, legend=c("Calculated Sample Size",
                        paste("Estimate Sample Size: ", choosedSampleSize)), lty=1:2)
dev.off()

print("Problem 1")
print(choosedSampleSize)

## Problem 2
randomSampling <- sample(1:120, choosedSampleSize)
randomSampling <- sort(randomSampling)

doplot("problem2",
       dataframe.plot(randomSampling),
       "Random Sampling Choosed Plot")



## Problem 3
systemSampling.rate <- choosedSampleSize / 120
systemSampling.n <- floor(1 / systemSampling.rate)

systemSampling.init <- sample(1:10, 1)

systemSampling <- seq(systemSampling.init, 120, by=systemSampling.n)

doplot("problem3",
       dataframe.plot(systemSampling),
       "System Sampling Choosed Plot")



## Problem 4
stratifiedSampling.rate <- systemSampling.rate

stratifiedSampling.1.body <- 1:30
stratifiedSampling.2.body <- 31:50
stratifiedSampling.3.body <- 51:120

getStratifiedSample <- function(body, rate) {
    return (sample(body, length(body) * rate))
}

stratifiedSampling.1 <- getStratifiedSample(stratifiedSampling.1.body,
                                            stratifiedSampling.rate) 
stratifiedSampling.2 <- getStratifiedSample(stratifiedSampling.2.body,
                                            stratifiedSampling.rate) 
stratifiedSampling.3 <- getStratifiedSample(stratifiedSampling.3.body,
                                            stratifiedSampling.rate) 

stratifiedSampling <- c(stratifiedSampling.1, stratifiedSampling.2, stratifiedSampling.3)
stratifiedSampling <- sort(stratifiedSampling)

doplot("problem4",
       dataframe.plot(stratifiedSampling),
       "Stratified Sampling Choosed Plot")

## Statistics
samp.sts <- data.frame(
    Problem2=calculate.statistic(samples, systemSampling),
    Problem3=calculate.statistic(samples, randomSampling),
    Problem4=calculate.statistic(samples, stratifiedSampling))

samp.sts <- round(samp.sts, digits = 2)
write.csv(t(samp.sts), file="problem.csv")
