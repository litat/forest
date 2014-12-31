options(scipen=6)

visitor <- read.csv("Sitou-visitor.csv")

## Problem 1, 2

pdf("timeSeries.pdf", width = 6, height = 6)

plot(N ~ Year.w, data = visitor, main = "Visitors to Xitou",
     xlab = "Year", ylab = "Number of Visitors",
     type = "l")

visitor.q1 <- visitor[visitor$Qrt == 1,]
visitor.q3 <- visitor[visitor$Qrt == 3,]

points(visitor.q1$Year.w, visitor.q1$N - 20000, pch = "1")
points(visitor.q3$Year.w, visitor.q3$N + 20000, pch = "3")

maPar <- 4
visitor.ma <- filter(visitor$N, rep(1/maPar, maPar), sides = 1)
lines(visitor$Year.w, visitor.ma, lty = 2)

legend(2002, 540000,  c("Number of Visitors", "Moving Average"), lty = c(1, 2))

dev.off()

## Problem 3, 4

visitor.lm <- lm(N ~ Year + factor(Qrt), data = visitor)
visitor.fitted <- fitted(visitor.lm)

pdf("timeSeriesX3.pdf", width = 6, height = 6)

plot(N ~ Year.w, data = visitor, main = "X-3 Estimation",
     xlab = "Year", ylab = "Number of Visitors",
     type = "l", lty = 2)

lines(visitor$Year.w, visitor.fitted)

legend(2002, 540000,  c("X3 Estimation", "Actual Visitors"), lty = c(1, 2))

dev.off()
