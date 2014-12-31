source("functions.r")

init <- read.csv("init.csv")
ages <- c(25, 35, 45)

## No thining
A0 <- init$age
V0 <- init$V
B0 <- init$B
N0 <- init$N
Hd0 <- init$Hd

A <- ages
V <- volume(A0, A, Hd0, B0, N0)
B <- basalArea(A0, A, Hd0, B0, N0)
N <- numberOfTrees(A0, A, Hd0, N0)
Hd <- height(A0, A, Hd0)

A <- c(A0, A)
V <- c(V0, V)
B <- c(B0, B)
N <- c(N0, N)
Hd <- c(Hd0, Hd)

noThining <- data.frame(A = A,
                        V = V,
                        B = B,
                        N = N,
                        Hd = Hd)

write.csv(noThining, "noThining.csv")

## Thining
A0.th <- A0
V0.th <- V0 * 0.6
B0.th <- B0 * 0.6
N0.th <- ceiling(N0 * 0.6)
Hd0.th <- Hd0

A.th <- ages
V.th <- volume(A0.th, A.th, Hd0.th, B0.th, N0.th)
B.th <- basalArea(A0.th, A.th, Hd0.th, B0.th, N0.th)
N.th <- numberOfTrees(A0.th, A.th, Hd0.th, N0.th)
Hd.th <- height(A0.th, A.th, Hd0.th)

A.th <- c(A0.th, A.th)
V.th <- c(V0.th, V.th)
B.th <- c(B0.th, B.th)
N.th <- c(N0.th, N.th)
Hd.th <- c(Hd0.th, Hd.th)

thining <- data.frame(A = A.th,
                      V = V.th,
                      B = B.th,
                      N = N.th,
                      Hd = Hd.th)

write.csv(thining, "thining.csv")

## Cal
noThining.income <- noThining$V[nrow(noThining)] * 2500
thining.income <- thining$V[nrow(thining)] * 2800
thining.income <- thining.income + V0 * 0.4 * 800

income <- c(noThining.income, thining.income)
names(income) <- c("noThining.income", "thining.income")

print(income)

print(paste("fraction: ",
            round(thining.income / noThining.income * 100, digits = 2),
            "%", sep = ""))
