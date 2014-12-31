height <- function(A0, A, Hd0) {
    height.log <- (1.33 + 5.84 / A0
                   - 10.61 / A
                   + 0.64 * log(Hd0))
    height <- exp(height.log)

    return (height)
}

numberOfTrees <- function(A0, A, Hd0, N0) {
    numberOfTrees.log <- (0.28 - 0.19 / A0
                         + 0.45 / A
                         - 0.02 * log(Hd0)
                         + 0.96 * log(N0))
    numberOfTrees <- exp(numberOfTrees.log)
    numberOfTrees <- ceiling(numberOfTrees)

    return (numberOfTrees)
}

basalArea <- function(A0, A, Hd0, B0, N0) {
    basalArea.log <- (0.20 + 9.23 / A0
                      - 12.62 / A
                      + 0.46 * log(Hd0)
                      + 0.37 * log(B0)
                      + 0.15 * log(N0))
    basalArea <- exp(basalArea.log)

    return (basalArea)
}

volume <- function(A0, A, Hd0, B0, N0) {
    volume.log <- (0.87 + 16.43 / A0
                   - 21.91 / A
                   + 1.09 * log(Hd0)
                   + 0.46 * log(B0)
                   + 0.05 * log(N0))
    volume <- exp(volume.log)

    return (volume)
}
