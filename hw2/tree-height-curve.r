dbh_height <- read.csv('tree-height-curve.csv')
dbh_height$Height.log <- log(dbh_height$Height)
dbh_height$DBH.log <- log(dbh_height$DBH)

formula1 <- Height.log ~ DBH.log
formula2 <- Height ~ DBH+I(DBH^2)
formula3 <- Height ~ I(1/DBH)
formula4 <- Height.log ~ DBH

lm1 <- lm(formula1, data=dbh_height)
lm2 <- lm(formula2, data=dbh_height)
lm3 <- lm(formula3, data=dbh_height)
lm4 <- lm(formula4, data=dbh_height)

summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
