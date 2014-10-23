data <- read.csv('tree-volume.csv');
data.log <- log(data);
data.lm <- lm(Volume~DBH+Height, data=data.log);
data.coef <- coef(data.lm);

names(data.coef) <- c("Intercept (a)", "DBH (b)", "Height (c)");
data.coef['Intercept (a)'] <- exp(data.coef['Intercept (a)']);

print(data.coef);

summary(data.lm);
