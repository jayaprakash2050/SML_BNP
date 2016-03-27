num.iterations <- 1000;

source("SML_BNP\\SML_Functions.R");

# Download South African heart disease data
sa.heart <- read.csv("Data/Example.csv",head=TRUE,row.names=1);

x <- sa.heart[,c("age", "ldl")]
y <- sa.heart$chd
plot(x, pch=21, bg=c("red","green")[factor(y)])


# Standardize the features
x.scaled <- zscore(x)

unscaled.theta <- gradient.descent(x=x, y=y, num.iterations=num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=x.scaled, y=y, num.iterations=num.iterations, output.path=TRUE)

summary(glm(chd ~ age + ldl, family = binomial, data=sa.heart))

qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")

# Look at output for various different alpha values
vary.alpha <- lapply(c(1e-12, 1e-9, 1e-7, 1e-3, 0.1, 0.9), 
	function(alpha) gradient.descent(x=x.scaled, y=y, alpha=alpha, num.iterations=num.iterations, output.path=TRUE))

par(mfrow = c(2, 3))
for (j in 1:6) {
	plot(vary.alpha[[j]][,2], ylab="area (alpha=1e-9)", xlab="iteration", type="l")
}