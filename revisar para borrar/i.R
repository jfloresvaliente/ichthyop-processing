# # Parameters
# alpha <- 20
# beta <- -0.05
# theta <- 10
# 
# # Sample some points along x axis
# n <- 100
# x <- seq(n)
# 
# Make  y = f(x) + Gaussian_noise 
data.df <- data.frame(x = km,
                      y = re)

# plot data
plot(data.df$x, data.df$y)

##################--
# Select an approximate $\theta$, since theta must be lower than min(y), and greater than zero
theta.0 <- min(data.df$y) * 0.5

# Estimate the rest parameters using a linear model
model.0 <- lm(log(y - theta.0) ~ x, data=data.df)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters
start <- list(alpha = alpha.0, beta = beta.0)
start

##################--
model <- nls(y ~ alpha * exp(beta * x), data = data.df, start = start)

# Plot fitted curve
x11()
plot(data.df$x, data.df$y, xlab = 'Distance to the coast (km)', ylab = 'Larval Retention (%)', xlim = c(0,510), ylim = c(0, 45))
lines(data.df$x, predict(model, list(x = data.df$x)), col = 'red', lwd = 3)
# lines(seq(0,700,1), predict(model, list(x = seq(0,700,1))), col = 'gray', lwd = 1, lty = 2)
# abline(h = 0)
legend('topright', legend = expression('Y = 45.02 * exp(-0.006*X)'), bty = 'n')




