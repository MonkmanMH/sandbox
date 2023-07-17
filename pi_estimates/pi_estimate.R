# estimating pi with increasing sample size
# modified from code posted to mastodon 
#    by @neilstats@fediscience.org 2023-03-14

# with z as number of iterations
set.seed(14)
z <- 1000

x <- runif(z)
y <- runif(z)
n <- 1:z

#plot(n, sapply(n, (\(i) mean(x[1:i]^2 + y[1:i]^2 < 1) * 4)), type = "l")

pi_est <- sapply(n, (\(i) mean(x[1:i]^2 + y[1:i]^2 < 1) * 4))

pi_est[z]
mean(pi_est)
plot(n, pi_est, type = "l")
