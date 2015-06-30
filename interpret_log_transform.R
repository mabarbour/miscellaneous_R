m = 0.5
b = 5
x = 1:100
r1 = rnorm(100, mean = 0, sd = 1)
r2 = rnorm(100, mean = 0, sd = 5)
y1 = m*x + b + r1
y2 = m*x + b + r2

plot(y1 ~ x)

summary(lm(log(y1) ~ x))
summary(lm(log(y1+1) ~ x)) # this example shows that little bias in the coefficient is introduced by adding '1' to this example.
