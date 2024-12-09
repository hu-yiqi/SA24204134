## -----------------------------------------------------------------------------
library(knitr)
set.seed(123)
group1 <- rnorm(50, mean = 5, sd = 1)
group2 <- rnorm(50, mean = 6, sd = 1)

t_test_result <- t.test(group1, group2)
kable(t_test_result$statistic, caption = "t-Test Results")

boxplot(group1, group2, names = c("Group 1", "Group 2"),
      main = "Comparison of Two Groups", ylab = "Values", col = c("red", "green"))



## -----------------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(knitr)

set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)
y <- 3 + 1.5 * x + rnorm(100)
data <- data.frame(x, y)

model <- lm(y ~ x, data = data)
kable(summary(model)$coefficients, caption = "Regression Coefficients")

ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatter Plot with Regression Line", x = "x", y = "y")


## ----echo=FALSE---------------------------------------------------------------
data(iris)
summary_stats <- summary(iris)
kable(summary_stats, caption = "Descriptive Statistics of the Iris Dataset")

hist(iris$Sepal.Length, main = "Distribution of Sepal Length", xlab = "Sepal Length", col = "lightblue")


## -----------------------------------------------------------------------------
# 设置随机种子。
set.seed(123)

# 生成10000个U(0, 1)随机数。
n <- 1e4
u <- runif(n)

# 设置参数sigma的取值。
sigma <- 2

# 基于逆变换法构造Rayleigh分布随机数。
r <- sqrt(-2*sigma^2*log(1-u))

# 绘制r随机数频率直方图。
hist(r, probability=TRUE, main=expression())

y <- seq(0, 10, .01)
lines(y, y/sigma^2*exp(-y^2/(2*sigma^2)))

## -----------------------------------------------------------------------------
set.seed(123)
n <- 1e4
u1 <- rnorm(n)
u2 <- rnorm(n, 3, 1)

p1 <- c(0, .25, .50, .75, 1)

i <- 1

while(i < 6){
  r <- p1[i]*u1 + (1-p1[i])*u2
  hist(r, probability = TRUE, main=paste("P1=", p1[i]))
  y <- seq(-6, 6, .01)
  lines(y, p1[i]*(1/sqrt(2*pi)*exp(-y^2/2))+(1-p1[i])*(1/sqrt(2*pi)*exp(-(y-3)^2/2)))
  
  i <- i+1
}

## -----------------------------------------------------------------------------
set.seed(123)
# 定义生成随机数函数。
rand_func <-function(lambda, alpha, beta){ 
  # lambda为pisson分布参数，alpha为伽马分布尺度参数，beta为伽马分布形状参数。
  n <- 10000 # 定义生成的随机数个数。
  X <- numeric(length= n) # 定义随机数数组。
  t <- 10 # 定义时刻。
  
  i <- 0 # 初始化。
  while(i < n){
    i <- i + 1
    N_t = rpois(1, lambda*t)
    y <- rgamma(N_t, shape= alpha, rate= 1/beta)
    X[i] <- sum(y)
  }
  
  # X_t的理论均值和方差。
  E_X_t <- lambda*t*alpha*beta
  Var_X_t <- lambda*t*(alpha*(1+alpha)*beta^2)
  
  # 比较X_t均值，方差的估计值和理论值的差异。
  result <- c(mean(X)/E_X_t, var(X)/Var_X_t)
  
  return(result)
}

## -----------------------------------------------------------------------------
# lambda=1, alpha=2, beta=3
rand_func(1, 2, 3)

## -----------------------------------------------------------------------------
# lambda=3, alpha=1, beta=2
rand_func(3, 1, 2)

## -----------------------------------------------------------------------------
# lambda=2, alpha=3, beta=1
rand_func(2, 3, 1)

## -----------------------------------------------------------------------------
# 定义估计函数。
est_beta <- function(x){
  n <- 1e4
  r <- runif(n, 0, x)
  s <- 30*(r^2)*((1-r)^2)*x
  est <- round(mean(s), 5)
  return(est)
}

# 调用函数，生成不同x取值下的cdf估计值。
set.seed(42)
x <- seq(0.1, 0.9, 0.1)
i <- 0
est_cdf <- c()
real_cdf <- c()

while(i < 9){
  i <- i + 1
  est_cdf[i] <- est_beta(x[i])
  real_cdf[i] <- pbeta(x[i], 3, 3)
}

# 输出估计值和实际值。
print("beta(3,3)累积分布函数值估计值：")
est_cdf
print("beta(3,3)累积分布函数值实际值：")
real_cdf

# 比较估计值和实际值。
print("beta(3,3)累积分布函数值估计值与实际值的比值：")
round(est_cdf / real_cdf, 5)





## -----------------------------------------------------------------------------
set.seed(42)
sample_rayleigh <- function(n, sigma){ # 生成n个参数为sigma的Rayleigh分布随机数。
  # 生成两个独立的Rayleigh分布的随机样本，并取平均值。
  r1 <- (sigma * sqrt(-2 * log(runif(n)))+sigma * sqrt(-2 * log(runif(n))))/2
  # 生成一个均匀分布样本，并用antithetic方法生成一个Rayleigh分布的随机样本。
  ru <- runif(n)
  r2 <- (sigma * sqrt(-2 * log(ru))+sigma * sqrt(-2 * log(1-ru)))/2
  
  # 计算方差和百分比。
  var_1 <- var(r1)
  var_2 <- var(r2)
  percent_reduction <- abs((var(r2)-var(r1))/var(r1))
  
  result <- round(c(var_1, var_2, percent_reduction), 5)
  # 返回值。
  return(result)
  
}


# 调用函数。
result <- sample_rayleigh(1e4, 1) # 生成10000个参数为1的Rayleigh分布随机数。
cat("生成两个独立的Rayleigh分布的随机样本方差为：", result[1], end="\n")
cat("对偶方法生成一个Rayleigh分布的随机样本方差为：", result[2], end="\n")
cat("方差减少的百分比为：", result[3], end="\n")
  


## -----------------------------------------------------------------------------
# 设置随机数种子。
set.seed(42)
# 生成随机数个数。
n <- 1e6

# 生成服从标准正态分布和参数为1的瑞利分布的随机数。
r_norm <- rnorm(n)
r_rayleigh <- sqrt(-2 * log(runif(n)))

# 筛选出大于1的随机数，并计算其频率，用来估计对应概率。
r1 <- r_norm[r_norm>1]
r2 <- r_rayleigh[r_rayleigh>1]

p1 <- length(r1)/n
p2 <- length(r2)/n

# 画出g(x), f_1(x), f_2(x)的函数图象，进行比较，其中概率用频率估计。
x <- seq(0, 10, 0.01)
g <- x^2/(sqrt(2*pi))*exp(-x^2/2)
f_1 <- 1/(sqrt(2*pi))*exp(-x^2/2)/p1
f_2 <- x^2/2*exp(-x)/p2
                       
plot(x, g, type = 'l', col='blue',ylim = c(0, 2.5), ylab = "y")
lines(x, f_1, type = 'l', col="red")
lines(x, f_2, type = 'l', col="yellow")

# 估计量相关函数表达式。
g <- 1/sqrt(2*pi)*r1^2*exp(-x^2/2)
f1 <- 1/sqrt(2*pi)*exp(-x^2/2)/p1
f2 <- r2^2/2*exp(-r2^2/2)/p2

# 计算估计值及估计值的方差。
est1 <- mean(g/f1)
var1 <- var(g/f1)
est2 <- mean(g/f2)
var2 <- var(g/f2)

# 输出结果。
cat("由重要性函数f_1得到的积分估计值：",est1, end="\n")
cat("由重要性函数f_1得到的积分估计值的方差：",var1, end="\n")
cat("由重要性函数f_2得到的积分估计值：",est2, end="\n")
cat("由重要性函数f_2得到的积分估计值的方差：",var2, end="\n")


## -----------------------------------------------------------------------------
# 定义快速排序法函数。
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}#??????
}


# 定义生成的随机数个数并初始化。
n <-c(1, 2, 4, 8)*1e4
i <- 0
n_len <- length(n)
a <-c()

# 采用循环处理不同随机数个数的情况。
while(i<n_len){
  i <- i+1
  time_i <-c()
  j <- 0
  while(j<100){ # 对每个n，进行100次排序，计算平均时间。
    j <- j+1
    test<-sample(1:n[i])
    time_i[j] <- system.time(quick_sort(test))[1]
  }
  a[i] <- mean(time_i)
}

# 定义时间复杂度。
t <- n*log(n)

model <- lm(a~t)

a_predict <- predict(model)

plot(t, a, col="blue")
lines(t, a_predict, col="red")

# 其统计结果输出。
summary(model)


## -----------------------------------------------------------------------------
# 定义估计函数。
est_beta <- function(x){
  n <- 1e4
  r <- runif(n, 0, x)
  s <- 30*(r^2)*((1-r)^2)*x
  est <- round(mean(s), 5)
  return(est)
}

# 调用函数，生成不同x取值下的cdf估计值。
set.seed(42)
x <- seq(0.1, 0.9, 0.1)
i <- 0
est_cdf <- c()
real_cdf <- c()

while(i < 9){
  i <- i + 1
  est_cdf[i] <- est_beta(x[i])
  real_cdf[i] <- pbeta(x[i], 3, 3)
}

# 输出估计值和实际值。
print("beta(3,3)累积分布函数值估计值：")
est_cdf
print("beta(3,3)累积分布函数值实际值：")
real_cdf

# 比较估计值和实际值。
print("beta(3,3)累积分布函数值估计值与实际值的比值：")
round(est_cdf / real_cdf, 5)





## -----------------------------------------------------------------------------
set.seed(42)
sample_rayleigh <- function(n, sigma){ # 生成n个参数为sigma的Rayleigh分布随机数。
  # 生成两个独立的Rayleigh分布的随机样本，并取平均值。
  r1 <- (sigma * sqrt(-2 * log(runif(n)))+sigma * sqrt(-2 * log(runif(n))))/2
  # 生成一个均匀分布样本，并用antithetic方法生成一个Rayleigh分布的随机样本。
  ru <- runif(n)
  r2 <- (sigma * sqrt(-2 * log(ru))+sigma * sqrt(-2 * log(1-ru)))/2
  
  # 计算方差和百分比。
  var_1 <- var(r1)
  var_2 <- var(r2)
  percent_reduction <- abs((var(r2)-var(r1))/var(r1))
  
  result <- round(c(var_1, var_2, percent_reduction), 5)
  # 返回值。
  return(result)
  
}


# 调用函数。
result <- sample_rayleigh(1e4, 1) # 生成10000个参数为1的Rayleigh分布随机数。
cat("生成两个独立的Rayleigh分布的随机样本方差为：", result[1], end="\n")
cat("对偶方法生成一个Rayleigh分布的随机样本方差为：", result[2], end="\n")
cat("方差减少的百分比为：", result[3], end="\n")
  


## -----------------------------------------------------------------------------
# 设置随机数种子。
set.seed(42)
# 生成随机数个数。
n <- 1e6

# 生成服从标准正态分布和参数为1的瑞利分布的随机数。
r_norm <- rnorm(n)
r_rayleigh <- sqrt(-2 * log(runif(n)))

# 筛选出大于1的随机数，并计算其频率，用来估计对应概率。
r1 <- r_norm[r_norm>1]
r2 <- r_rayleigh[r_rayleigh>1]

p1 <- length(r1)/n
p2 <- length(r2)/n

# 画出g(x), f_1(x), f_2(x)的函数图象，进行比较，其中概率用频率估计。
x <- seq(0, 10, 0.01)
g <- x^2/(sqrt(2*pi))*exp(-x^2/2)
f_1 <- 1/(sqrt(2*pi))*exp(-x^2/2)/p1
f_2 <- x^2/2*exp(-x)/p2
                       
plot(x, g, type = 'l', col='blue',ylim = c(0, 2.5), ylab = "y")
lines(x, f_1, type = 'l', col="red")
lines(x, f_2, type = 'l', col="yellow")

# 估计量相关函数表达式。
g <- 1/sqrt(2*pi)*r1^2*exp(-x^2/2)
f1 <- 1/sqrt(2*pi)*exp(-x^2/2)/p1
f2 <- r2^2/2*exp(-r2^2/2)/p2

# 计算估计值及估计值的方差。
est1 <- mean(g/f1)
var1 <- var(g/f1)
est2 <- mean(g/f2)
var2 <- var(g/f2)

# 输出结果。
cat("由重要性函数f_1得到的积分估计值：",est1, end="\n")
cat("由重要性函数f_1得到的积分估计值的方差：",var1, end="\n")
cat("由重要性函数f_2得到的积分估计值：",est2, end="\n")
cat("由重要性函数f_2得到的积分估计值的方差：",var2, end="\n")


## -----------------------------------------------------------------------------
# 定义快速排序法函数。
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}#??????
}


# 定义生成的随机数个数并初始化。
n <-c(1, 2, 4, 8)*1e4
i <- 0
n_len <- length(n)
a <-c()

# 采用循环处理不同随机数个数的情况。
while(i<n_len){
  i <- i+1
  time_i <-c()
  j <- 0
  while(j<100){ # 对每个n，进行100次排序，计算平均时间。
    j <- j+1
    test<-sample(1:n[i])
    time_i[j] <- system.time(quick_sort(test))[1]
  }
  a[i] <- mean(time_i)
}

# 定义时间复杂度。
t <- n*log(n)

model <- lm(a~t)

a_predict <- predict(model)

plot(t, a, col="blue")
lines(t, a_predict, col="red")

# 其统计结果输出。
summary(model)


## -----------------------------------------------------------------------------
d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, -0.937, 0.779, -1.409, 0.027, -1.569)
d2 <- c(1.608, 1.009, 0.878, 1.600, -0.263, 0.680, 2.280, 2.390, 1.793, 8.091, 1.468)

set.seed(123)
B <- 1e4
theta_star <- numeric(B)
theta_0 <- mean(d1) - mean(d2)

for(i in 1:B){
  d1_star <- sample(d1, replace = T)
  d2_star <- sample(d2, replace = T)
  theta_star[i] <- mean(d1_star) - mean(d2_star)
}

round(c(theta=theta_0, se.boot=sd(theta_star), se.sample=sqrt(var(d1)/length(d1) + var(d2)/length(d2))), 3)

## -----------------------------------------------------------------------------
set.seed(123)
N <- 1000
null_hypotheses <- 950
alt_hypotheses <- 50
alpha <- 0.1
m <- 10000

# 初始化结果矩阵
results <- matrix(0, nrow=3, ncol=2)
rownames(results) <- c("FWER", "FDR", "TPR")
colnames(results) <- c("Bonferroni correction", "B-H correction")

for (i in 1:m) {
  # 生成 p 值
  p_null <- runif(null_hypotheses)  # 从均匀分布生成 p 值
  p_alt <- rbeta(alt_hypotheses, 0.1, 1)  # 从 Beta 分布生成 p 值
  p_values <- c(p_null, p_alt)  # 合并 p 值

  # Bonferroni 校正
  p_bonferroni <- p.adjust(p_values, method = "bonferroni")
  reject_bonferroni <- p_bonferroni < alpha
  
  # B-H 校正
  p_bh <- p.adjust(p_values, method = "BH")
  reject_bh <- p_bh < alpha
  
  # 计算 FWER, FDR, TPR
  fwer_bonferroni <- any(reject_bonferroni[1:null_hypotheses])
  fdr_bonferroni <- ifelse(sum(reject_bonferroni) > 0, 
                           sum(reject_bonferroni & !c(rep(FALSE, null_hypotheses), rep(TRUE, alt_hypotheses))) / sum(reject_bonferroni), 
                           0)
  tpr_bonferroni <- sum(reject_bonferroni[(null_hypotheses + 1):N]) / alt_hypotheses
  
  fwer_bh <- any(reject_bh[1:null_hypotheses])
  fdr_bh <- ifelse(sum(reject_bh) > 0, 
                   sum(reject_bh & !c(rep(FALSE, null_hypotheses), rep(TRUE, alt_hypotheses))) / sum(reject_bh), 
                   0)
  tpr_bh <- sum(reject_bh[(null_hypotheses + 1):N]) / alt_hypotheses
  
  results[1, 1] <- results[1, 1] + fwer_bonferroni
  results[2, 1] <- results[2, 1] + fdr_bonferroni
  results[3, 1] <- results[3, 1] + tpr_bonferroni
  
  results[1, 2] <- results[1, 2] + fwer_bh
  results[2, 2] <- results[2, 2] + fdr_bh
  results[3, 2] <- results[3, 2] + tpr_bh
}

# 计算平均值
results <- results / m
results


## -----------------------------------------------------------------------------
x <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
n <- length(x)
lambda_hat <- n / sum(x)
lambda_hat

set.seed(123)
B <- 1000  # 自助重抽样次数
bootstrap_lambdas <- numeric(B)

for (b in 1:B) {
    sample_x <- sample(x, replace = TRUE)
    bootstrap_lambdas[b] <- length(sample_x) / sum(sample_x)
}

# 计算偏差和标准误差
bias <- mean(bootstrap_lambdas) - lambda_hat
se <- sd(bootstrap_lambdas)

# 输出结果
bias
se



## -----------------------------------------------------------------------------
set.seed(123)
x <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
n <- length(x)
lambda_hat <- n / sum(x)
mean_time <- 1 / lambda_hat
B <- 1000  # 自助重抽样次数
bootstrap_means <- numeric(B)

# 生成自助样本并计算平均时间
for (b in 1:B) {
    sample_x <- sample(x, replace = TRUE)
    bootstrap_lambda <- length(sample_x) / sum(sample_x)
    bootstrap_means[b] <- 1 / bootstrap_lambda
}

# 计算标准误差
se <- sd(bootstrap_means)

# 计算标准正态法置信区间
z <- qnorm(0.975)  # 95% 置信区间的 z 值
ci_normal <- c(mean_time - z * se, mean_time + z * se)

# 计算基本法置信区间
basic_ci <- quantile(bootstrap_means, c(0.025, 0.975))

# 计算百分位法置信区间
percentile_ci <- quantile(bootstrap_means, c(0.025, 0.975))

# 计算 BCa 置信区间
library(boot)
bca_ci <- boot.ci(boot.out = boot(data = x, statistic = function(data, indices) {
    lambda_hat <- length(data) / sum(data[indices])
    return(1 / lambda_hat)
}, R = B), type = "bca")

# 输出置信区间
ci_normal
basic_ci
percentile_ci
bca_ci$bca[4:5]  # BCa 区间


## -----------------------------------------------------------------------------
library(bootstrap)
data <- scor
n<-nrow(data)
val<-numeric(n)
theta.jack<-numeric(5)
theta<- function(data){
  svd_result <- svd(data)
  val = svd_result$d
  val_sorted <- sort(val, decreasing = TRUE)
  return(val_sorted[1]/sum(val))
}

for(i in 1:n){
  theta.jack[i] <- theta(data[-i])
}

theta.hat<-theta(data)
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
sd_error<- sd(theta.jack)

cat("theta",theta.hat,"\n")
cat("bias",bias.jack,"\n")
cat("stand error",sd_error)

## -----------------------------------------------------------------------------
library(DAAG)
X <- ironslag$chemical
Y <- ironslag$magnetic
n <- length(Y)
linear_model <- function(X, Y) { lm(Y ~ X) }
quadratic_model <- function(X, Y) { lm(Y ~ poly(X, 2)) }
cubic_model <- function(X, Y) { lm(Y ~ poly(X, 3)) }
exponential_model <- function(X, Y) { lm(log(Y) ~ X) }

cv_error <- function(model) {
  errors <- numeric(n)
  for (i in 1:n) {
    t <- model(X[-i], Y[-i])
    prediction <- predict(t, newdata = data.frame(X = X[i]))
    errors[i] <- (Y[i] - prediction)^2
  }
  return(mean(errors))
}

expo_cv <- function(model) {
  errors <- numeric(n)
  for (i in 1:n) {
    t <- model(X[-i], Y[-i])
    prediction <- predict(t, newdata = data.frame(X = X[i]))
    errors[i] <- (Y[i] - exp(prediction))^2
  }
  return(mean(errors))
}

linear_cv <- cv_error(linear_model)
quadratic_cv <- cv_error(quadratic_model)
cubic_cv <- cv_error(cubic_model)
exponential_cv <- expo_cv(exponential_model)
cat("Linear CV Error:", linear_cv,"\n")
cat("Quadratic CV Error", quadratic_cv,"\n")
cat("Cubic CV Error", cubic_cv,"\n")
cat("Expoential CV Error", exponential_cv,"\n\n")

adj_r2 <- function(model) {
  return(summary(model)$adj.r.squared)
}

linear_r2 <- adj_r2(lm(Y ~ X))
quadratic_r2 <- adj_r2(lm(Y ~ poly(X, 2)))
cubic_r2 <- adj_r2(lm(Y ~ poly(X, 3)))
exponential_r2 <- adj_r2(lm(log(Y) ~ X))

cat("Linear Adjusted R^2", linear_r2,"\n")
cat("Quadratic Adjusted R^2", quadratic_r2,"\n")
cat("Cubic Adjusted R^2",cubic_r2,"\n")
cat("Expoentical Adjusted R^2",exponential_r2,"\n")
#根据例题7.18的方法应该选择Qudratic的方法，而根据adjusted R^2应该选择Expoential
#的方法


## -----------------------------------------------------------------------------
set.seed(123)
library(cramer)

attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

cramer_von_mises<- cramer.test(x, y)$statistic

permutation_test <- function(x, y) {
  data <- c(x,y)
  m <- length(x)
  perm_stats <- numeric(1e3)
  
  for (i in 1:1e3) {
    data1 <- sample(data,size=length(data),replace = FALSE)
    t <- data1[1:m]
    w <- data1[(m + 1):length(data)]
    perm_stats[i] <- cramer.test(t, w)$statistic
  }
  p_value <- mean(perm_stats >= cramer_von_mises)
  return(p_value)
}

p_value <- permutation_test(x, y)

# 输出结果
cat("Cramér-von Mises Test Statistic:", cramer_von_mises, "\n")
cat("Permutation Test P-value:", p_value, "\n")


## -----------------------------------------------------------------------------
#由于题目没有给出参数，故需要生成数据
set.seed(123)  
x <- rnorm(100)   
y <- 0.5 * x + rnorm(100) 
  
cor0 <- cor(x, y, method = "spearman")  
  
R <- 1e4 
t <- numeric(R)  
 
for (i in 1:R) {  
  w <- sample(y,size=length(y),replace=FALSE)  
  # 计算打乱后的Spearman秩相关系数  
  t[i] <- cor(x, w, method = "spearman")  
}  
  
p_value <- mean(t >= cor0)  

cat("Original Spearman correlation:", cor0, "\n")  
cat("Permutation test p-value:", p_value, "\n")

## -----------------------------------------------------------------------------
answer_9_3 <- function(n, discard) {
  # 初始化：存储采样的向量，起始值为0
  samples <- numeric(n)
  samples[1] <- 0
  
  # Metropolis-Hastings采样过程
  for (i in 2:n) {
    # 生成候选值
    proposal <- samples[i - 1] + rnorm(1, mean = 0, sd = 1)
    # 计算接受比率
    acceptance_ratio <- dcauchy(proposal) / dcauchy(samples[i - 1])
    # 根据接受比率决定是否接受候选值
    if (runif(1) < acceptance_ratio) {
      samples[i] <- proposal
    } else {
      samples[i] <- samples[i - 1]
    }
  }
  
  # 丢弃前 discard 个样本，计算样本的分位数
  samples <- samples[(discard + 1):n]
  sample_deciles <- quantile(samples, probs = seq(0.1, 0.9, by = 0.1))
  theoretical_deciles <- qcauchy(seq(0.1, 0.9, by = 0.1))
  
  # 输出结果
  cat("Deciles of generated observations:\n", sample_deciles, "\n")
  cat("Deciles of standard Cauchy distribution:\n", theoretical_deciles, "\n")
}

# 设定随机种子并调用函数
set.seed(123)
answer_9_3(10000, 1000)

## -----------------------------------------------------------------------------
answer_9_8 <- function(n, discard, a, b, n_val) {
  # 初始化：x 和 y 样本向量，设置初始值
  x_samples <- numeric(n)
  y_samples <- numeric(n)
  x_samples[1] <- 5
  y_samples[1] <- 0.5
  
  # Gibbs 采样过程
  for (i in 2:n) {
    # Step 1: 根据当前的 y 值更新 x 的条件分布：Binomial(n_val, y)
    y_current <- y_samples[i - 1]
    x_samples[i] <- rbinom(1, size = n_val, prob = y_current)
    
    # Step 2: 根据当前的 x 值更新 y 的条件分布：Beta(x + a, n_val - x + b)
    x_current <- x_samples[i]
    y_samples[i] <- rbeta(1, shape1 = x_current + a, shape2 = n_val - x_current + b)
  }
  
  # 丢弃前 discard 个样本，绘制后续的联合分布图
  plot(x_samples[(discard + 1):n], y_samples[(discard + 1):n], 
       pch = 20, main = "Joint Density f(x, y)", xlab = "x", ylab = "y")
}

# 设置随机种子并调用函数
set.seed(123)
answer_9_8(10000, 1000, 2, 2, 10)

## -----------------------------------------------------------------------------
converge_9_3 <- function(n, discard, n_chain) {
  # 初始化
  chains <- matrix(NA, nrow = n, ncol = n_chain)
  # 运行多条马尔可夫链
  for (j in 1:n_chain) {
    chain <- numeric(n)
    chain[1] <- 0
    for (i in 2:n) {
      proposal <- chain[i - 1] + rnorm(1, mean = 0, sd = 1)
      acceptance_ratio <- dcauchy(proposal) / dcauchy(chain[i - 1])
      if (runif(1) < acceptance_ratio) {
        chain[i] <- proposal
      } else { chain[i] <- chain[i - 1] }
    }
    chains[, j] <- chain
  }
  # 转换为mcmc.list对象
  mcmc_chains <- mcmc.list(lapply(1:n_chain, function(j) mcmc(chains[(discard + 1):n, j])))
  # Gelman-Rubin收敛诊断
  gelman_diag <- gelman.diag(mcmc_chains)$psrf[, "Point est."]
  cat("convergence of chains:", gelman_diag, "\n")
}

## -----------------------------------------------------------------------------
converge_9_8 <- function(n, discard, a, b, n_val, n_chain) {
  # 初始化
  x_chains <- matrix(NA, nrow = n, ncol = n_chain)
  y_chains <- matrix(NA, nrow = n, ncol = n_chain)
  # 运行多条马尔可夫链
  for (j in 1:n_chain) {
    x_chain <- numeric(n)
    y_chain <- numeric(n)
    x_chain[1] <- 5
    y_chain[1] <- 0.5
    for (i in 2:n) {
      y_current <- y_chain[i - 1]
      x_chain[i] <- rbinom(1, size = n_val, prob = y_current)
      x_current <- x_chain[i]
      y_chain[i] <- rbeta(1, shape1 = x_current + a, shape2 = n_val - x_current + b)
    }
    x_chains[, j] <- x_chain
    y_chains[, j] <- y_chain
  }
  # 转换为mcmc.list对象
  x_mcmc_chains <- mcmc.list(lapply(1:n_chain, function(j) mcmc(x_chains[(discard + 1):n, j])))
  y_mcmc_chains <- mcmc.list(lapply(1:n_chain, function(j) mcmc(y_chains[(discard + 1):n, j])))
  # Gelman-Rubin收敛诊断
  gelman_diag_x <- gelman.diag(x_mcmc_chains)$psrf[, "Point est."]
  gelman_diag_y <- gelman.diag(y_mcmc_chains)$psrf[, "Point est."]
  cat("convergence of x_chains:", gelman_diag_x, "\n")
  cat("convergence of y_chains:", gelman_diag_y, "\n")
}

## -----------------------------------------------------------------------------
library(coda)
set.seed(123)
converge_9_3(10000, 1000, 4)
converge_9_8(10000, 1000, 2, 2, 10, 4)

## -----------------------------------------------------------------------------
answer_11_3 <- function(a, d, max_k) {
  # 计算第 k 项
  compute_term <- function(a, d, k) {
    norm_a <- sqrt(sum(a^2))  # 向量 a 的范数
    term <- ((-1)^k / (factorial(k) * 2^k)) * 
            (norm_a^(2 * k + 2) / ((2 * k + 1) * (2 * k + 2))) * 
            (gamma((d + 1) / 2) * gamma(k + 3 / 2) / gamma(k + d / 2 + 1))
    return(term)
  }
  
  # 计算和返回总和
  compute_sum <- function(a, d, max_k) {
    # 使用 sapply 计算所有项并求和
    summation <- sum(sapply(0:max_k, compute_term, a = a, d = d))
    return(summation)
  }
  
  # 输出结果
  result <- compute_sum(a, d, max_k)
  cat("The sum when a =", toString(a), "is:", result, "\n")
}

# 调用函数
answer_11_3(c(1, 2), 2, 10)

## Question 11.5

## -----------------------------------------------------------------------------
answer_11_5 <- function(k_values) {
  # function of the integral
  ratio <- function(k) {
    return(2 * gamma((k + 1) / 2) / (sqrt(pi * k) * gamma(k / 2)))
  }
  ck <- function(a, k) {
    return(sqrt((a^2 * k) / (k + 1 - a^2)))
  }
  integral_left <- function(a, k) {
    result <- integrate(function(u) (1 + u^2 / (k - 1))^(-k / 2), lower = 0, upper = ck(a, k - 1))
    return(result$value)
  }
  integral_right <- function(a, k) {
    result <- integrate(function(u) (1 + u^2 / k)^(-(k + 1) / 2), lower = 0, upper = ck(a, k))
    return(result$value)
  }
  # solve the equation for a
  find_a <- function(k) {
    f <- function(a) {
      left_term <- ratio(k - 1) * integral_left(a, k)
      right_term <- ratio(k) * integral_right(a, k)
      return(abs(left_term - right_term))
    }
    solution <- optimize(f, interval = c(0.01, sqrt(k)))$minimum
    return(solution)
  }
  # 输出结果
  a_values <- sapply(k_values, find_a)
  print(data.frame(k = k_values, a = a_values))
}

## -----------------------------------------------------------------------------
answer_11_5(c(4, 6, 8, 10))

## -----------------------------------------------------------------------------
em_algo <- function(epsilon, max_iter) {
  Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
  tau <- 1
  lambda <- 1
  for (iter in 1:max_iter) {
    # E-step
    uncensor <- Y[Y < tau]
    censor <- Y[Y == tau]
    n_uncensor <- length(uncensor)
    n_censor <- length(censor)
    E_T_censor <- tau + 1 / lambda
    # M-step
    lambda_iter <- (n_uncensor + n_censor) / (sum(uncensor) + n_censor * E_T_censor)
    if (abs(lambda_iter - lambda) < epsilon) {
      lambda <- lambda_iter
      break }
    lambda <- lambda_iter
  }
  # 输出结果
  cat("the E-M algorithm to estimate λ:", lambda, "\n")
  cat("the observed data MLE:", 1 / mean(Y), "\n")
}

## -----------------------------------------------------------------------------
em_algo(1e-6, 100)

## -----------------------------------------------------------------------------
# 加载 lpSolve 包
if (!require(lpSolve)) install.packages("lpSolve")
library(lpSolve)

# 定义目标函数系数 (c)
objective <- c(4, 2, 9)

# 定义约束矩阵 A
constraints <- matrix(c(
  2, 1, 1,  # 2x + y + z
  1, -1, 3  # x - y + 3z
), nrow = 2, byrow = TRUE)

# 定义右侧常数 b
rhs <- c(2, 3)

# 定义约束类型 (<=)
constraint_directions <- c("<=", "<=")

# 使用 lpSolve 进行优化
result <- lp(
  direction = "min",        # 最小化问题
  objective.in = objective, # 目标函数
  const.mat = constraints,  # 约束矩阵
  const.dir = constraint_directions, # 约束方向
  const.rhs = rhs           # 右侧常数
)

# 提取结果
solution <- result$solution
optimal_value <- result$objval

# 打印最优解和目标值
solution
optimal_value

## -----------------------------------------------------------------------------
# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 使用 for 循环拟合模型
models_for <- list()
for (i in seq_along(formulas)) {
  models_for[[i]] <- lm(formulas[[i]], data = mtcars)
}

# 使用 lapply() 拟合模型
models_lapply <- lapply(formulas, function(f) lm(f, data = mtcars))

# 输出结果
models_for
models_lapply

## -----------------------------------------------------------------------------
# 创建 bootstrap 样本列表
bootstrap_samples <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[rows, ]
})

# 定义一个函数来拟合模型
fit_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

# 使用 for 循环拟合模型
bootstrap_models_for <- list()
for (i in seq_along(bootstrap_samples)) {
  bootstrap_models_for[[i]] <- fit_model(bootstrap_samples[[i]])
}

# 使用 lapply() 拟合模型
bootstrap_models_lapply <- lapply(bootstrap_samples, fit_model)

# 输出结果
bootstrap_models_for
bootstrap_models_lapply

## -----------------------------------------------------------------------------
# 定义提取 R^2 的函数
rsq <- function(mod) summary(mod)$r.squared

# 提取问题 3 中的模型的 R^2
r_squared_for <- sapply(models_for, rsq)
r_squared_lapply <- sapply(models_lapply, rsq)

# 提取问题 4 中的 bootstrap 模型的 R^2
r_squared_bootstrap_for <- sapply(bootstrap_models_for, rsq)
r_squared_bootstrap_lapply <- sapply(bootstrap_models_lapply, rsq)

# 输出 R^2 值
r_squared_for
r_squared_lapply
r_squared_bootstrap_for
r_squared_bootstrap_lapply

## -----------------------------------------------------------------------------
# 模拟 t 检验的 100 次试验
set.seed(123) # 设置随机种子以保证结果可复现
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

# 查看 trials 的前几个结果
head(trials)

## -----------------------------------------------------------------------------
# 使用匿名函数提取 p 值
p_values_anonymous <- sapply(trials, function(x) x$p.value)

# 查看前几个 p 值
head(p_values_anonymous)

## -----------------------------------------------------------------------------
# 使用 [[ 直接提取 p 值
p_values_direct <- sapply(trials, `[[`, "p.value")

# 查看前几个 p 值
head(p_values_direct)

## -----------------------------------------------------------------------------
# 验证两种方法是否一致
all.equal(p_values_anonymous, p_values_direct)

## -----------------------------------------------------------------------------
# 加载 ggplot2 包
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 创建 p 值数据框
p_values_df <- data.frame(p_value = p_values_direct)

# 绘制 p 值分布直方图
ggplot(p_values_df, aes(x = p_value)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(
    title = "Distribution of p-values from t-tests",
    x = "p-value",
    y = "Frequency"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
# 定义快速卡方统计量计算函数
fast_chisq <- function(x, y) {
  # 检查输入是否为数值向量且无缺失值
  stopifnot(is.numeric(x), is.numeric(y), !anyNA(x), !anyNA(y))
  
  # 创建联合频率表
  observed <- table(x, y)
  
  # 计算行和、列和和总和
  row_sums <- rowSums(observed)
  col_sums <- colSums(observed)
  total <- sum(observed)
  
  # 计算期望频数
  expected <- outer(row_sums, col_sums) / total
  
  # 计算卡方统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  
  return(chisq_stat)
}

## -----------------------------------------------------------------------------
# 定义快速 table 函数
fast_table <- function(x, y) {
  # 检查输入是否为整数向量且无缺失值
  stopifnot(is.integer(x), is.integer(y), !anyNA(x), !anyNA(y))
  
  # 获取 x 和 y 的取值范围
  x_levels <- sort(unique(x))
  y_levels <- sort(unique(y))
  
  # 初始化频数矩阵
  freq_table <- matrix(0, nrow = length(x_levels), ncol = length(y_levels),
                       dimnames = list(x_levels, y_levels))
  
  # 填充频数矩阵
  for (i in seq_along(x)) {
    freq_table[as.character(x[i]), as.character(y[i])] <- 
      freq_table[as.character(x[i]), as.character(y[i])] + 1
  }
  
  return(freq_table)
}

## -----------------------------------------------------------------------------
# 定义结合快速 table 的卡方统计量计算
fast_chisq_with_table <- function(x, y) {
  # 检查输入是否为整数向量且无缺失值
  stopifnot(is.integer(x), is.integer(y), !anyNA(x), !anyNA(y))
  
  # 使用 fast_table 生成频率表
  observed <- fast_table(x, y)
  
  # 计算行和、列和和总和
  row_sums <- rowSums(observed)
  col_sums <- colSums(observed)
  total <- sum(observed)
  
  # 计算期望频数
  expected <- outer(row_sums, col_sums) / total
  
  # 计算卡方统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  
  return(chisq_stat)
}

## -----------------------------------------------------------------------------
# 生成随机数据
set.seed(123)
x <- sample(1:5, 1000, replace = TRUE)
y <- sample(1:5, 1000, replace = TRUE)

# 比较性能
system.time({
  chisq_test_stat <- chisq.test(x, y)$statistic
})

system.time({
  fast_stat <- fast_chisq(x, y)
})

system.time({
  fast_stat_table <- fast_chisq_with_table(as.integer(x), as.integer(y))
})

# 验证结果一致性
all.equal(chisq_test_stat, fast_stat)
all.equal(chisq_test_stat, fast_stat_table)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
gibbs_sampler <- function(n, a, b, iter, x0, y0) {
  # 初始化变量
  x <- numeric(iter)
  y <- numeric(iter)
  x[1] <- x0
  y[1] <- y0
  
  for (t in 2:iter) {
    # 1. 给定 y，从 Binomial(n, y) 中采样 x
    x[t] <- rbinom(1, n, y[t-1])
    
    # 2. 给定 x，从 Beta(x + a, n - x + b) 中采样 y
    y[t] <- rbeta(1, x[t] + a, n - x[t] + b)
  }
  
  return(data.frame(x = x, y = y))
}

# 示例参数
n <- 10
a <- 2
b <- 3
iter <- 1000
x0 <- 5
y0 <- 0.5

# 运行采样
samples <- gibbs_sampler(n, a, b, iter, x0, y0)
plot(samples$x, samples$y, main = "Gibbs采样结果", xlab = "x", ylab = "y")

## -----------------------------------------------------------------------------
# 加载必要包
library(ggplot2)
library(microbenchmark)

# 生成随机数
set.seed(123)
custom_samples <- gibbs_sampler(n, a, b, iter, x0, y0)$y  # 使用自定义函数生成
default_samples <- rbeta(iter, a, b)  # 使用默认Beta分布采样

# 绘制 Q-Q 图
qqplot(custom_samples, default_samples, main = "Q-Q Plot: Custom vs Default",
       xlab = "Custom Gibbs Samples", ylab = "Default Beta Samples")
abline(0, 1, col = "red")


## -----------------------------------------------------------------------------
# 使用 microbenchmark 比较两种方法的速度
benchmark_results <- microbenchmark(
  custom = gibbs_sampler(n, a, b, iter, x0, y0),
  default = rbeta(iter, a, b),
  times = 100
)

# 打印基准测试结果
print(benchmark_results)

# 可视化性能对比
boxplot(benchmark_results, main = "Performance Comparison",
        ylab = "Execution Time (ms)")

