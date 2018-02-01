t <- c(0,0.67,0.84,1.28,1.65,2.32,2.58,3.09,3.72)
n <- c(100,1000,10000)

# true value
ture_Phi <- pnorm(t)

# Prediction function
Pred_Phi_100 <- function(x){
  y <- rnorm(100);
  m <- sum(y<=x);
  m/100
}

Pred_Phi_1000 <- function(x){
  y <- rnorm(1000);
  m <- sum(y<=x);
  m/1000
}

Pred_Phi_10000 <- function(x){
  y <- rnorm(10000);
  m <- sum(y<=x);
  m/10000
}

## One-time experiment
# try sapply function
Pred_1_100 <- sapply(t,Pred_Phi_100)
Pred_1_1000 <- sapply(t,Pred_Phi_1000)
Pred_1_10000 <- sapply(t,Pred_Phi_10000)
final_data_1 <- data.frame(t = t, `Ture Value`= ture_Phi, `n = 100` = Pred_1_100, `n = 1000` = Pred_1_1000,
                           `n = 10000` = Pred_1_10000, stringsAsFactors = FALSE, check.names = FALSE)

## 100-time experiment
# T <- matrix(rep(t,each = 100), nrow = 100)
T <- rep(t,each = 100)
# Ture_Phi <- matrix(rep(ture_Phi,each = 100), nrow = 100)
# n = 100
Pred_100_100 <- sapply(T,Pred_Phi_100)
Pred100_100 <- array(Pred_100_100,c(100,9)) #reshape

# n = 1000
Pred_100_1000 <- sapply(T,Pred_Phi_1000)
Pred100_1000 <- array(Pred_100_1000,c(100,9)) #reshape

# n = 10000
Pred_100_10000 <- sapply(T,Pred_Phi_10000)
Pred100_10000 <- array(Pred_100_10000,c(100,9)) #reshape

final_data_100 <- data.frame(t = T, Pred_100 = Pred_100_100, Pred_1000 = Pred_100_1000, Pred_10000 = Pred_100_10000)

## box plots for predictions
#png(file = "boxplot.png")
#boxplot(Pred_100 ~ t, data = final_data_100, xlab = "t",ylab = "Predictions", main = "100-run Predictions with 100 samples")
#dev.off()
#boxplot(Pred_1000 ~ t, data = final_data_100, xlab = "t",ylab = "Predictions", main = "100-run Predictions with 1000 samples")
#boxplot(Pred_10000 ~ t, data = final_data_100, xlab = "t",ylab = "Predictions", main = "100-run Predictions with 10000 samples")

## box plots for predictions' bias
# mean of predictions
mean100 <- apply(Pred100_100,2,mean)
mean1000 <- apply(Pred100_1000,2,mean)
mean10000 <- apply(Pred100_10000,2,mean)
Mean100 <- rep(mean100,each = 100)
Mean1000 <- rep(mean1000,each = 100)
Mean10000 <- rep(mean10000,each = 100)
bias100 <- Pred_100_100 - Mean100
bias1000 <- Pred_100_1000 - Mean1000
bias10000 <- Pred_100_10000 - Mean10000
final_data_bias <- data.frame(t = T, bias_100 = bias100, bias_1000 = bias1000, bias_10000 = bias10000)
#boxplot(bias_100 ~ t, data = final_data_bias, xlab = "t",ylab = "Prediction Bias", main = "100-run Prediction Bias with 100 samples")