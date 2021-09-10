# v = vector()
# s1 = seq(1, 220, 30)
# s2 = seq(0, 220, 30)
# for(i in s1){
#   print(i)
# }
# 
 # t1 = c(1,2,3)
 # t2 = c(4,5,6)
 # t3 = c(7,8,9)
 # tm1 = matrix(cbind(t1,t2,t3), 3, 3)
# matrix(cbind(tm1, t1), 3, 4)

#install.packages("ggcorrplot")
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(MASS)


AV = c(0,20,0,0,0,0)
IV = c(30,45,60,40,40,40)
DO = c(15,20,25,15,20,25)
nsrcs = 6
N = 240
V = 441
x1 = 21
x2 = 21
vt = 0.25
vs = 0.015

TC = matrix(0, N, nsrcs)
for (i in 1:nsrcs){
  for (j in seq(AV[i], N-20, IV[i])) {
    for (k in 1:DO[i]){
      TC[j+k, i] = 1
    }
  }
}

# default standardising
TC = scale(TC)
#TC = round((TC), 2)


par(mfrow=c(2,3))
for (i in 1:nsrcs) {
  plot(TC[, i],xlab = paste(c("TC", i), collapse = " "), ylab = "",  type = 'l')
}
#because we are goinf to do regression, the assumption for regression is that
#the datas (in columns) follows normal distribution, standardisaion can scale 
#the datas into a standard normal distribution, whereas normalisation only 
#scale the data into a range(0-1) but doesn't change the distribution.
#

corr = round(cor(TC), 2)
ggcorrplot(corr, lab = TRUE)

melted_corr = melt(corr)
ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
#judging for the plot TC4, TC5, TC6 are highly correlated


tmpSM <- function(vb, vh, hb, hh) {
  m = matrix(0, x1, x2)
  m[vb:vh, hb: hh] = 1
  return(m)
}

tmpSM1 = tmpSM(2,6,2,6)
#tmpSM1 = matrix(0, x1, x2)
#tmpSM1[2:6, 2: 6] = 5
tmpSM2 = tmpSM(2,6,15,19)
tmpSM3 = tmpSM(8,13,2,6)
tmpSM4 = tmpSM(8,13,15,19)
tmpSM5 = tmpSM(15,19,2,6)
tmpSM6 = tmpSM(15,19,15,19)

par(mfrow=c(2,3))
ggcorrplot(tmpSM1)
ggarrange(ggcorrplot(tmpSM1,title = 'SM 1', legend.title = ""), 
          ggcorrplot(tmpSM2,title = 'SM 2', legend.title = ""), 
          ggcorrplot(tmpSM3,title = 'SM 3', legend.title = ""), 
          ggcorrplot(tmpSM4,title = 'SM 4', legend.title = ""), 
          ggcorrplot(tmpSM5,title = 'SM 5', legend.title = ""), 
          ggcorrplot(tmpSM6,title = 'SM 6', legend.title = ""), 
          ncol = 2, nrow = 3)

tmpSM1v = as.vector(tmpSM1)
tmpSM2v = as.vector(tmpSM2)
tmpSM3v = as.vector(tmpSM3)
tmpSM4v = as.vector(tmpSM4)
tmpSM5v = as.vector(tmpSM5)
tmpSM6v = as.vector(tmpSM6)

SM = matrix(rbind(tmpSM1v, tmpSM2v, tmpSM3v,tmpSM4v,tmpSM5v,tmpSM6v), nsrcs,V)
ggcorrplot(round(cor(t(SM)), 2),lab = TRUE)
#from the CM we can say SMs are quite uncorrelated, but
#we can conclue that SMs are independent, because
#and we don't need to standise SM because there is no 
#assumptions for SM/B/A, if Imagine one of the slice has
#pixel values of 5, while others remain at 1. the X we 
#generated is still in a linear regression. there is an
#linear relationship between X/y(TC/X) which fits the 
#assumption of linear 

generateX = function(TC, SM, vt, vs){
  noise_t = rnorm(N, 0, sqrt(vt))
  for (i in 1:nsrcs-1){
    noise_t = matrix(cbind(noise_t, rnorm(N, 0, sqrt(vt))), N, i+1)
  }
  
  noise_s = rnorm(V, 0, sqrt(vs))
  for (i in 1:nsrcs-1){
    noise_s = matrix(rbind(noise_s, rnorm(V, 0, sqrt(vs))), i+1, V)
  }
  
  X = (TC + noise_t)%*%(SM + noise_s)
  X = scale(X)
  return(X)
}
X = generateX(TC, SM, vt, vs)
#X = scale(X)

noise_t = rnorm(N, 0, sqrt(vt))
for (i in 1:nsrcs-1){
  noise_t = matrix(cbind(noise_t, rnorm(N, 0, sqrt(vt))), N, i+1)
}

noise_s = rnorm(V, 0, sqrt(vs))
for (i in 1:nsrcs-1){
  noise_s = matrix(rbind(noise_s, rnorm(V, 0, sqrt(vs))), i+1, V)
}

CM_t = cor(noise_t)
ggcorrplot(CM_t, lab = TRUE)
CM_s = cor(t(noise_s))
ggcorrplot(CM_s, lab = TRUE)

hist_t1 = ggplot(data.frame(noise = noise_t[,1]),
       aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)

hist_t2 = ggplot(data.frame(noise = noise_t[,2]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)

hist_t3 = ggplot(data.frame(noise = noise_t[,3]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)

hist_t4 = ggplot(data.frame(noise = noise_t[,4]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)

hist_t5 = ggplot(data.frame(noise = noise_t[,5]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)

hist_t6 = ggplot(data.frame(noise = noise_t[,6]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vt)),
             color="red", linetype="dashed", size=1)

ggarrange(hist_t1, 
          hist_t2, 
          hist_t3, 
          hist_t4, 
          hist_t5, 
          hist_t6, 
          ncol = 2, nrow = 3)

hist_s1 = ggplot(data.frame(noise = noise_s[1,]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)
hist_s2 = ggplot(data.frame(noise = noise_s[2,]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)

hist_s3 = ggplot(data.frame(noise = noise_s[3,]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)

hist_s4 = ggplot(data.frame(noise = noise_s[4,]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)

hist_s5 = ggplot(data.frame(noise = noise_s[5,]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)

hist_s6 = ggplot(data.frame(noise = noise_s[6,]),
                 aes(x=noise)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 2*(N^(1/3)))+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)+ 
  geom_vline(aes(xintercept=-1.96*sqrt(vs)),
             color="red", linetype="dashed", size=1)

ggarrange(hist_s1, 
          hist_s2, 
          hist_s3, 
          hist_s4, 
          hist_s5, 
          hist_s6, 
          ncol = 2, nrow = 3)

product = noise_t%*%noise_s
product_corr = round(cor(product),2)
ggcorrplot(product_corr[1:8, 1:8], lab = TRUE)
ggcorrplot(product_corr[221:228, 221:228], lab = TRUE)

X = (TC + noise_t)%*%(SM + noise_s)


Xt = TC%*%SM + product

random = runif(120, 1, 441)%/%1
random = unique(random)
time_series = X[,random]
data = data.frame(time_series)
# ggplot(data,
#        aes(x = 1:nrows(data),
#            y = value,
#            col = variable)) +
#   geom_line()
write.csv(time_series, file = "time.csv")

plot(var(X))
plot(diag(var(X)), type = 'l')

X = scale(X)
X = generateX(TC, SM, vt, vs)

Alsr = solve(t(TC)%*%TC)%*%t(TC)%*%X
Dlsr = X%*%t(Alsr)
ggcorrplot(abs(matrix(Alsr[1,], x1, x2)), title = "Retrieved Spatial Maps 1", legend.title = "")
plot(Dlsr[,1], type = 'l')
title("Retrieved Time Source 1")
ggcorrplot(abs(matrix(Alsr[2,], x1, x2)), title = "Retrieved Spatial Maps 2", legend.title = "")
plot(Dlsr[,2], type = 'l')
title("Retrieved Time Source 2")
ggcorrplot(abs(matrix(Alsr[3,], x1, x2)), title = "Retrieved Spatial Maps 3", legend.title = "")
plot(Dlsr[,3], type = 'l')
title("Retrieved Time Source 3")
ggcorrplot(abs(matrix(Alsr[4,], x1, x2)), title = "Retrieved Spatial Maps 4", legend.title = "")
plot(Dlsr[,4], type = 'l')
title("Retrieved Time Source 4")
ggcorrplot(abs(matrix(Alsr[5,], x1, x2)), title = "Retrieved Spatial Maps 5", legend.title = "")
plot(Dlsr[,5], type = 'l')
title("Retrieved Time Source 5")
ggcorrplot(abs(matrix(Alsr[6,], x1, x2)), title = "Retrieved Spatial Maps 6", legend.title = "")
plot(Dlsr[,6], type = 'l')
title("Retrieved Time Source 6")



ggarrange(ggcorrplot(matrix(Alsr[1,], x1, x2)), 
          plot(Dlsr[,1], type = 'l'),
          ncol = 2, nrow = 1)
plot(x = Dlsr[, 3], y = X[,30])
plot(x = Dlsr[, 4], y = X[,30])

Atlsr = solve(t(TC)%*%TC)%*%t(TC)%*%Xt
Dtlsr = Xt%*%t(Atlsr)
plot(x = Dtlsr[, 3], y = Xt[,30])

MSEs = c()
lambdas = seq(0, 1, 0.01)
for(i in lambdas){
  I = diag(nsrcs)
  As = solve(t(TC)%*%TC + i*I, t(TC)%*%X) #B
  Ds = X%*%t(As) #X
  mse = sum((X - Ds%*%As)^2)/(N*V)
  MSEs = c(MSEs, mse)
}
plot(lambdas, MSEs, type = "l")

lambda = 0.2
lambda_hat = lambda*V

maxcor = function(m1, m2) {
  m = c()
  ac = abs(cor(m1, m2))
  for (i in 1:ncol(ac)) {
    m = c(m, max(ac[,i], na.rm = TRUE))
  }
  return(m)
} 

Arr = solve(t(TC)%*%TC + lambda_hat*diag(nsrcs))%*%t(TC)%*%X
Drr = X%*%t(Arr)
Ctlsr = maxcor(TC, Dlsr)
Ctrr = maxcor(TC, Drr)
sum(Ctlsr)
sum(Ctrr)

lambda = 1000
lambda_hat = lambda*V
Arr = solve(t(TC)%*%TC + lambda_hat*diag(nsrcs))%*%t(TC)%*%X
Drr = X%*%t(Arr)
plot(Arr[,1])
plot(Alsr[,1])
matplot(abs(cbind(Arr[,1],Alsr[,1])), pch=19, xlab = "sources", ylab =  "absolute parameters")
legend(0.8, 1.05, c("Ridge", "Least Square"),col=seq_len(2),cex=0.8,fill=seq_len(2))

# all peneliesd towards 0, but not 0

rhos = seq(0, 1, 0.05)

LR = function(rho, X, TC) {
  step <- 1/(norm(TC %*% t(TC)) * 1.1)
  thr <- rho*N*step
  Ao <- matrix(0, nsrcs, 1)
  A <- matrix(0, nsrcs, 1)
  Alr <- matrix(0, nsrcs, x1*x2)
  
  for (k in 1:(x1*x2)) {
    
    A <- Ao+step*(t(TC) %*% (X[,k]-(TC%*%Ao)))
    A <- (1/(1+thr)) * (sign(A)*pmax(replicate(nsrcs, 0), abs(A)-thr))
    
    for (i in 1:10) {
      Ao <- A
      A <- Ao+step * (t(TC)%*%(X[,k]-(TC%*%Ao)))
      A <- (1/(1+thr)) * (sign(A)*pmax(replicate(nsrcs, 0), abs(A)-thr))
    }
    Alr[,k] <- A
  }
  return(Alr)
}

realisation = 10

MSEs = c()
for (rho in rhos) {
  MSEr = c()
  for (i in 1:realisation) {
    X = generateX(TC, SM, vt, vs)
    As = LR(rho, X, TC)
    Ds = X%*%t(As)
    mse = sum((X-Ds%*%As)^2)/(N*V)
    MSEr = c(MSEr, mse)
  }
  msea = mean(MSEr)
  MSEs = c(MSEs, msea)
}
plot(rhos, MSEs, type = "l")

#rho = 0.6
(rho = rhos[which.min(MSEs)])

lambda = 0.2
lambda_hat = lambda*V
#rho is 0.3 by elbow method
rho = 0.5
#refit X
X = generateX(TC, SM, vt, vs)
Arr = solve(t(TC)%*%TC + lambda_hat*diag(nsrcs))%*%t(TC)%*%X
Drr = X%*%t(Arr)
Alr = LR(rho, X, TC)
Dlr = X%*%t(Alr)
Ctrr = maxcor(TC, Drr)
Csrr = maxcor(t(SM), t(Arr))
Ctlr = maxcor(TC, Dlr)
Cslr = maxcor(t(SM), t(Alr))
sum(Ctrr)
sum(Csrr)
sum(Ctlr)
sum(Cslr)

plot(Drr[, 1], type = 'l')
plot(Dlr[, 1], type = 'l')
ggcorrplot(matrix(Arr[1, ], x1, x2))
ggcorrplot(matrix(Alr[1, ], x1, x2))
#plot(Alr[1, ])
par(mfrow=c(1,2))
plot(Drr[, 1], type = 'l')
title('Drr1')
plot(Dlr[, 1], type = 'l')
title('Dlr1')
ggarrange(
  ggcorrplot(abs(matrix(Arr[1, ], x1, x2)), title = 'Arr1', legend.title = ''),
  ggcorrplot(abs(matrix(Alr[1, ], x1, x2)), title = 'Alr1', legend.title = ''),
  ncol = 2)
#lasso penalies irrelavent atribute to 0
par(mfrow=c(1,2))
plot(Drr[, 2], type = 'l')
title('Drr2')
plot(Dlr[, 2], type = 'l')
title('Dlr2')
ggarrange(
  ggcorrplot(abs(matrix(Arr[2, ], x1, x2)), title = 'Arr2', legend.title = ''),
  ggcorrplot(abs(matrix(Alr[2, ], x1, x2)), title = 'Alr2', legend.title = ''),
  ncol = 2)
par(mfrow=c(1,2))
plot(Drr[, 3], type = 'l')
title('Drr3')
plot(Dlr[, 3], type = 'l')
title('Dlr3')
ggarrange(
  ggcorrplot(abs(matrix(Arr[3, ], x1, x2)), title = 'Arr3', legend.title = ''),
  ggcorrplot(abs(matrix(Alr[3, ], x1, x2)), title = 'Alr3', legend.title = ''),
  ncol = 2)
par(mfrow=c(1,2))
plot(Drr[, 4], type = 'l')
title('Drr4')
plot(Dlr[, 4], type = 'l')
title('Dlr4')
ggarrange(
  ggcorrplot(abs(matrix(Arr[4, ], x1, x2)), title = 'Arr4', legend.title = ''),
  ggcorrplot(abs(matrix(Alr[4, ], x1, x2)), title = 'Alr4', legend.title = ''),
  ncol = 2)
par(mfrow=c(1,2))
plot(Drr[, 5], type = 'l')
title('Drr5')
plot(Dlr[, 5], type = 'l')
title('Dlr5')
ggarrange(
  ggcorrplot(abs(matrix(Arr[5, ], x1, x2)), title = 'Arr5', legend.title = ''),
  ggcorrplot(abs(matrix(Alr[5, ], x1, x2)), title = 'Alr5', legend.title = ''),
  ncol = 2)
par(mfrow=c(1,2))
plot(Drr[, 6], type = 'l')
title('Drr6')
plot(Dlr[, 6], type = 'l')
title('Dlr6')
ggarrange(
  ggcorrplot(abs(matrix(Arr[6, ], x1, x2)), title = 'Arr6', legend.title = ''),
  ggcorrplot(abs(matrix(Alr[6, ], x1, x2)), title = 'Alr6', legend.title = ''),
  ncol = 2)

PC = svd(TC)
plot(PC$d, xlab = 'source')
ggcorrplot(cor(PC$u))
D <- diag(PC$d)
t = PC$u %*% D %*% t(PC$v)

Zl = eigen(TC%*%t(TC))
Zl$vectors
Zl$values


Zr = eigen(t(TC)%*%TC)
Zr$vectors
Zr$values


par(mfrow=c(6,2))
for (i in 1:nsrcs) {
  plot(PC$u[, i], ,xlab = paste(c("Z", i), collapse = " "), ylab = "",  type = 'l')
  plot(TC[, i],xlab = paste(c("TC", i), collapse = " "), ylab = "",  type = 'l')
}
par(mfrow=c(1,2))
plot(PC$u[, 1], ,xlab = paste(c("Z", 1), collapse = " "), ylab = "",  type = 'l')
plot(TC[, 1],xlab = paste(c("TC", 1), collapse = " "), ylab = "",  type = 'l')

par(mfrow=c(1,2))
plot(PC$u[, 2], ,xlab = paste(c("Z", 2), collapse = " "), ylab = "",  type = 'l')
plot(TC[, 2],xlab = paste(c("TC", 2), collapse = " "), ylab = "",  type = 'l')

par(mfrow=c(1,2))
plot(PC$u[, 3], ,xlab = paste(c("Z", 3), collapse = " "), ylab = "",  type = 'l')
plot(TC[, 3],xlab = paste(c("TC", 3), collapse = " "), ylab = "",  type = 'l')

par(mfrow=c(1,2))
plot(PC$u[, 4], ,xlab = paste(c("Z", 4), collapse = " "), ylab = "",  type = 'l')
plot(TC[, 4],xlab = paste(c("TC", 4), collapse = " "), ylab = "",  type = 'l')

par(mfrow=c(1,2))
plot(PC$u[, 5], ,xlab = paste(c("Z", 5), collapse = " "), ylab = "",  type = 'l')
plot(TC[, 5],xlab = paste(c("TC", 5), collapse = " "), ylab = "",  type = 'l')

par(mfrow=c(1,2))
plot(PC$u[, 6], ,xlab = paste(c("Z", 6), collapse = " "), ylab = "",  type = 'l')
plot(TC[, 6],xlab = paste(c("TC", 6), collapse = " "), ylab = "",  type = 'l')

Alsr = solve(t(TC)%*%TC)%*%t(TC)%*%X
Dlsr = X%*%t(Alsr)
Apcr = LR(0.001, X, PC$u)
Dpcr = X%*%t(Apcr)
plot(Dpcr[, 1], type = 'l')
ggarrange(ggcorrplot(abs(matrix(Apcr[1, ], x1, x2)), title = 'PCR', legend.title =  ''),
ggcorrplot(abs(matrix(Arr[1, ], x1, x2)), title = 'RR', legend.title =  ''),
ggcorrplot(abs(matrix(Alr[1, ], x1, x2)), title = 'LR', legend.title =  ''),
ggcorrplot(matrix(SM[1, ], x1, x2), title = 'SM', legend.title =  ''),
nrow = 2, ncol = 2)
plot(Dpcr[, 6], type = 'l')
ggcorrplot(abs(matrix(Apcr[6, ], x1, x2)))
ggcorrplot(abs(matrix((SM+noise_s)[6, ], x1, x2)))
Xpcr = Dpcr%*%Apcr
sum((X-Dpcr%*%Apcr)^2)/(N*V)
sum((X-Drr%*%Arr)^2)/(N*V)
sum((X-Dlr%*%Alr)^2)/(N*V)
sum((X-Dlsr%*%Alsr)^2)/(N*V)
sum(maxcor(X, Dpcr%*%Apcr))
sum(maxcor(X, Drr%*%Arr))
sum(maxcor(X, Dlr%*%Alr))
sum(maxcor(X, Dlsr%*%Alsr))
