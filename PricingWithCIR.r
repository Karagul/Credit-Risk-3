library(sde)
X0=5
N=252 
t0=0
T=100
M=1
theta1=0.559
theta2=0.238
theta3=0.074
X<-sde.sim(X0=X0, N=N, M=M, t0=t0, T=T, theta=c(theta1, theta2, theta3), model="CIR") 
plot(X)

X0=1
N=252 
t0=0
T=100
M=100
theta1=0.559
theta2=0.238
theta3=0.074
X<-sde.sim(X0=X0, N=N, M=M, t0=t0, T=T, theta=c(theta1, theta2, theta3), model="CIR") 
dt=(T-t0)/N
X=exp(-dt*X)
X.mean = rowMeans(X)
X.sd   = apply(X,1,sd)
plot(as.vector(time(X)),X.mean,type="l",xlab="time",ylab="value") 
lines(as.vector(time(X)),X.mean + (1.96*X.sd)/sqrt(M),col = "purple") 
lines(as.vector(time(X)),X.mean - (1.96*X.sd)/sqrt(M),col = "purple") 


default <- function (i,j,X,dt) { 
  if (j <= i) {
  return(1) }
  if (j == i+1) {
    return (mean(exp(-dt*X[j,])))
  }
  return (mean(exp(-dt*colSums(X[(i+1):j,])))) 
}
l = matrix(1,N,N)
for (i in 1:N) { 
  for (j in 1:N) {
  l[i,j] = default(i, j, X, dt) }
}
mean(l)
image (1:N,1:N,log(l))




X0=10
N=252 
t0=0
T=1
M=1
theta1=0.00125
theta2=0.25
theta3=0.1
X<-sde.sim(X0=X0, N=N, M=M, t0=t0, T=T, theta=c(theta1, theta2, theta3), model="CIR") 
plot(X)

X0=10
N=252 
t0=0
T=1
M=100
theta1=0.00125
theta2=0.25
theta3=0.1
X<-sde.sim(X0=X0, N=N, M=M, t0=t0, T=T, theta=c(theta1, theta2, theta3), model="CIR") 
dt=(T-t0)/N
X.mean = rowMeans(X)
X.sd   = apply(X,1,sd)
plot(as.vector(time(X)),X.mean,type="l",xlab="time",ylab="value") 
lines(as.vector(time(X)),X.mean + (1.96*X.sd)/sqrt(M),col = "purple") 
lines(as.vector(time(X)),X.mean - (1.96*X.sd)/sqrt(M),col = "purple") 

default <- function (i,j,X,dt) { 
  if (j <= i) {
    return(1) }
  if (j == i+1) {
    return (mean(exp(-dt*X[j,])))
  }
  return (mean(exp(-dt*colSums(X[(i+1):j,])))) 
}
l = matrix(1,N,N)
for (i in 1:N) { 
  for (j in 1:N) {
    l[i,j] = default(i, j, X, dt) }
}
image (1:N,1:N,log(l))



X0=10
N=100
t0=0
T=12
M=1000
theta=c(0.559, 0.238, 0.074)
X0a=10
Na=100
t0a=0
Ta=12
Ma=1000
thetaa=c(0.514878, 0.082, 0.67) 
X <- sde.sim(X0=X0, N=N, M=M, t0=t0, T=T, theta=theta, model="CIR")
Y <- sde.sim(X0=X0a, N=Na, M=Ma, t0=t0a, T=Ta, theta=thetaa, model="CIR") 
W=X+Y
dt=(T-t0)/N
W.mean = rowMeans(W)
W.sd = apply(W,1,sd) 
plot(as.vector(time(W)),W.mean,type="l",xlab="time",ylab="value") 
lines(as.vector(time(W)),W.mean + (1.96*W.sd)/sqrt(M), col="purple") 
lines(as.vector(time(W)),W.mean - (1.96*W.sd)/sqrt(M),col="purple")
default <- function (i,j,W,dt) { if (j <= i) {
  return(1) }
  if (j == i+1) {
    return (mean(exp(-dt*W[j,])))
  }
  return (mean(exp(-dt*colSums(W[(i+1):j,])))) }
l = matrix(1,N,N)
for (i in 1:N) { for (j in 1:N) {
  l[i,j] = default(i, j, W, dt)
}
}
image (1:N,1:N,log(l))
