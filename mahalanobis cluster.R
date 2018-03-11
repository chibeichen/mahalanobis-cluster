# random number
a<-runif(100,0,100)
b<-runif(100,0,10)
x<-cbind(a,b)

?mahalanobis

mahc<-function(x){
  center<-apply(x, 2, mean)  # mean
  center<-as.matrix(center)
  
  cov<-matrix(rep(0,ncol(x)*ncol(x)), c(ncol(x),ncol(x)))  # p*p 0 as covariance matrix
  for (i in 1:nrow(x)) {
    cov<-(x[i,]-center)%*%t(x[i,]-center)+cov
  }  
  cov<-cov*(1/(nrow(x)-1))  # 1/(n-1) sigma((xi-mean)(xi-mean)')
  scov<-solve(cov)  # inverse matrix
  
  ma<-matrix(rep(0,nrow(x)*nrow(x)), c(nrow(x),nrow(x)))  # mahalanobis matrix
  for (i in 1:nrow(x)) {
    for (j in 1:nrow(x)) {
      ma[i,j]<-sqrt(t(x[i,]-x[j,]) %*% scov %*% (x[i,]-x[j,]))
    }
  }
  ma<-as.dist(ma)  # distance matrix
  hc<-hclust(ma)  # cluster
  return(hc)
}

hc<-mahc(x)
plot(hc)




