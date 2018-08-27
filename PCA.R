set.seed(10)
x<-rnorm(60)
dim(x)<-c(10,6)
pc<-function(x){
  meanx<-apply(x,2,mean)
  sdx<-apply(x,2,sd)
  
  dz2<-dim(x)[2]
  z<-x
  for(i in 1:dz2){
    z[,i]=(x[,i]-meanx[i])/sdx[i]
  }
  S<-cov(z)
  z.p<-princomp(z,cor=TRUE)
  summary(z.p,loadings=TRUE)
  d<-svd(S)$d
  prop<-svd(S)$d/sum(d)
  newz<-z%*%svd(S)$u
  
  u<-svd(S)$u
  colnames(u)<-c(1:dz2)
  colnames(u)[1]<-"print1"
  rownames(u)<-c(1:dz2)
  rownames(u)[1]<-"z1"
  
  ricky<-list(prop,u,newz)    # pc為原來向量的組合  組合係數是u   newz=principal component為組合後的向量觀察值
  return(ricky)
}

pc(x)
