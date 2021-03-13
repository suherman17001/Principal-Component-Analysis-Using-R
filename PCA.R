##SYNTAX PCA##
PCA = function(data,type=c("Covariance","Correlation")){
if (type=="Covariance"){
cat("Principal Component Analysis using Covariance Matrix\n")
cat("\n")
covarians.func=function(data){
matrix.cov=cov(data)
cat("Covariance Matrix\n")
print(matrix.cov)
cat("\n")
eigen.cov=eigen(matrix.cov)
vector.cov=t(eigen.cov$vectors)
ne.cov=(eigen.cov$values)
cat("Eigen Values\n")
print(eigen.cov$values)
cat("\n")
cat("Eigen Vectors\n")
print(eigen.cov$vectors)
cat("\n")
cat("Determining the Number of Main Components:\n")
cat("\n")
ComponentNumber = c(1:ncol(data))
EigenValueCov = ne.cov
scree.plot = plot(ComponentNumber,EigenValueCov,type='l')
cat("1. Scree Plot\n")
cat("Look at the chart!\n")
scree.plot
cat("\n")
evalue=function(data,p){
prop = rep(1,p)
for (i in 1:p){
prop[i]=(data[i])/(sum(data))*100
}
print(prop)
}
cat("2a. Proportion of the Influence of the Main Components to the Data\n")
proporsi.cov=evalue(ne.cov,ncol(data))
cat("\n")
sum.cumulative=function(data,p){
x = rep(1,p)
for (i in 1:p){
x[i]=sum(data[1:i])
}
print(x)
}
cat("2b. The Cumulative Proportion of the Effect of the Main Components on the Data\n")
sum.cumulative(proporsi.cov,length(proporsi.cov))
cat("\n")
matrix.covfunc=function(x,y,z,p){
output=matrix(nrow=p,ncol=p)
for (i in 1:p){
for (j in 1:p){
output[i,j]=x[j,i]*sqrt(y[i])/z[j,j]
}
}
print(output)
}
cat("MATRIX OF COVARIANCE between Y and X variables\n")
matrix.covfunc(vector.cov,ne.cov,matrix.cov,ncol(matrix.cov))
cat("\n")
}
covarians.func(data)
}
else {
cat("Main Component Analysis with CORRELATION MATRIX\n")
cat("\n")
corr.func=function(data){
matrix.cor=cor(data)
cat("Covariance Matrix\n")
print(matrix.cor)
cat("\n")
eigen.cor=eigen(matrix.cor)
vector.cor=t(eigen.cor$vectors)
ne.cor=(eigen.cor$values)
cat("Eigen Values\n")
print(eigen.cor$'values')
cat("\n")
cat("Eigen Vectors\n")
print(eigen.cor$vectors)
cat("\n")
cat("Determining the Number of Main Components:\n")
cat("\n")
ComponentNumber = c(1:ncol(data))
EigenValueCor = ne.cor
scree.plot = plot(ComponentNumber,EigenValueCor,type='l')
cat("1. Scree Plot\n")
cat("Look at the chart!\n")
scree.plot
cat("\n")
evalue=function(data,p){
prop = rep(1,p)
for (i in 1:p){
prop[i]=(data[i])/(sum(data))*100
}
print(prop)
}
cat("2a. Proportion of the Influence of the Main Components to the Data\n")
proporsi.cor=evalue(ne.cor,ncol(data))
cat("\n")
sum.cumulative=function(data,p){
x = rep(1,p)
for (i in 1:p){
x[i]=sum(data[1:i])
}
print(x)
}
cat("2b. The Cumulative Proportion of the Effect of the Main Components on the Data\n")
sum.cumulative(proporsi.cor,length(proporsi.cor))
cat("\n")
matrix.corfunc=function(x,y,z,p){
output=matrix(nrow=p,ncol=p)
for (i in 1:p){
for (j in 1:p){
output[i,j]=x[j,i]*sqrt(y[i])/z[j,j]
}
}
print(output)
}
cat("MATRIX OF CORRELATION between Y and Z variables\n")
matrix.corfunc(vector.cor,ne.cor,matrix.cor,ncol(matrix.cor))
cat("\n")
}
corr.func(data)
}
}
-------------------------------------------------------------
PCA(data,type="Covariance")
PCA(data,type="Correlation")
