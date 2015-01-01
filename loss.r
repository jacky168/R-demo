library(ggplot2)
library(reshape2)

matrix_element_max <-function(ele, mat) {
  mat[mat >= ele] = ele
  return(mat)
}

logistic_loss <- function(t, y) {
  return(log2(1 + exp(-t*y)))
}

#logistic_loss_grad <- function(t, y) {
#  return(-1*y/(exp(y*t+1)))  
#}

hinge_loss <-function (t, y){
  mat = 1 - t*y
  mat[mat <= 0] = 0
  return(mat)
}

squared_hinge_loss <-function(t, y) {
  return(hinge_loss(t,y) * hinge_loss(t,y))
}

squared_loss <- function(t, y){
  return(0.5*(t - y)*(t - y) )
}


zero_one_loss <-function(t, y){
  result = rep(0,401)
  one_idx = t*y <= 0
  zero_idx = t*y > 0
  result[zero_idx] = 0
  result[one_idx] = 1
  
  return(result)
}

t <- seq(-2,2,by=0.01)
y = rep(1,401)


zero_one_loss(t,y)

#df <- data.frame(x = t, Logistic_Loss = logistic_loss(t,y), Squared_Loss = squared_loss(t,y), Zero_One_Loss = zero_one_loss(t,y), Hinge_Loss = hinge_loss(t,y), Squared_Hinge_Loss = squared_hinge_loss(t, y))
df <- data.frame(x = t, Logistic_Loss = logistic_loss(t,y), Zero_One_Loss = zero_one_loss(t,y), Hinge_Loss = hinge_loss(t,y), Squared_Hinge_Loss = squared_hinge_loss(t, y))

losses <- melt(df, id.vars = "x")
g <- ggplot(losses, aes(x=t, y=value, colour=variable)) 
g + geom_line()

#plot(t,zero_one_loss(t,y), type='l', xlab='t', ylab='loss', main = 'Loss Function')
#plot(t,squared_loss(t,y), type='l', xlab='t', ylab='loss', main = 'Loss Function')
#plot(t,hinge_loss(t,y), type='l', xlab='t', ylab='loss', main = 'Loss Function')

