# Sarah Shaw
# CSC 604 - Scientific Computing with R
# 8/2/2018
# Exam 3

question.2 <- function(){
  rm(list = ls())
  #Question 2
  source('spuRs/resources/scripts/newtonraphson.r')
  source('spuRs/resources/scripts/Phi.r')
  
  print("**** Question 2 ****")
  p <- .05
  ftnx <- function(p)
  {
    y <- Phi(p)
    z <- phi(p)
    return(c(z, y))
  }
  print("p = .05")
  newtonraphson(ftn = ftnx, 0, tol = 1e-9)
  
  print("p = .95")
  p <- .95
  newtonraphson(ftnx, 0, tol = 1e-9)
  
  print("p = .975")
  p.3 <- .975
  newtonraphson(ftnx, 0, tol = 1e-9)
  
  print("p = .99")
  p.4 <- .99
  newtonraphson(ftnx, 0, tol = 1e-9)
}
question.3 <- function(){

  #Question 3
  
  rm(list = ls())
  
  print ("**** Problem #3 ****")
  
  #Data
  ufc.plots <-
    read.csv(
      "/School/2018/2 Summer/CSC 604 (Scientific Computing with R)/spuRs/resources/data/ufc.csv"
    )
  str(ufc.plots)
  y <- ufc.plots$height.m
  x <- ufc.plots$dbh.cm
  
  #Scatterplot
  plot(
    x,
    y,
    xlab = "dbh.com",
    ylab = "Height (m)",
    type = 'p',
    main = "Estimated Height"
  )
  
  #Regression Line
  abline(lm(y ~ x))
  
  # summary for finding regression formula
  # summary(lm(y~x))
  
  x.1 <- seq(70, 100, by = 10)
  y.formula <- 12.10768 + (.32389) * x.1
  points(x.1, y = y.formula, type = 'p', pch = 19)
  
}
question.4 <- function()
{
  rm(list = ls())
  #Question 4
  print("**** Question 4 ****")
  source('spuRs/resources/scripts/forest_fire.r')
  
  x <- forest.fire(X,.2,.4,FALSE)
  
  plot (0:max_n, pmf, type = "h", main = 'Forest Fire Simulation', ylab = 'Duration to Stop')
}