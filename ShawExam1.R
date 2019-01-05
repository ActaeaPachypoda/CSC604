# Sarah Shaw
# CSC 604 - Scientific Computing with R
# 7/14/2018
# Exam 1

# Question 1
# (a) Write a function to count the number of even numbers in a vector of integers 
# without using the modulus operator, i.e., %%. You cannot use any R-defined functions, 
# e.g., floor(). You may only use any number of operations including addition, subtraction, 
# division of any kind, and multiplication. You cannot use "exponentiation" either. 
# You are not allowed to use any loop. Recursion can be used. 
# You may also use any form of the apply() function.

Problem1a <- function()
{
  rm(list = ls())
  print ("**** Problem #1a ****")
  # evenList <- NULL
  n <- 1:10
  is_Even <- function(x)
  {
    if ((x %/% 2) * 2 == x)
    {
      return(TRUE)
    }
    else
    {
      return(FALSE)
    }
  }
  Even_Count <- function(x){
    evenCount <- 0
    if (is_Even(x) == TRUE)
      {
      evenCount <- evenCount + 1
      # evenList <- c(evenList, x)
      } else {evenCount = 0}
    }
  evenCount <- sum(sapply(n, Even_Count))
  evenCount
}
# (b) Use your function to count the number of even numbers in the 
# Nile dataset that came with RStudio. (Hint: You may use length() or sum().) 
Problem1b <- function()
  {
  print("**** Problem #1b ****")
  evenCount <- 0
  evenCount <- sum(sapply(Nile, Even_Count))
  evenCount
  }

# Question 2
# Write an R program which assigns an odd number, say 9, 
# (you can use an assignment to assign a valid odd number to a variable, 
# e.g., n, and your function returns the following vectors:
# (a)  (1, 3, 5, 7, 9, 7, 5, 3, 1)
Problem2a <- function()
  {
  rm(list = ls())
  
  print("**** Problem #2a ****")
  n <- 9
  numList <- seq(1, (n-1), by = 2)
  numList <- c(numList, n, rev(numList))
  print(numList)
  
  }
# (b)  Using n = 5, your function returns a vector containing three 1's, 
# followed by three 2's, and then three 3's, etc., until three 5's

Problem2b <- function()
  {
  rm(list = ls())
  
  print("**** Problem #2b ****")
  n <- 5
  y <- 1:n
  i <- 1
  numList <- (NULL)
  
  for (x in y) 
    {
    numList <- c(numList, rep(i, 3))
    i <- i+1
    }
  print(numList)
  }
#Question 3
# Write an assignment statement to complete each of the following without using
# any loop, diff() and sign(): 
# (a) generate a vector of the difference between the ith element
# and the (i+1)th element in a vector v of integers, where i = 1: (length(v)-1) 

Problem3a <- function()
{
  rm(list = ls())
  
  print("**** Problem #3a ****")
  v <- c(1, 4, 7, 2, 1)
  space <- 1
  counter <- 5
  lst <- NULL
  if (counter > 0 && space <= length(v))
    {
      lst[space] <- v[space] - v[(space + 1)]
      counter <- counter - 1
      space <- space + 1
    }
  if (counter > 0 && space <= length(v))
    {
    lst[space] <- v[space] - v[(space + 1)]
    counter <- counter - 1
    space <- space + 1
  }    
  if (counter > 0 && space <= length(v))
    {
    lst[space] <- v[space] - v[(space + 1)]
    counter <- counter - 1
    space <- space + 1
  }    
  if (counter > 0 && space <= length(v))
    {
    lst[space] <- v[space] - v[(space + 1)]
    counter <- counter - 1
    space <- space + 1
  }

  print(lst)
  
  }
# (b) generate a vector of the sign of each elements in the vector generated in (a) 
# as follows:'p' for positive, 'n' for negative. 
# Again, you cannot use any loops. (Use v <- c(1,4,7,2,1)) 

Problem3b <- function()
  {
 
  print("**** Problem #3b ****")
  v <- c(1,4,7,2,1)
  counter <- length(lst)
  space <- 1
  posLst <- NULL
  if(counter>0 && space <= length(lst))
  {
    if(lst[space]>0)
      posLst[space] <- "p"
    else
      posLst[space] <- "n"
    counter <- counter - 1
    space <- space + 1
  }
  if(counter>0 && space <= length(lst))
  {
    if(lst[space]>0)
      posLst[space] <- "p"
    else
      posLst[space] <- "n"
    counter <- counter - 1
    space <- space + 1
  }
  if(counter>0 && space <= length(lst))
  {
    if(lst[space]>0)
      posLst[space] <- "p"
    else
      posLst[space] <- "n"
    counter <- counter - 1
    space <- space + 1
  }
  if(counter>0 && space <= length(lst))
  {
    if(lst[space]>0)
      posLst[space] <- "p"
    else
      posLst[space] <- "n"
    counter <- counter - 1
    space <- space + 1
  }
  if(counter>0 && space <= length(lst))
  {
    if(lst[space]>0)
      posLst[space] <- "p"
    else
      posLst[space] <- "n"
    counter <- counter - 1
    space <- space + 1
  }
  print(posLst)
  
  }
# Question 4
#  Write a function that assigns an integer value, say 7, to an integer n and
# generate a special matrix where the diagonal elements are all zeros, and the elements
# following the 0 value are consecutive odd numbers. For example, the first row of a 5X5
# matrix contains "0, 1, 3, 5, 7"; the second row contains "1, 0, 3, 5, 7"; the third row
# contains "1, 3, 5, 0, 7", etc. You may use any function that we have used in our lectures
# to write your function. (You cannot either hard-code the matrix; it needs to be
# generated, or use any other function such as cumsum() or toeplitz().)
Problem4 <- function()
{
  rm(list = ls())
  
  print("**** Problem #4 ****")
  n <- 7
  x <- c(0,seq(1, n, by = 2))
  y <- matrix(c(rep(0, 5),1, rep(0,4),1,2,rep(0,3),1,rep(2,2),rep(0,2),1,rep(2,3),0),nrow = 5, ncol = 5, 
              byrow = TRUE)
  y1 <- matrix(x, nrow = 5, ncol = 5, byrow = TRUE)
  y2 <- y1 - diag(diag(y1))
  final <- y2 + y
  final
}
