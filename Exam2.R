# Sarah Shaw
# CSC 604 - Scientific Computing with R
# 7/26/2018
# Exam 2

# Question 1

Problem1 <- function()
{
  rm(list = ls())
  print ("**** Problem #1 ****")
  n = 6
  
  d1.Kids <- c("Jack", "Jill", "Jillian", "John", "James")
  d1.States <- c("CA", "MA", "DE", "HI", "PA")
  d1 <- data.frame(Names = (d1.Kids), States = (d1.States))
  
  d2.Ages <- c(10,7,12,30)
  d2.Kids <- c("Jill", "Jillian", "Jack", "Mary")
  d2 <- data.frame(Names = (d2.Kids) ,Ages = (d2.Ages))
  
  kids.Names <- union(d1.Kids,d2.Kids)
  kids.Names <- unique(kids.Names)
  length(d1.States) <- n
  length(d2.Ages) <- n
  d3 <- data.frame(Names = (kids.Names), States = (d1.States), Ages = (d2.Ages))
  d3
}

# Question 2

Problem2 <- function()
{
  rm(list = ls())
  
  print ("**** Problem #2 ****")
  
  par(mfrow = c(1,2))
  t <- seq(0,10,.01)
  
  #Left
  r <- 2*sin(2*t)
  x1 <- r*cos(t)
  y1 <- r*sin(t)
  plot(x1,y1, type = "l", col = "green", main = "r=2sin(2 theta)")
  abline(v = 0, h = 0, col = "red")
  
  #Right
  r <- 1+1.5*cos(t)
  x2 <- r*cos(t)
  y2 <- r*sin(t)
  plot(x2,y2,type = "l", col= "green", main = "r = 1 + 1.5 cos(theta)")
  abline(v = 0, h = 0, col = "red")
}

# Question 3

Problem3a <- function()
{
  rm(list = ls())
  
  print ("**** Problem #3a ****")
  
  premierships <- list(
    Adelaide = c(1997, 1998),
    Carlton = c(1906, 1907, 1908, 1914, 1915, 1938, 1945, 1947, 1968, 1970, 1972, 1979, 1981, 1982, 1987, 1995),
    Collingwood = c(1902, 1903, 1910, 1917, 1919, 1927, 1928, 1929, 1930, 1935, 1936, 1953, 1958, 1990),
    Essendon = c(1897, 1901, 1911, 1912, 1923, 1924, 1942, 1946, 1949, 1950, 1962, 1965, 1984, 1985, 1993, 2000),
    Fitzroy_Brisbane = c(1898, 1899, 1904, 1905, 1913, 1916, 1922, 1944, 2001, 2002, 2003),
    Footscray_W.B. = c(1954),
    Fremantle = c(),
    Geelong = c(1925, 1931, 1937, 1951, 1952, 1963, 2007),
    Hawthorn = c(1961, 1971, 1976, 1978, 1983, 1986, 1988, 1989, 1991, 2008),
    Melbourne = c(1900, 1926, 1939, 1940, 1941, 1948, 1955, 1956, 1957, 1959, 1960, 1964),
    N.Melb_Kangaroos = c(1975, 1977, 1996, 1999),
    PortAdelaide = c(2004),
    Richmond = c(1920, 1921, 1932, 1934, 1943, 1967, 1969, 1973, 1974, 1980),
    StKilda = c(1966),
    S.Melb_Sydney = c(1909, 1918, 1933, 2005),
    WestCoast = c(1992, 1994, 2006)
  )
  
  # What are the names of the teams that won the premiership the most?
  mostWins <- sort(sapply(premierships, length))
  print("The three teams with the most premiership wins are: ")
  mostWins <- c(mostWins[length(mostWins)],mostWins[length(mostWins)-1],mostWins[length(mostWins)-2])
  print(mostWins)
  
  print ("**** Problem #3b ****")
  # What year was the most recent premiership games?
  
  mostRecent <- max(sapply(premierships, max))
  print ("The most recent Premiership was in: ")
  print (mostRecent)
  
  print ("**** Problem #3c ****")
  #Which team won the premeirship in the most recent games?
  print("The team to win the most recent games was: ")
  nameRecent <- sort(sapply(premierships, max))
  nameRecent <- nameRecent[length(nameRecent)]
  print(nameRecent)
}

#Question 4

Problem4 <- function()
{
  rm(list = ls())

  library("lattice")
  
  print ("**** Problem #4 ****")

  ufc.plots <- read.csv("/School/2018/2 Summer/CSC 604 (Scientific Computing with R)/spuRs/resources/data/ufc.csv")
  str(ufc.plots)

  y <- ufc.plots$height.m
  x <- ((ufc.plots$dbh.cm)/100)
  z <- (2*pi)*(x/2)*y
  
  pt2 <- lattice::cloud(z~x*y, data = ufc.plots, xlab = "Diameter (m)", 
                        ylab = "Height (m)", zlab = "Suface (m^2)", screen = list(z = 20, x = -75),
                         panel.aspect = 0.75, pch = 1)
  pt2
  }

