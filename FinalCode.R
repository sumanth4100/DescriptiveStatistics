#To generate 100 random numbers form a sample data ranging from 1 to 100000 
RandomNumbers <- c(sample(1:100000, 10, replace=F)) #floor(runif(3, min=0, max=101))
print(paste("RandomNumbers : ",RandomNumbers))
#To count number of elements in vector
Length <- function(input)
{
  Count <- 0
  for (i in input) 
    {
    Count <- Count+1   
  }
  Count
}
VectorLength <- Length(RandomNumbers)  #Length <- length(RandomNumbers)
print(paste("Vector Length : ",VectorLength))
#To find Minimum,Maximum from a vector
MinMax <- function(RV)
{
  Min <- RandomNumbers[1]
  Max <- RandomNumbers[1]
  for (i in RandomNumbers) 
  {
    if(i < Min) {
      Min <- i           # print(min(RandomNumbers,na.rm=FALSE))
    }
    else if(i > Max ) {
      Max <- i           # print(max(RandomNumbers,na.rm=FALSE))
    }
  }
  return(c(Min,Max))
}
MinMax.Output <- MinMax(RandomNumbers)
Minimum <- MinMax.Output[1]
Maximum <- MinMax.Output[2]
print(paste("Minimum Number :",Minimum))
print(paste("Maximum Number :",Maximum))
#To find sum of elements in a vectora
VectorSum <- function(input)
{
  Sum <- 0
  for (i in input) 
  {
    #print(i)
    Sum <- Sum+i
    #print(Sum)
  }
  return(Sum)
}
Sum <- VectorSum(RandomNumbers)      #sum(RandomNumbers)
print(paste("Sum of elements :",Sum))
#To sort the elements in a vector by using BubbleSort Algorithm
BubbleSort <- function(input)
{
  Length <- Length(input) 
  for (k in Length:2) 
  {
    i <- 1       
    while (i < k)        
    {
      if (input[i] > input[i+1]) 
      {
        Temp <- input[i+1]
        input[i+1] <- input[i]
        input[i] <- Temp
      }
      i <- i+1          
    }
  }
  return(input)
}
Sort <- BubbleSort(RandomNumbers) #sort(RandomNumbers)
print(paste("Sorted Vector :",Sort))
#To find median of a vector
Median <- function(input)
{
  Length <- Length(input) 
  if(Length%%2==0){ #To check if number of elements in vector is even is or odd
    Median <- (input[Length/2]+input[(Length/2)+1])/2 #If number of elements is even
  }else{
    Median <- input[Length/2] #If number of elements is odd
  }
  return(Median)
}
Median <- Median(Sort) #print(median(Sort, na.rm = FALSE))
print(paste("Median :",Median))
#To Calculate Arithmetic Mean of a vector
Mean <- function(input)
{
  sum  <- VectorSum(input)
  Length <- Length(input)
  Mean <- sum/Length
  return(Mean)
}
ArithmeticMean <- Mean(Sort)   # print(mean(RandomNumbers,na.rm=FALSE))
print(paste("Arithmetic Mean :",ArithmeticMean))
#To find Squareroot of a number using Babylonian Method
SquareRoot <- function(n)
{
  Temp1 <- n
  Temp2 <- 1
  Temp3 <- 0.000001
  while(Temp1-Temp2 > Temp3)
  {
    Temp1 <- (Temp1+Temp2)/2
    Temp2 <- n/Temp1
  }
  return(Temp1)
}
#To Calculate Standard Deviation  of a vector by welford's method
Deviation <- function(input)
{
  sd_sum <- 0
  Len <- Length(input)
  for(j in input)
  {
    new <- j-ArithmeticMean
    final <- new*new
    if(final>0)
    {
      final <- final
    }else {
      final <- final*-1
    }
    sd_sum=sd_sum+final
  }
  return(SquareRoot(sd_sum/(Len-1)))  #print(sd(RandomNumbers,na.rm=FALSE))
}
StandardDeviation <- Deviation(Sort)   # print(mean(RandomNumbers,na.rm=FALSE))
print(paste("Standard Deviation:",StandardDeviation))

#To print all DescriptiveStatistics
print(paste("RandomNumbers : ",RandomNumbers))
print(paste("Vector Length : ",VectorLength))
print(paste("Minimum Number :",Minimum))
print(paste("Maximum Number :",Maximum))
print(paste("Median :",Median))
print(paste("Arithmetic Mean :",ArithmeticMean))
print(paste("Standard Deviation:",StandardDeviation))