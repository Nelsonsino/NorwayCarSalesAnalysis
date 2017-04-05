salesCal <- function(x, dataset){
  salesbymake <- data.frame(Make = character(),
                            Quantity = integer(),
                            stringsAsFactors = FALSE)
  for(i in 1:length(x)){
    dataset$Make <- as.character(dataset$Make)
    sales <- subset(dataset, Make == x[i])
    salesbymake[i, 1] <- x[i]
    salesbymake[i, 2] <- sum(sales$Quantity)
  }
  salesbymake <- salesbymake[order(salesbymake$Quantity),]
  return(salesbymake)
}

drawPie <- function(x){
  label <- paste(tail(x$Make, n = 10), "\n", tail(x$Quantity, n = 10))
  pie(tail(x$Quantity, n = 10), label, edges = 300)
}

sampleGen <- function(x, y){
  x$Make <- as.character(x$Make)
  y$Make <- as.character(y$Make)
  sample <- data.frame(a = character(),
                       b = character(),
                       stringsAsFactors = FALSE)
  counter <- 0
  for(i in 2007:2017){
    salesbyyear1 <- subset(x, Year == i)
    salesbyyear2 <- subset(y, Year == i)
    if(i < 2017){
      for(j in 1:12){
        salesbymonth1 <- subset(salesbyyear1, Month == j)
        salesbymonth2 <- subset(salesbyyear2, Month == j)
        sample[counter + j, 1] <- head(salesbymonth1$Make, n = 1)
        sample[counter + j, 2] <- head(salesbymonth2$Make, n = 1)
      }
    }else{
      sample[counter + 1, 1] <- head(salesbyyear1$Make, n = 1)
      sample[counter + 1, 2] <- head(salesbyyear2$Make, n = 1)
    }
    counter <- counter + 12
  }
  return(sample)
}

jaccard <- function(x){
  #There is a whitespace following the make name in data2, gsub is used to delete the whitespace
  x$b <- gsub(" ","",x$b)
  setunion <- union(x$a, x$b)
  setintersect <- intersect(x$a, x$b)
  return(length(setintersect) / length(setunion))
}
