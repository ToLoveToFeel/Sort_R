main <- function() {
  y <- c( 6.2, 2.2, 7.2, 4.0, 9.0, 0.8, 5.0, 6.0 )  #待排序数据，需注意下标都是从1开始
  print("Before Sort : ")
  print(y)
  
  y <- BinaryInsertSort(y)
  
  print("After Sort : ")
  print(y)
}


BinaryInsertSort <- function(list) {
  size <- length(list)
  for (i in c(2:size)) {
    temp <- list[c(i)]
    high <- i - 1
    low <- 1
    while (low <= high) {
      middle <- as.integer((high + low) / 2)
      if (temp > list[c(middle)]) {
        low <- middle + 1
      } else {
        high <- middle - 1
      }
    }
    j <- i - 1
    while(j >= high + 1) {
      list[c(j+1)] = list[c(j)]
      j <- j - 1
    }
    list[c(high+1)] = temp
  }
  
  returnValue(list)
}

main()
