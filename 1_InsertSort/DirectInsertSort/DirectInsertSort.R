main <- function() {
  y <- c( 6.2, 2.2, 7.2, 4.0, 9.0, 0.8, 5.0, 6.0 )  #待排序数据，需注意下标都是从1开始
  print("Before Sort : ")
  print(y)
  
  y <- DirectInsertSort(y)
  
  print("After Sort : ")
  print(y)
}


DirectInsertSort <- function(list) {
  size <- length(list)
  
  for (i in c(2:size)) {
    if (list[c(i)] < list[c(i-1)]) {
      temp <- list[c(i)]
      list[c(i)] <- list[c(i-1)]
      j <- i - 2
      while (j > 0 && list[c(j)] > temp) {
        list[c(j+1)] <- list[c(j)]
        j <- j - 1
      }
      list[c(j+1)] = temp
    }
  }
  
  returnValue(list)
}

main()
