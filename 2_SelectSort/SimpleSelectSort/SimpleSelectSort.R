main <- function() {
  y <- c( 6.2, 2.2, 7.2, 4.0, 9.0, 0.8, 5.0, 6.0 )  #待排序数据，需注意下标都是从1开始
  print("Before Sort : ")
  print(y)
  
  y <- SimpleSelectSort(y)
  
  print("After Sort : ")
  print(y)
}


SimpleSelectSort <- function(list) {
  size <- length(list)
  
  for (i in c(1:(size-1))) {  #c(1:size-1) 代表( 0, 1, ... , size - 1 )
    min <- i
    for (j in c((i+1), size)) {
      if (list[c(j)] < list[c(min)]) {
        min <- j
      }
    }
    if (min != i) {
      list[c(i)] <- list[c(i)] + list[c(min)]
      list[c(min)] <- list[c(i)] - list[c(min)]
      list[c(i)] <- list[c(i)] - list[c(min)]
    }
  }
  
  returnValue(list)
}

main()
