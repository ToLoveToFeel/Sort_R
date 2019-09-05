main <- function() {
  y <- c( 6.2, 2.2, 7.2, 4.0, 9.0, 0.8, 5.0, 6.0 )  #待排序数据，需注意下标都是从1开始
  a <- c( 5, 3, 1 )
  print("Before Sort : ")
  print(y)
  
  for (i in a) {
    y <- ShellSort(y, i)
  }
  
  print("After Sort : ")
  print(y)
}


ShellSort <- function(list, d) {
  size <- length(list)
  for (i in c(d+1:size)) {
    if (i > size) {   #c(9:7)代表9 8 7
      break
    }
    if (list[c(i)] < list[c(i-d)]) {
      temp <- list[c(i)]
      list[c(i)] <- list[c(i-d)]
      j <- i - 2 * d
      while (j > 0 && list[c(j)] > temp) {
        list[c(j+1)] <- list[c(j)]
        j <- j - d
      }
      list[c(j+d)] = temp
    }
  }
  returnValue(list)
}

main()
