dimcheck <- function(x) {
  test <- list.files("UCI HAR Dataset", recursive = TRUE, pattern = "test.*.txt", full.names = TRUE)
  train <- list.files("UCI HAR Dataset", recursive = TRUE, pattern = "train.*.txt", full.names = TRUE)
  testnames <- list.files("UCI HAR Dataset", recursive = TRUE, pattern = "test.*.txt")
  trainnames <- list.files("UCI HAR Dataset", recursive = TRUE, pattern = "train.*.txt")
  final <- data.frame()
  if(x == "test") {
    for(i in 1:12) {
      testdata <- read.table(test[i])
      nrow <- nrow(testdata)
      ncol <- ncol(testdata)
      dimm <- cbind(nrow, ncol)
      final <- rbind(final, dimm)
    }
    final <- cbind(testnames, final)
  }
  if(x == "train") {
    for(i in 1:12) {
      traindata <- read.table(train[i])
      nrow <- nrow(traindata)
      ncol <- ncol(traindata)
      dimm <- cbind(nrow, ncol)
      final <- rbind(final, dimm)
    }
    final <- cbind(trainnames, final)
  }
    final
}