run_analysis <- function(folder="UCI HAR Dataset/") {
     X_train <- read.table(paste(folder,"train/X_train.txt",sep=""), quote="\"")
     y_train <- read.table(paste(folder,"train/y_train.txt",sep=""), quote="\"")
     subject_train <- read.table(paste(folder,"train/subject_train.txt",sep=""), quote="\"")
     features <- read.table(paste(folder,"features.txt",sep=""), quote="\"")
     X_test <- read.table(paste(folder,"test/X_test.txt",sep=""), quote="\"")
     y_test <- read.table(paste(folder,"test/y_test.txt",sep=""), quote="\"")
     subject_test <- read.table(paste(folder,"test/subject_test.txt",sep=""), quote="\"")
     subject <- rbind(subject_train, subject_test)
     X <- rbind(X_train, X_test)
     y <- rbind(y_train, y_test)
     selection <- c(grep("mean()", features[,2], fixed = T), grep("std()", features[,2], fixed = T))
     activity_labels <- read.table(paste(folder,"activity_labels.txt",sep=""), quote="\"")
     x <- X[,selection]
     names(x) <- features[selection,2]
     names(subject) <- "subject"
     names(y) <- "activity_ID"
     res <- cbind(subject, y, x)
     names(activity_labels) <- c("activity_ID", "activity")
     Res <- merge(activity_labels, res, by.x = "activity_ID", by.y = "activity_ID")
     t <- aggregate(Res[,-(1:3)], list(Res$activity_ID, Res$subject), mean)
     names(t)[1:2] <- c("activity_ID","subject")
     T5 <- merge(activity_labels, t, by.x = "activity_ID", , by.y = "activity_ID")
     write.table(T5, "step5_data.txt", row.names = F)
     return(T5)
}