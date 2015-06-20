run_analysis <- function () {
  library(dplyr)
  data_path <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/"
  features <- read.table(paste(data_path, "features.txt", sep=""))
  activity_labels <- read.table(paste(data_path, "activity_labels.txt", sep=""))
  x_test <- read.table(paste(data_path, "test/X_test.txt", sep=""))
  y_test <- read.table(paste(data_path, "test/y_test.txt", sep=""))
  x_train <- read.table(paste(data_path, "train/X_train.txt", sep=""))
  y_train <- read.table(paste(data_path, "train/y_train.txt", sep=""))
  colnames(x_test) <- features[,2]
  colnames(x_train) <- features[,2]
  x_test_mean_std <- x_test[grepl("tBodyAcc-mean\\(|tBodyAcc-std|tBodyGyro-mean\\(|tBodyGyro-std", names(x_test))]
  x_train_mean_std <- x_train[grepl("tBodyAcc-mean\\(|tBodyAcc-std|tBodyGyro-mean\\(|tBodyGyro-std", names(x_train))]
  x_mean_std <- rbind(x_test_mean_std, x_train_mean_std)
  y_mean_std <- rbind(y_test, y_train)
  y_mean_std_acty_name <- vector(mode="character", length = nrow(y_mean_std))
  xy_mean_std <- cbind(Activity=y_mean_std, x_mean_std)
  num_features = ncol(x_mean_std)
  avg_data_set <- matrix(nrow=6, ncol=num_features)
  for (i in 1:num_features) {
    xy_mean_std_avg <- tapply(xy_mean_std[,i+1], xy_mean_std[,1], mean, simplify=TRUE)
    avg_data_set[,i] <- as.matrix(xy_mean_std_avg, nrow=6, ncol=1)
  }
  c_names <- colnames(x_mean_std)
  avg_data_set <- cbind(as.integer(rownames(xy_mean_std_avg)), avg_data_set)
  colnames(avg_data_set) <- c("Activity", colnames(x_mean_std))
  
  ## Melt dimention X, Y, Z
  dimx <- avg_data_set[,c(1,2,5,8,11)]
  dimy <- avg_data_set[,c(1,3,6,9,12)]
  dimz <- avg_data_set[,c(1,4,7,10,13)]
  dimx <- cbind(dimx, c("X"))
  dimy <- cbind(dimy, c("Y"))
  dimz <- cbind(dimz, c("Z"))
  data_set_dim <- rbind(dimx, dimy, dimz)
  colnames(data_set_dim) <- c("Activity", "Value.Body.Acc.Mean", "Value.Body.Acc.Std", "Value.Body.Gyro.Mean", "Value.Body.Gyro.Std", "Dimention")
  
  ## Melt measure type Mean or Std
  measure_mean <- data_set_dim[, c(1,2,4,6)]
  measure_std <- data_set_dim[, c(1,3,5,6)]
  measure_mean <- cbind(measure_mean, c("Mean"))
  measure_std <- cbind(measure_std, c("Standard Deviation"))
  data_set_dim_measure <- rbind(measure_mean, measure_std)
  colnames(data_set_dim_measure) <- c("Activity", "Value.Body.Acc", "Value.Body.Gyro", "Dimention", "Statistic.Measure")
  
  ## Melt sensor name Acc or Gyro
  sensor_acc <- data_set_dim_measure[, c(1, 2, 4, 5)]
  sensor_gyro <- data_set_dim_measure[, c(1, 3, 4, 5)]
  sensor_acc <- cbind(sensor_acc, c("Accelerometer"))
  sensor_gyro <- cbind(sensor_gyro, c("Gyroscope"))
  data_set_dim_measure_sensor <- rbind(sensor_acc, sensor_gyro)
  colnames(data_set_dim_measure_sensor) <- c("Activity", "Avg.Body.Signal.Value", "Dimention", "Statistic.Measure", "Sensor.Type")
  df_data_set_dim_measure_sensor <- as.data.frame(data_set_dim_measure_sensor)
  df_data_set_dim_measure_sensor <- mutate(df_data_set_dim_measure_sensor, Activity.Name=as.character(activity_labels[Activity, 2]))
  tidy_data_set_dim_measure_sensor <- df_data_set_dim_measure_sensor[, 2:6]
  write.table(tidy_data_set_dim_measure_sensor, "tidy_data_set_dim_measure_sensor.txt", row.name=FALSE)
  tidy_data_set_dim_measure_sensor
}