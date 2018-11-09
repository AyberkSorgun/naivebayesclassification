#Ayberk Sorgun, 50397
#COMP421 HW01
setwd("~/Desktop/hw01")
data_set <- read.csv("hw01_data_set_images.csv",header = FALSE)

#safelog <- function(x) {
#  x[x == 0] <- 1
#  return (log(x))}

safelog <- function(x) {
  return (log(x + 1e-100))
}


train_a <- data_set[1:25,]
train_b <- data_set[40:64,]
train_c <- data_set[79:103,]
train_d <- data_set[118:142,]
train_e <- data_set[157:181,]
test_a <- data_set[26:39,]
test_b <- data_set[65:78,]
test_c <- data_set[104:117,]
test_d <- data_set[143:156,]
test_e <- data_set[182:195,]

training_set <- rbind(train_a,train_b,train_c,train_d,train_e)
test_set <- rbind(test_a,test_b,test_c,test_d,test_e)

names(test_set) <- NULL
names(training_set) <- NULL
names(train_a) <- NULL
names(train_b) <- NULL
names(train_c) <- NULL
names(train_d) <- NULL
names(train_e) <- NULL
names(test_a) <- NULL
names(test_b) <- NULL
names(test_c) <- NULL
names(test_d) <- NULL
names(test_e) <- NULL



sumofa <- colSums(train_a)/25
sumofb <- colSums(train_b)/25
sumofc <- colSums(train_c)/25
sumofd <- colSums(train_d)/25
sumofe <- colSums(train_e)/25

pcd <- data.matrix(cbind(sumofa,sumofb,sumofc,sumofd,sumofe))

#print(pcd[,1])
#print(pcd[,2])
#print(pcd[,3])
#print(pcd[,4])
#print(pcd[,5])

#TRAINING
training_set <- data.matrix(training_set)

score_func_A <- sapply(X = 1:125, FUN = function(c) {training_set[c,]%*%safelog(pcd[,1])+(1-training_set[c,])%*%safelog(1-pcd[,1])+safelog(1/5)})
score_func_B <- sapply(X = 1:125, FUN = function(c) {training_set[c,]%*%safelog(pcd[,2])+(1-training_set[c,])%*%safelog(1-pcd[,2])+safelog(1/5)})
score_func_C <- sapply(X = 1:125, FUN = function(c) {training_set[c,]%*%safelog(pcd[,3])+(1-training_set[c,])%*%safelog(1-pcd[,3])+safelog(1/5)})
score_func_D <- sapply(X = 1:125, FUN = function(c) {training_set[c,]%*%safelog(pcd[,4])+(1-training_set[c,])%*%safelog(1-pcd[,4])+safelog(1/5)})
score_func_E <- sapply(X = 1:125, FUN = function(c) {training_set[c,]%*%safelog(pcd[,5])+(1-training_set[c,])%*%safelog(1-pcd[,5])+safelog(1/5)})
score_func <- rbind(score_func_A, score_func_B, score_func_C, score_func_D, score_func_E)

prediction <- sapply(X=1:125, FUN = function(c) {match(max(score_func[,c]),score_func)})
prediction <- (prediction-1)%%5+1

conf_A <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction[1:25],c)))})
conf_B <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction[26:50],c)))})
conf_C <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction[51:75],c)))})
conf_D <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction[76:100],c)))})
conf_E <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction[101:125],c)))})
confusion_matrix_train <- t(rbind(conf_A,conf_B,conf_C,conf_D,conf_E))
confusion_matrix_train

#TEST
test_set <- data.matrix(test_set)

score_func_A_test <- sapply(X = 1:70, FUN = function(c) {test_set[c,]%*%safelog(pcd[,1])+(1-test_set[c,])%*%safelog(1-pcd[,1])+safelog(1/5)})
score_func_B_test <- sapply(X = 1:70, FUN = function(c) {test_set[c,]%*%safelog(pcd[,2])+(1-test_set[c,])%*%safelog(1-pcd[,2])+safelog(1/5)})
score_func_C_test <- sapply(X = 1:70, FUN = function(c) {test_set[c,]%*%safelog(pcd[,3])+(1-test_set[c,])%*%safelog(1-pcd[,3])+safelog(1/5)})
score_func_D_test <- sapply(X = 1:70, FUN = function(c) {test_set[c,]%*%safelog(pcd[,4])+(1-test_set[c,])%*%safelog(1-pcd[,4])+safelog(1/5)})
score_func_E_test <- sapply(X = 1:70, FUN = function(c) {test_set[c,]%*%safelog(pcd[,5])+(1-test_set[c,])%*%safelog(1-pcd[,5])+safelog(1/5)})
score_func_test <- rbind(score_func_A_test, score_func_B_test, score_func_C_test, score_func_D_test, score_func_E_test)

prediction_test <- sapply(X=1:70, FUN = function(c) {match(max(score_func_test[,c]),score_func_test)})
prediction_test <- (prediction_test-1)%%5+1

conf_A_test <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction_test[1:14],c)))})
conf_B_test <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction_test[15:28],c)))})
conf_C_test <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction_test[29:42],c)))})
conf_D_test <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction_test[43:56],c)))})
conf_E_test <- sapply(X=1:5, FUN=function(c) {sum(!is.na(match(prediction_test[57:70],c)))})
confusion_matrix_test <- t(rbind(conf_A_test,conf_B_test,conf_C_test,conf_D_test,conf_E_test))
confusion_matrix_test

