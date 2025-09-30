#Importing Libraries
library(ggplot2) #For plotting
library(cowplot) #For improved plotting
library(randomForest) #Random Forest

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=FALSE) #Assigning a dataframe for the url
head(data) #Header without proper titles

colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
) #Assigning category titles

head(data) #Data with category titles


#Checking for structure of data
str(data)

#Correcting terminology within the dataset
data[data == "?"] <- NA
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex) #Converting into a column vector

data$cp <- as.factor(data$cp) #Converting into a column vector
data$fbs <- as.factor(data$fbs) #Converting into a column vector
data$restecg <- as.factor(data$restecg) #Converting into a column vector
data$exang <- as.factor(data$exang) #Converting into a column vector
data$slope <- as.factor(data$slope) #Converting into a column vector

data$ca <- as.integer(data$ca) #Changing from string to integer
data$ca <- as.factor(data$ca) #Converting into a column vector

data$thal <- as.integer(data$thal) #Changing from string to integer
data$thal <- as.factor(data$thal) 

data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) 

str(data)



#Random sampling is needed
set.seed(42)
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)

#Random forest
model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)
model

#Error Data
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Healthy"],
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#In general, as the number of trees increases, the error reduces.
#But will this continue?

model <- randomForest(hd ~., data=data.imputed, ntree=1000, proximity=TRUE)
model


#Error Data
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Healthy"],
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#After roughly 500 trees the error rate stabilises, so adding more trees didn't help

#Would changing the number of variables at each node improve results?
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
} #Creating a loop which examines whether 3 is the optimal value

oob.values
# 2 and 3 are equivalently optimal

distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
  X=mds.values[,1],
  Y=mds.values[,2],
  Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
  geom_text(aes(color=Status)) +
  theme_bw() + 
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")