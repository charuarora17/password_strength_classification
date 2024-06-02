library(ggplot2)
library(caret)
library(dplyr)

#initial dataset
datakaggle<-Kaggle.Password
dim(datakaggle)
str(datakaggle)
head(datakaggle)

#Missing Values
sum(is.na(datakaggle))

createNAs <- function (x, pctNA = 0.1) {
  n <- nrow(x)
  p <- ncol(x)
  NAloc <- rep(FALSE, n * p)
  NAloc[sample.int(n * p, floor(n * p * pctNA))] <- TRUE
  x[matrix(NAloc, nrow = n, ncol = p)] <- NA
  return(x)
}
datakaggle=createNAs(datakaggle)
summary(datakaggle)
sum(is.na(datakaggle))
head(datakaggle)

#processed dataset
dataset<-Password
dim(dataset)
str(dataset)
head(dataset)

datasetset=dataset[c(-1,-2)]
#Frequency of Length of password
ggplot(count(dataset,Length),aes(x=Length,y=n))+geom_bar(stat="identity")
ggplot(count(dataset[dataset$Length<25,],Length),aes(x=Length,y=n))+geom_bar(stat="identity")
#correlation between length and strength
cor(dataset[,c("Length","Strength")])
#boxplot
boxplot(dataset[,c(3:8)])
#removing outlier in Length
Q1 <- quantile(dataset$Length,0.25)
Q3 <- quantile(dataset$Length,0.75)
IQR <- IQR(dataset$Length)
dataset <- subset(dataset, dataset$Length> (Q1 - 1.5*IQR) & dataset$Length< (Q3 + 1.5*IQR))
Q1 <- quantile(dataset$Length,0.25)
Q3 <- quantile(dataset$Length,0.75)
IQR <- IQR(dataset$Length)
dataset <- subset(dataset, dataset$Length> (Q1 - 1.5*IQR) & dataset$Length< (Q3 + 1.5*IQR))

#removing outlier in X.No.of.Lower.Case
Q1 <- quantile(dataset$X.No.of.Lower.Case,0.25)
Q3 <- quantile(dataset$X.No.of.Lower.Case,0.75)
IQR <- IQR(dataset$X.No.of.Lower.Case)
dataset <- subset(dataset, dataset$X.No.of.Lower.Case> (Q1 - 1.5*IQR) & dataset$X.No.of.Lower.Case< (Q3 + 1.5*IQR))
Q1 <- quantile(dataset$X.No.of.Lower.Case,0.25)
Q3 <- quantile(dataset$X.No.of.Lower.Case,0.75)
IQR <- IQR(dataset$X.No.of.Lower.Case)
dataset <- subset(dataset, dataset$X.No.of.Lower.Case> (Q1 - 1.5*IQR) & dataset$X.No.of.Lower.Case< (Q3 + 1.5*IQR))

#Scatterplot
ggplot(dataset,aes(x=Length,y=Strength))+geom_point()
#heatmap
ggplot(dataset,aes(x=Length,y=Strength))+geom_tile()
#training datasetset
library(caTools)
set.seed(123)
split=sample.split(dataset,SplitRatio = 0.75)
train=subset(dataset,split==TRUE)
test=subset(dataset,split==FALSE)
#Normalizaton
train[-6] = scale(train[-6])
test[-6] = scale(test[-6])

#Missing Values
sum(is.na(dataset))

createNAs <- function (x, pctNA = 0.1) {
  n <- nrow(x)
  p <- ncol(x)
  NAloc <- rep(FALSE, n * p)
  NAloc[sample.int(n * p, floor(n * p * pctNA))] <- TRUE
  x[matrix(NAloc, nrow = n, ncol = p)] <- NA
  return(x)
}
dataset=createNAs(dataset)
summary(dataset)
sum(is.na(dataset))
head(dataset)

#Reducing the size of the dataset to improve the speed 
dataset=dataset[sample(c(1:669640), size=200000, replace = FALSE, prob = NULL),]
dim(dataset)
str(dataset)
sum(is.na(dataset))
#Removing Unnecessay coloumns

dataset=dataset[c(-1,-2)]

#Dealing with missing values

mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

dataset$Length = ifelse(is.na(dataset$Length),ave(dataset$Length, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$Length)
dataset$X.No.of.Upper.Case = ifelse(is.na(dataset$X.No.of.Upper.Case),ave(dataset$X.No.of.Upper.Case, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$X.No.of.Upper.Case)
dataset$X.No.of.Lower.Case = ifelse(is.na(dataset$X.No.of.Lower.Case),ave(dataset$X.No.of.Lower.Case, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$X.No.of.Lower.Case)
dataset$X.No.of.Numbers = ifelse(is.na(dataset$X.No.of.Numbers),ave(dataset$X.No.of.Numbers, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$X.No.of.Numbers)
dataset$X.No.of.Spl.Chars = ifelse(is.na(dataset$X.No.of.Spl.Chars),ave(dataset$X.No.of.Spl.Chars, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$X.No.of.Spl.Chars)
dataset$Strength = ifelse(is.na(dataset$Strength),ave(dataset$Strength, FUN = function(x) mode(x, na.rm = 'TRUE')),dataset$Strength)
as.data.frame(table(dataset$Strength))
sum(is.na(dataset))
sum(is.na(dataset$Strength))
dataset$Strength=as.factor(dataset$Strength)
dataset$Length=as.numeric(dataset$Length)

#Heatmap
d1=dataset[,c(1,2,3,4,5)]
res <- cor(d1)
cormat= round(res, 2)
library(reshape2)
melted_cormat <- melt(cormat)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()





#Detecting Outliers

boxplot(dataset)
#install.packages("outliers")
library(outliers)

outlier_tf = outlier(dataset$Length,logical=TRUE)
sum(outlier_tf)
#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
dataset = dataset[-find_outlier,]
nrow(dataset)

outlier_tf = outlier(dataset$X.No.of.Upper.Case,logical=TRUE)
sum(outlier_tf)
#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
dataset = dataset[-find_outlier,]
nrow(dataset)

outlier_tf = outlier(dataset$X.No.of.Lower.Case,logical=TRUE)
sum(outlier_tf)
#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
dataset = dataset[-find_outlier,]
nrow(dataset)

outlier_tf = outlier(dataset$X.No.of.Numbers,logical=TRUE)
sum(outlier_tf)
#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
dataset = dataset[-find_outlier,]
nrow(dataset)

outlier_tf = outlier(dataset$X.No.of.Spl.Chars,logical=TRUE)
sum(outlier_tf)
#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
dataset = dataset[-find_outlier,]
nrow(dataset)

dim(dataset)
str(dataset)
boxplot(dataset)
# Splitting the dataset into the Training set and Test set

# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Strength, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
#Normalizaton
training_set[-6] = scale(training_set[-6])
test_set[-6] = scale(test_set[-6])


#Applying Machine Learning Models


#SVM
# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier_svm = svm(formula = Strength ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier_svm, newdata = test_set[-6])

# Making the Confusion Matrix
confusionMatrix((y_pred),test_set[,6])


#Kernel SVM
# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier_ksvm = svm(formula = Strength ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier_ksvm, newdata = test_set[-6])

# Making the Confusion Matrix
confusionMatrix((y_pred),test_set[,6])


#naiveBayes
# Fitting naiveBayes to the Training set
# install.packages('e1071')
library(e1071)
classifier_nb = naiveBayes(x = training_set[-6],
                        y = training_set$Strength)

# Predicting the Test set results
y_pred = predict(classifier_nb, newdata = test_set[-6])

# Making the Confusion Matrix
confusionMatrix((y_pred),test_set[,6])


#RandomForest
#install.packages("randomForest")
#install.packages("party")
library(party)
library(randomForest)

#rf = randomForest(Strength ~Length+X.No.of.Upper.Case+X.No.of.Lower.Case+X.No.of.Numbers+X.No.of.Spl.Chars,data=training_set)
rf <- ctree(Strength ~Length+X.No.of.Upper.Case+X.No.of.Lower.Case+X.No.of.Numbers+X.No.of.Spl.Chars,
            data=training_set)

y_pred = predict(rf, newdata=test_set[-6])
confusionMatrix((y_pred),test_set[,6])
plot(rf,type="simple")


#Single prediction

#install.packages('data.table')
library(stringr)
library(data.table)

str=readline(prompt = "Enter Password to find strength: ");
length = nchar(str)
upp=str_count(str, "[A-Z]")
loo=str_count(str, "[a-z]")
num=str_count(str, "[0-9]")
spec=length-(upp+loo+num)

vec<-c(length,upp/length,loo/length,num/length,spec/length)

new_df=data.frame(vec)
new_df=transpose(new_df)

names(new_df)[1] <- "Length"
names(new_df)[2] <- "X.No.of.Upper.Case"
names(new_df)[3] <- "X.No.of.Lower.Case"
names(new_df)[4] <- "X.No.of.Numbers"
names(new_df)[5] <- "X.No.of.Spl.Chars"
new_df[] = scale(new_df)
print("Prediction form SVM")
y_pred = predict(classifier_svm, newdata = new_df)
print(y_pred)

print("Prediction form Kernel SVM")
y_pred = predict(classifier_ksvm, newdata = new_df)
print(y_pred)

print("Prediction form Naive Bayes")
y_pred = predict(classifier_nb, newdata = new_df)
print(y_pred)

print("Prediction form Random Forest")
y_pred = predict(rf, newdata = new_df)
print(y_pred)



#Stats
#install.packages('caret')


#Accuracy Graph
library(RColorBrewer)
coul <- brewer.pal(4, "Set2") 

barplot(c(0.9533 , 0.9612 , 0.939,0.85 ),
        main = "Accuracy Comparison",
        xlab = "ML Models",
        ylab = "Accuracy Percentage",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.70,1.0), xpd=FALSE,col=coul)


#Precision Graph

barplot(c(0.8999, 0.8933, 0.8011,0.85),
        main = "Class 0 Precision Comparison",
        xlab = "ML Models",
        ylab = "Class 0 Precision Percentage",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.50,1.0), xpd=FALSE,col=coul)

barplot(c(0.9684, 0.9685, 0.9614,0.8222),
        main = "Class 1 Precision Comparison",
        xlab = "ML Models",
        ylab = "Class 1 Precision Percentage",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.80,1.0), xpd=FALSE,col=coul)

barplot(c(0.9083, 0.9831, 0.9338,0.5588),
        main = "Class 2 Precision Comparison",
        xlab = "ML Models",
        ylab = "Class 2 Precision Percentage",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.5,1), xpd=FALSE,col=coul)






#Sensitivity Graph

coul <- brewer.pal(5, "Set2") 

barplot(c(c(0.8999),c(0.8933),c(0.80119),0.54852),
        main = "Class 0 Sensitivity",
        xlab = "ML Models",
        ylab = "Sensitivity",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.50,1.0), xpd=FALSE,col=coul)

barplot(c(c(0.9685),c(0.9686),c(0.9614),0.8222),
        main = "Class 1 Sensitivity",
        xlab = "ML Models",
        ylab = "Sensitivity",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.80,1.0), xpd=FALSE,col=coul)

barplot(c(c(0.9084),c(0.9832),c(0.9339),0.9089),
        main = "Class 2 Sensitivity",
        xlab = "ML Models",
        ylab = "Sensitivity",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.90,1.0), xpd=FALSE,col=coul)







#Specificity Graph
coul <- brewer.pal(5, "Set1")
barplot(c(c(0.9862),c(0.9866),c(0.98536),0.97308),
        main = "Class 0 Specificity",
        xlab = "ML Models",
        ylab = "Specificity",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.95,1.0), xpd=FALSE,col=coul)

barplot(c(c(0.9043),c(0.9375),c(0.8688),0.7507),
        main = "Class 1 Specificity",
        xlab = "ML Models",
        ylab = "Specificity",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.70,1.0), xpd=FALSE,col=coul)

barplot(c(c(0.9863),c(0.9861),c(0.9805),0.8662),
        main = "Class 2 Specificity",
        xlab = "ML Models",
        ylab = "Specificity",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.80,1.0), xpd=FALSE,col=coul)



#F1 Score
coul <- brewer.pal(4, "Set3")
barplot(c(c((2*0.8999),(2*0.8933),(2*0.8011),(2*0.85))*c(0.8999, 0.8933, 0.80119,0.54852))/(c((0.8999),(0.8933),(0.8011),(0.85))+c(0.8999, 0.8933, 0.80119,0.54852)),
        main = "Class 0 F1 Score",
        xlab = "ML Models",
        ylab = "F1 Score",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.4,.9), xpd=FALSE,col=coul)

barplot(c(c((2*0.9684),(2*0.9685),(2*0.9614),2*0.8222)*c(0.9685,0.9686,0.9614,0.8222)/c((0.9684),(0.9685),(0.9614),0.8222)+c(0.9685,0.9686,0.9614,0.8222)),
        main = "Class 1 F1 Score",
        xlab = "ML Models",
        ylab = "F1 Score",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(1.975,3), xpd=FALSE,col=coul)

barplot(c(c((2*0.9083),(2*0.9831),(2*0.9338),2*0.5588)*c(0.9084,0.9832,0.9339,0.9089))/(c((0.9083),(0.9831),(0.9338),0.5588)+c(0.9084,0.9832,0.9339,0.9089)),
        main = "Class 2 F1 Score",
        xlab = "ML Models",
        ylab = "F1 Score",
        names.arg = c("SVM", "SVM(Radial Kernel)", "Naive Bayes","RandomForest"), ylim = c(0.5,1), xpd=FALSE,col=coul)




#Ploting Dataset Coloumns

#Modifying Dataset for Plotting Graphs

for (i in 1:nrow(dataset)) {
  dataset$X.No.of.Upper.Case[i]=dataset$X.No.of.Upper.Case[i]*dataset$Length[i]
  dataset$X.No.of.Lower.Case[i]=dataset$X.No.of.Lower.Case[i]*dataset$Length[i]
  dataset$X.No.of.Numbers[i]=dataset$X.No.of.Numbers[i]*dataset$Length[i]
  dataset$X.No.of.Spl.Chars[i]=dataset$X.No.of.Spl.Chars[i]*dataset$Length[i]}


plot(dataset$Length, dataset$Strength, main = "Length vs Strength",
     xlab = "Length of Password", ylab = "Strength of Password",
     pch = 19)

plot(dataset$X.No.of.Upper.Case, dataset$Strength, main = "No. of Upper Case Characters vs Strength",
     xlab = "No. of Upper Case Characters", ylab = "Strength of Password",
     pch = 19)

plot(dataset$X.No.of.Lower.Case, dataset$Strength, main = "No. of Lower Case Characters vs Strength",
     xlab = "No. of Lower Case Characters", ylab = "Strength of Password",
     pch = 19)

plot(dataset$X.No.of.Numbers, dataset$Strength, main = "No. of Number Characters vs Strength",
     xlab = "No. of Number Characters", ylab = "Strength of Password",
     pch = 19)

plot(dataset$X.No.of.Spl.Chars, dataset$Strength, main = "No. of Special Characters vs Strength",
     xlab = "No. of Special Characters", ylab = "Strength of Password",
     pch = 19)






