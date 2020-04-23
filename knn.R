library(gmodels)
library(e1071)
library(caret)
library(class)
library(ggplot2)
library(caTools)
flags_df <-
  read.csv(
    "C:\\Users\\Sharang\\Documents\\Assignment Sem 2\\Random Forrest\\flag.data",
    header = FALSE
  )
flags_df

colnames(flags_df) <-
  c(
    'Names',
    'landmass',
    'zone',
    'area',
    'population',
    'language',
    'religion',
    'bars',
    'stripes',
    'colours',
    'red',
    'green',
    'blue',
    'gold',
    'white',
    'black',
    'orange',
    'mainhue',
    'circles',
    'crosses',
    'saltires',
    'quarters',
    'sunstars',
    'crescent',
    'triangle',
    'icon',
    'animate',
    'text',
    'topleft',
    'botright'
  )
colnames(flags_df)
#Removing Unnecessary Columns from the Data
flg_df <- flags_df[-1]
flg_df1 <- flg_df[-29]
flg_df2 <- flg_df1[-28]
flg_df3 <- flg_df2[-17]
colnames(flg_df3)
flg_df4 <- flg_df3[-6]
colnames(flg_df4)
summary(flags_df$religion)
str(flags_df)
table(flags_df$religion)
#-------creating function to normalize the data.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# -------------standardizing all the column Lengths
flag_df1 <- as.data.frame(lapply(flg_df4[1:25], normalize))
flag_df1$religion <- flg_df$religion
colnames(flag_df1)
#Catool library is used for spliting up of the data

set.seed(101)
sample <- sample.split(flag_df1, SplitRatio = .70)
train <- subset(flag_df1, sample == T)
test <- subset(flag_df1, sample == F)
# KNN 
############KNN Model ###################
#----------contains KNN function
#Class library contains the Knn function
#----------Choosing a K-Value
#The first argument is a data frame that contains the training data set,
#the second argument is a data frame that contains the testing data set,
#the third argument is the train.religion column (Y) that we save earlier, and the fourth argument is the k (how many neighbors).
#Using For Loop to iterate the values and to check the mean.
n = 13
mean.df = NULL
for (i in 1:n) {
  set.seed(101)
  knn3 <-
    knn(
      train = train,
      test = test,
      cl = train$religion,
      k = i
    )
  mean.df[i] <- mean(knn3 == test$religion)
  print(mean.df[i])
}

# -------------Visualizing k-Values on the Graph---------
k.values <- 1:13
error.df <- data.frame(mean.df, k.values)
error.df
ggplot(error.df, aes(x = k.values, y = mean.df)) + geom_point() + geom_line(lty =
                                                                              "dotted", color = 'red')


########### Creating Knn Model#########
#Using For Loop We obatained Multiple K values 
#Therefore Using K Value as 3 We try to bulid the Model.
knn3 <-
  knn(
    train = train,
    test = test,
    cl = train$religion,
    k = 3
  )


#-------Crosstable gives the relationship
CrossTable(x = test$religion,
           y = knn3,
           prop.chisq = FALSE)
#---- Confusion Matrix-----
confusionMatrix(knn3, factor(test$religion, levels = seq(0, 7)))
#From Confusion Matrix we can see that at 95%CI Accuracy of the Model is 87.9%
