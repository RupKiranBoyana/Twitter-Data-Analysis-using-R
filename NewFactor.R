BJP <- read.csv("bjp_tweets.csv",header = TRUE)

# Install the caret package
install.packages("caret")

# Load necessary libraries
library(e1071)
library(caret)

#library used for data manipulation
library(dplyr)

# Remove columns using select() function from dplyr
BJP <- BJP %>%
  select(-X, -target)


# build corpus(corpus is used to hold the text data)
library(tm)

#encodes the text in data frame using UTF
corpus = iconv(BJP$tweet, to='UTF-8', sub = "byte")

#text data stored in above object is stored & converted
corpus = Corpus(VectorSource(corpus))

#inspect 

inspect(corpus[1:5])

#clean text

#change the text into lowercase
corpus = tm_map(corpus,tolower)

inspect(corpus[1:5])

#remove the punctuation
corpus = tm_map(corpus,removePunctuation)

inspect(corpus[1:5])

#remove the numerical digits
corpus = tm_map(corpus,removeNumbers)

inspect(corpus[1:5])

#remove the common stopwords
cleanset = tm_map(corpus, removeWords, stopwords('english'))

inspect(cleanset[1:5])

#to remove whitespaces

cleanset = tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Create a document-term matrix using count vectorization

dtm <- DocumentTermMatrix(cleanset)

dtm <- as.matrix(dtm)

# Convert the document-term matrix to a data frame

dtm_df <- as.data.frame(as.matrix(dtm))

# Assuming 'dtm_df' is your data frame
write.csv(dtm_df, file = "dtm_df.csv", row.names = FALSE)

# Bind the target column to the data frame if you want to use it for modeling
dtm_df$target <- BJP$target


# Assuming 'tdm' is your term-document matrix
selected_terms <- c("bjp", "congress",  "rahul", "govt", "time", "like", "modi", "india", "indian", "amp", "attack", "one", "vote", "say", "pakistan",  "nation",  "elect", "peopl", "target" )

# Subset the data frame based on selected columns
subset_df <- dtm_df[, selected_terms]

# Assuming 'subset_df' is your data frame
subset_df$target <- factor(subset_df$target)


# Split the data into training and testing sets
set.seed(123)
indices <- createDataPartition(subset_df$target, p = 0.7, list = FALSE)
train_data <- subset_df[indices, ]
test_data <- subset_df[-indices, ]

# Load the necessary libraries
library(e1071)


# Train an SVM model
svm_model <- svm(target ~ ., data = train_data, kernel = "radial", cost = 1)


# Make predictions on the test set
predictions <- predict(svm_model, newdata = test_data)

# Evaluate the model
confusion_matrix <- table(predictions, test_data$target)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")



#####randomforest
install.packages("randomForest")
library(randomForest)

# Train a Random Forest model
rf_model <- randomForest(target ~ ., data = train_data, ntree = 100)

# Make predictions on the test set
predictions_rf <- predict(rf_model, newdata = test_data)

# Evaluate the model
confusion_matrix_rf <- table(predictions_rf, test_data$target)
accuracy_rf <- sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)
cat("Accuracy (Random Forest):", accuracy_rf, "\n")


#########naivebayes

library(e1071)

# Train a Naive Bayes model
nb_model <- naiveBayes(target ~ ., data = train_data)

# Make predictions on the test set
predictions_nb <- predict(nb_model, newdata = test_data)

# Evaluate the model
confusion_matrix_nb <- table(predictions_nb, test_data$target)
accuracy_nb <- sum(diag(confusion_matrix_nb)) / sum(confusion_matrix_nb)
cat("Accuracy (Naive Bayes):", accuracy_nb,"\n")