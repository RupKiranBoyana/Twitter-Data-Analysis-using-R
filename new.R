
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
#dtm <- DocumentTermMatrix(cleanset)

#dtm <- as.matrix(dtm)

# Convert the document-term matrix to a data frame
#dtm_df <- as.data.frame(as.matrix(dtm))

tdm = TermDocumentMatrix(cleanset)
tdm = as.matrix(tdm)


###############wordcloud######
#Bar plot

#summation of frequency values in the each row 
w = rowSums(tdm)

#words that occur 500 times or more
w = subset(w, w>=500)
w

barplot(w,
        las = 2,
        col =rainbow(50))

dim(w)


# Assuming 'tdm' is your term-document matrix
selected_terms <- c("bjp", "congress",  "rahul", "govt", "time", "like", "modi", "india", "indian", "amp", "attack", "one", "vote", "say", "pakistan",  "nation",  "elect", "peopl" )

# Subset the TDM based on selected rows
subset_tdm <- tdm[selected_terms, ]

# Convert the subset TDM to a data frame
#subset_df <- as.data.frame(subset_tdm)

# Display the new data frame
#print(subset_df)

# Transpose the data frame
transposed_df <- t(subset_tdm)

transposed_df1 <- as.data.frame(transposed_df)

# Display the transposed data frame
print(transposed_df)


#########
BJP <- read.csv("bjp_tweets.csv",header = TRUE)

# Bind the target column to the data frame if you want to use it for modeling
transposed_df1$target <- BJP$target

# Assuming 'dtm_df' is your data frame
write.csv(transposed_df, file = "transposed_df.csv", row.names = FALSE)

# Now, dtm_df contains the count vectorized representation of your text data

install.packages("caret")
library(caret)

# Split the data into training and testing sets
set.seed(123)
indices <- createDataPartition(transposed_df1$target, p = 0.7, list = FALSE)
train_data <- transposed_df1[indices, ]
test_data <- transposed_df1[-indices, ]

# Separate 'X', 'tweet', and 'target' from the training and testing sets
train_tweet <- train_data$tweet
train_target <- train_data$target
test_tweet <- test_data$tweet
test_target <- test_data$target

# Assuming 'train_data' and 'test_data' are your training and testing sets
# Replace 'target' with the actual name of your target column.

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

