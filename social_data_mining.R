# sentiment analysis

BJP <- read.csv("bjp_tweets.csv",header = TRUE)

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

# term document matrix

#tdm matrix shows frequently appeared text in data
tdm = TermDocumentMatrix(cleanset)
tdm = as.matrix(tdm)
df = tdm[1:500, 1:500]

dim(tdm)

#library(proxy)

# Assuming tdm is your TermDocumentMatrix

# Compute cosine similarity
#cosine_sim <- proxy::simil(tdm, method = "cosine")

# Convert cosine similarity matrix to a square matrix
#square_matrix <- as.matrix(cosine_sim)



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

co_occurrence_matrix <- w %*% t(w)

############

threshold <- 50  # Set a threshold for co-occurrence count
co_occurrence_matrix[co_occurrence_matrix < threshold] <- 0

# Convert the co-occurrence matrix to a graph/network
library(igraph)

# Create an igraph object from the adjacency matrix
word_network <- graph_from_adjacency_matrix(co_occurrence_matrix, mode = "undirected", weighted = TRUE)

# Plot the network
plot(word_network, vertex.label.dist = 1.5, vertex.size = 5, edge.arrow.size = 0.5, edge.width = E(word_network)$weight / 2)

plot(
  word_network,
  vertex.label.dist = 1.5,
  vertex.size = 5,
  edge.width = E(word_network)$weight / max(E(word_network)$weight) * 5  # Adjust the scaling factor as needed
)
############

num_nodes <- vcount(word_network)
print(paste("Number of nodes:", num_nodes))

# Number of edges
num_edges <- ecount(word_network)
print(paste("Number of edges:", num_edges))




##################classifiers############################

# Assuming 'cleanset' contains preprocessed text data and 'target' contains labels


##############additional for representation

# Assuming 'co_occurrence_matrix' contains the co-occurrence frequencies

# Convert co-occurrence matrix to a graph/network
word_network <- graph_from_adjacency_matrix(co_occurrence_matrix, mode = "undirected", weighted = TRUE)

# Perform community detection for better visualization
communities <- cluster_walktrap(word_network)

# Plot the network with communities colored
plot(
  communities,
  word_network,
  vertex.label = NA,
  edge.width = E(word_network)$weight / max(E(word_network)$weight) * 3,
  vertex.color = rainbow(max(membership(communities)) + 1),
  layout = layout_with_fr(word_network)
)

#############################################


