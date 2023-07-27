### Loading and compiling data ###

library(readxl)

data <- read_excel("your_data_file_path")

data$Text <- data$your_text_variable

#### Stage 0 - PRE PROCESSING ####

library(dplyr)
library(ggplot2)
library(tokenizers)
library(tidytext)
library(SnowballC)
library(tm)
library(stringi)
library(ggrepel)

#cut text into words by splitting on spaces and punctuation
review_words <- data %>% unnest_tokens(word,Text,to_lower=FALSE) 
print("number of words")
nrow(review_words)

#Count the number of times each word occurs
counts <- review_words %>%count(word, sort=TRUE) # sort = TRUE for sorting in descending order of n. 
# For questions about a function type ?fun in the console. For example ?count
print("number of unique words")
nrow(counts)

# select the most and least mentioned words
counts_high <- head(counts, n = 600) #select the top 600 rows
counts_low <- tail(counts, n = 600) #select the bottom 600 rows


# showing those words in a plot
counts_high %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram") # reorder() function is needed to convert character string to factor in R, such that the plot is made on the number of occurences and not on alphabethic order (which is done when the input is a character vector)

# Removing stop words
data(stop_words)
review_words_nostop <- review_words %>% 
  anti_join(stop_words)
counts <- review_words_nostop %>%
  count(word, sort=TRUE)

# getting an update on the most frequent words after removing stop words
counts %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram")

## remove all capitals to facilitate cleaning of the data}
#cut text into words by splitting on spaces and punctuation
review_words <- data %>% unnest_tokens(word,Text,to_lower=TRUE) 

# count the number of times each word occurs
counts <- review_words %>%count(word, sort=TRUE) # sort = TRUE for sorting in descending order of n. 

review_words_nostop <- review_words %>% 
  anti_join(stop_words)
counts <- review_words_nostop %>%
  count(word, sort=TRUE)

# plotting the most frequent words}
counts %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram")

# creating the full review from the cleaned + stemmed words
j<-1
for (j in 1:nrow(data)) {
  stemmed_description<-  anti_join((data[j,] %>% unnest_tokens(word,Text, drop=FALSE,to_lower=TRUE) ),stop_words)
  
  stemmed_description<-(wordStem(stemmed_description[,"word"], language = "porter"))
  
  data[j,"Text"]<-paste((stemmed_description),collapse = " ")
  
}


# cut text into words by splitting on spaces and punctuation
review_words <- data %>% unnest_tokens(word,Text,to_lower=TRUE) 
print("number of words")
nrow(review_words)

# Count the number of times each word occurs
counts <- review_words %>%count(word, sort=TRUE) # sort = TRUE for sorting in descending order of n. 
print("number of unique words after stemming and without stop words")
nrow(counts)

# What are the most frequent words?
  
counts %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram")


# plotting the most infrequent words
counts %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(-20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram")


## getting the distribution of the word frequency counts
#Count the number of times each count occurs
counted_counts <- counts  %>%count(n, sort=TRUE) # sort = TRUE for sorting in descending order of n. 

counted_counts %>% 
  mutate(n = reorder(n,nn)) %>% 
  top_n(10, n) %>%
  ggplot(aes(n,nn)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("count Frequency Histogram")

head(counted_counts,n=11)

# removing the most infrequent and the too frequent words
infrequent <- counts %>% filter(n<0.01*nrow(data))
frequent <- counts[1:2,]
toremove  <- full_join(frequent,infrequent)
print("Number of infrequent words")
nrow(toremove)
print("Share of infrequent words")
nrow(toremove)/nrow(counts)

# putting all words back into the review format and saving the data, echo=FALSE, message=FALSE}
j<-1 
for (j in 1:nrow(data)) {
  stemmed_description<-  anti_join((data[j,] %>% unnest_tokens(word,Text,to_lower=TRUE) ),toremove)
  
  data[j,"Text"]<-   paste((stemmed_description[,"word"]),collapse = " ")
  
}

#### Stage 1 - LDA ####

install.packages("topicmodels")
install.packages("tm")

# data wrangling
library(dplyr)
library(tidyr)
library(lubridate)
# visualization
library(ggplot2)
# dealing with text
library(textclean)
library(tm)
# topic model
library(tidytext)
library(topicmodels)
library(textmineR)

options(stringsAsFactors = FALSE)

# variable used to skip slow parts in this code
skip_slow = TRUE
  #or
skip_slow = FALSE

# Creating a Document-Term-Matrix
dtm <- CreateDtm(doc_vec = data[, "Text"], # character vector of documents
                 doc_names = data[, "Text_ID"]) # document names

## Creating Word Clouds

if (!require("factoextra")) install.packages("factoextra")
library(dplyr)
library(ggplot2)
library(tokenizers)
library(Matrix)
library(tibble)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(tm)
library(stringi)
library(ggrepel)
library("wordcloud")
library("quanteda")

if (!require("smacof")) install.packages("smacof")
if (!require("ggfortify")) install.packages("ggfortify")
if (!require("ggthemes")) install.packages("ggthemes")
library(smacof)

corpus <- corpus(data, docid_field = "Text_ID", text_field = "Text")
corpus_token <- tokens(corpus)

# Count number of documents in which a word occurs
reviews_dfm <- dfm(corpus_token)
docfreqs <- docfreq(reviews_dfm) %>% sort(decreasing = TRUE)
docfreqs <- data.frame(word = names(docfreqs), n_docs=docfreqs)

#  Merge document frequency and word frequency
tf_idf_table <- merge(docfreqs, data)


# Divide word count by document count
tf_idf_table$tf_idf <- tf_idf_table$n/tf_idf_table$n_docs

tf_idf_table<-tf_idf_table[order(-tf_idf_table$tf_idf),]


# Create a document term matrix (DTM) of the reviews
reviews_corp <- corpus(data, docid_field = "Text_ID", text_field = "Text")
reviews_corp_token <- tokens(reviews_corp)
                               
co_occurrence_matrix <- fcm(x = reviews_corp_token, context = "document", count = "frequency", tri=FALSE)

# Create a matrix with number of documents with each word on the diagonal
counts <- colSums(as.matrix(reviews_dfm))
co_occurrence_matrix <- as.matrix(co_occurrence_matrix)


# Create a subset of 200 most occurring words
sortedcount <- counts%>% sort(decreasing=TRUE)
sortednames <- names(sortedcount)
nwords<-200
subset_words<-as.matrix(sortedcount[1:nwords])
co_occurrence_matrix <- co_occurrence_matrix[sortednames[1:nwords],sortednames[1:nwords]]

distances <- sim2diss(co_occurrence_matrix, method = "cooccurrence")

# Run the routine that finds the best matching coordinates in a 2D mp given the distances
MDS_map <- smacofSym(distances) # Transform similarity values to distances


# Plot words in a map based on the distances
ggplot(as.data.frame(MDS_map$conf), aes(D1, D2, label = rownames(MDS_map$conf))) +
  geom_text(check_overlap = TRUE) +
  theme_minimal(base_size = 15) +
  xlab('') +
  ylab('') +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)

## Determining number of topics through perplexity and coherence over 5,10,15,20 topics

# Create subsamples
set.seed(seed=2345)
train <- sample(1:n, round(n * 0.80)) # Create a training set consisting of 80% of observations
valid <- (1:n)[-train] # Put other observations in validation set

# Fit a Latent Dirichlet Allocation topic model using Gibbs sampling
if (!skip_slow) 
{
  set.seed(4321)
  n_topics=10; # Number of different topics
  lda_results <- FitLdaModel(dtm = dtm,
                             k = n_topics, # number of topic
                             burnin = 200 + 10*n_topics,
                             iterations = 700 + 10*n_topics,
                             #alpha: prior on topic probabilities - beta: prior on word probabilities
                             alpha = 0.1,beta = 0.05,
                             optimize_alpha = T,
                             calc_likelihood = T,
                             calc_coherence = T)
}

# Fit a Latent Dirichlet Allocation topic model using Gibbs sampling for different number of topics
if (skip_slow) 
{
  # remain empty
  
} else{
  dtm_train <- dtm[train,]
  dtm_val <- dtm[-train,]
  
  res <- NULL
  TMlist <- list()  # list to collect results
  n_topics <- 10
  
  for (n_topics in seq(5, 20, 5)) # Do for number of topics 5, 10, 15, 20
  {
    print(n_topics)
    
    lda_results <- FitLdaModel(dtm = dtm_train,
                               k = n_topics, # number of topic
                               burnin = 200 + 10*n_topics,
                               iterations = 700 + 10*n_topics,
                               alpha = 0.1,beta = 0.05,
                               optimize_alpha = T,
                               calc_likelihood = T,
                               calc_coherence = T) 
    
    # calculates word co-occurrence for moving window (M=5) and contrasts with topic structure
    coh_train <- mean(CalcProbCoherence(phi = lda_results$phi, dtm = dtm_train, M = 5) )
    coh_val <- mean(CalcProbCoherence(phi = lda_results$phi, dtm = dtm_val, M = 5) )
    
    # Calculate the log likelihood of the DTM for training set
    ll_train <- CalcLikelihood(dtm = dtm_train, 
                               phi = lda_results$phi, 
                               theta = lda_results$theta)/nrow(dtm_train)
    # Determine theta for validation set
    lda_results$theta_val <- predict(lda_results, dtm_val, method = "gibbs", iterations = 700 + 10*n_topics, burnin = 200 + 10*n_topics)
    
    # Calculate the log likelihood of the DTM for validation set
    ll_val <- CalcLikelihood(dtm = dtm_val, 
                             phi = lda_results$phi, 
                             theta = lda_results$theta_val)/nrow(dtm_val)
    
    # Combine all values in matrix per row
    res <- rbind(res, data.frame(n_topics=n_topics, ll_train = ll_train, ll_validation = ll_val, coh_train=coh_train, coh_val=coh_val))
    
    # Add current LDA result
    TMlist <- append(TMlist, c(lda_results))
    
    print(res)
  }
 
}

# Make plot showing the perplexity for different numbers of topics
ggplot(res, aes(n_topics)) +
  geom_line(aes(y = ll_train, colour = "Train")) + # show perplexity training set
  geom_line(aes(y = ll_validation, colour = "Validation")) + # show perplexity validation set
  labs(colour = "Sample") +
  scale_colour_manual(values = c("Train" = "blue", "Validation" = "lightblue")) +
  theme_minimal()

# Print results with best perplexity on validation sample (based on likelihood which is easier to read)
res[which.min(res$ll_validation),]

# Make plot showing the coherence for different numbers of topics
ggplot(res, aes(n_topics)) +
  geom_line(aes(y = coh_train, colour = "Train")) + # show coherence training set
  geom_line(aes(y = coh_val, colour = "Validation")) + # show coherence validation set
  labs(colour = "Sample") +
  scale_colour_manual(values = c("Train" = "blue", "Validation" = "lightblue")) +
  theme_minimal()

# Print results with best coherence
res[which.max(res$coh_val),]

## Computing LDA 

ap_lda <- LDA(dtm, k = "chosen number of topics", control = list(seed = 1234))
ap_lda

lda_model <- ap_lda 
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Generate a custom gradient color palette for each topic
num_topics <- length(unique(ap_top_terms$topic))
color_palette <- colorRampPalette(c("blue", "lightblue"))(num_topics)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = beta)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_gradient(low = "blue", high = "lightblue") +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


### Stage 2 - PCA ###

# Extract the topic-term matrix from LDA model
topic_term_matrix <- tidy(ap_lda, matrix = "beta")

# Convert the data to a numeric matrix
numeric_matrix <- data.matrix(topic_term_matrix[, -1])

# Perform PCA on the numeric matrix
pca <- prcomp(numeric_matrix)

# Determine the optimal number of components to retain
scree_plot <- plot(pca, type = "l")

# Select the desired number of components
num_components <- min(4, ncol(pca$x))

# Retain the desired number of components from PCA
pca_data <- data.frame(Topic = topic_term_matrix$topic,
                       pca$x[, 1:num_components])

# Merge the PCA data with the original topic-term matrix
pca_topic_terms <- cbind(pca_data, topic_term_matrix[, -1])

# Print the purified topics
print(pca_topic_terms)

# Generate a custom gradient color palette for each topic
num_topics <- length(unique(pca_topic_terms$Topic))
color_palette <- colorRampPalette(c("blue", "lightblue"))(num_topics)

# Visualize the top terms for each topic similarly to Stage 1
pca_topic_terms %>%
  group_by(Topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(Topic, -beta) %>%
  mutate(term = reorder_within(term, beta, Topic)) %>%
  ggplot(aes(beta, term, fill = beta)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_gradient(low = "blue", high = "lightblue") +
  facet_wrap(~ Topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Purified Topics", x = "Weight", y = "Term") +
  theme_minimal()

### Stage 3 - Sentiment Analysis ###

install.packages("tidyverse")
install.packages("tidytext")
install.packages("syuzhet")
library(tidyverse)
library(tidytext)
library(syuzhet)

# Read the CSV file containing the tweets
tweets <- read_csv("your_tweet_data_file_path")

# Perform sentiment analysis
sentiments <- get_sentiments("afinn")  # AFINN lexicon for sentiment analysis
tweet_sentiment <- tweets %>%
  mutate(text = as.character(text)) %>%
  mutate(sentiment_score = get_sentiment(text, method = "afinn")) %>%
  mutate(sentiment_category = ifelse(sentiment_score > 0, "positive",
                                     ifelse(sentiment_score < 0, "negative", "neutral")))

# View the sentiment-assigned tweets
head(tweet_sentiment)

## Convert AFFIN score to 0.1 ration based on tweet character length 

library(afinn)

calculate_sentiment_ratio <- function(tweet) {
  afinn_scores <- sapply(strsplit(tweet, " "), function(word) afinn::afinn(word))
  total_afinn_score <- sum(afinn_scores)
  max_possible_afinn_score <- nchar(tweet) * afinn::afinn$max_valence
  normalized_score <- total_afinn_score / max_possible_afinn_score
  sentiment_ratio <- (normalized_score + 1) / 2
  return(sentiment_ratio)
}


#### Stage 4 - Assessing Validity ####

## Implementing Cohen's Kappa on two human coders' output (as a measure of performance validity )
#filling each human coders' identified themes in a vector
coder1_labels <- c("topic name 1", "topic name 2", "topic name 3", ...)
coder2_labels <- c("topic name 1", "topic name 2", "topic name 3", ...)

# Convert the topic labels to factors to ensure correct computation
coder1_factor <- factor(coder1_labels)
coder2_factor <- factor(coder2_labels)

install.packages("irr")  
library(irr)

# Calculate Cohen's Kappa
coder1_factor <- factor(coder1_labels)
coder2_factor <- factor(coder2_labels)

kappa_result <- kappa2(data.frame(coder1_factor, coder2_factor), weight = "unweighted", sort.levels = FALSE)
kappa_score <- kappa_result$value

# Print the Kappa score
print(kappa_score)