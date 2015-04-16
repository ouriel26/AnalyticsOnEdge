# KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

# This script file is intended to help you deal with the text data provided in the competition data files

# If you haven't already, start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Now, let's load the "tm" package

library(tm)

# Then create a corpus from the headline variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of HeadlineWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(NewsTrain) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTrain"

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

# The tail function takes the last "n" rows of HeadlineWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(NewsTest) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTest"

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

# Note that this split of HeadlineWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!

HeadlineWordsTrain$Popular = NewsTrain$Popular

HeadlineWordsTrain$WordCount = NewsTrain$WordCount
HeadlineWordsTest$WordCount = NewsTest$WordCount

# Remember that you can always look at the structure of these data frames to understand what we have created


# Now let's create a logistic regression model using all of the variables:

HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)

# And make predictions on our test set:

PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionHeadlineLog.csv" on the Kaggle website to use this as a submission to the competition

# This script file was just designed to help you get started - to do well in the competition, you will need to build better models!

HeadlineWordsTrain2$WordsOK = (rowSums(HeadlineWordsTrain2)-HeadlineWordsTrain2$WordCount-HeadlineWordsTrain2$Popular) >=2

CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))

CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.99)
AbstractWords = as.data.frame(as.matrix(sparseAbstract))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
AbstractWordsTrain = head(AbstractWords, nrow(NewsTrain))
AbstractWordsTest = tail(AbstractWords, nrow(NewsTest))
AbstractWordsTrain$Popular = NewsTrain$Popular
AbstractWordsTrain$WordCount = NewsTrain$WordCount
AbstractWordsTest$WordCount = NewsTest$WordCount
AbstractWordsLog= glm(Popular ~ ., data = AbstractWordsTrain, family=binomial)






CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))
CorpusSnippet = tm_map(CorpusSnippet, tolower)
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)

dtmSnippet = DocumentTermMatrix(CorpusSnippet)
sparseSnippet = removeSparseTerms(dtmSnippet, 0.99)
SnippetWords = as.data.frame(as.matrix(sparseSnippet))
colnames(SnippetWords) = make.names(colnames(SnippetWords))
SnippetWordsTrain = head(SnippetWords, nrow(NewsTrain))
SnippetWordsTest = tail(SnippetWords, nrow(NewsTest))
SnippetWordsTrain$Popular = NewsTrain$Popular
SnippetWordsTrain$WordCount = NewsTrain$WordCount
SnippetWordsTest$WordCount = NewsTest$WordCount
SnippetWordsLog= glm(Popular ~ ., data = SnippetWordsTrain, family=binomial)

NewsTest = read.csv("http://static.wepingo.com/csv/NYTimesBlogTest.csv", stringsAsFactors = FALSE)
NewsTrain = read.csv("http://static.wepingo.com/csv/NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
tapply(NewsTrain$WordCount, NewsTrain$NewsDesk, mean)
tapply(NewsTrain$Popular, NewsTrain$NewsDesk, mean)
barplot(tapply(NewsTrain$Popular, NewsTrain$NewsDesk, mean))
table(NewsTrain$Popular, NewsTrain$NewsDesk)
isOpEd = NewsTrain$NewsDesk == "OpEd"

cor(tapply(NewsTrain$WordCount, NewsTrain$NewsDesk, mean), tapply(NewsTrain$Popular, NewsTrain$NewsDesk, mean)) 
# ==> Correlation de 0.08 dans NewsTrain : Il n'ya pas de corrélation entre la poplarité moyenne par desk et le nombre de mots par desk



#Hypothèse 1 : Abstract 0.995 + Bool(NewsDesk == OpEd)
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))

CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.995)
AbstractWords = as.data.frame(as.matrix(sparseAbstract))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
AbstractWordsTrain = head(AbstractWords, nrow(NewsTrain))
AbstractWordsTest = tail(AbstractWords, nrow(NewsTest))
AbstractWordsTrain$Popular = NewsTrain$Popular
AbstractWordsTrain$WordCount = NewsTrain$WordCount
AbstractWordsTest$WordCount = NewsTest$WordCount
AbstractWordsTrain$isOpEd = NewsTrain$NewsDesk == "OpEd"
AbstractWordsLog= glm(Popular ~ ., data = AbstractWordsTrain, family=binomial)
#Fin Hypothèse 1 ##############################################################################################
