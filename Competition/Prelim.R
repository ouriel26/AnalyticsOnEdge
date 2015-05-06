library(tm)
library(ROCR)

joinAlmostEquivalentDataFrames <- function(x,y,filteredColumnName){
  xx = subset(x, select = -eval(parse(text=filteredColumnName) ))
  n=intersect(names(xx), names(y));
  as.data.frame(c(xx[!(names(xx) %in% n)], 
                  y[!(names(y) %in% n)], 
                  xx[n] + y[n]))}

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

Datss = strptime(NewsTrain$PubDate, format="%Y-%m-%d %H:%M:%S")
NewsTrain$Hour = Datss$hour
NewsTrain$Weekday = weekdays(Datss)
Datss = strptime(NewsTest$PubDate, format="%Y-%m-%d %H:%M:%S")
NewsTest$Hour = Datss$hour
NewsTest$Weekday = weekdays(Datss)

# On retire tout facteur presenst dans le train set mais absent du test set 
NewsTrain$NewsDesk[NewsTrain$NewsDesk == "National"] = ""
NewsTrain$NewsDesk[NewsTrain$NewsDesk == "Sports"] = ""
NewsTrain$SectionName[NewsTrain$SectionName == "Magazine"] = ""
NewsTrain$SectionName[NewsTrain$SectionName == "Style"] = ""
NewsTrain$SectionName[NewsTrain$SectionName == "Sports"] = ""
NewsTrain$SubsectionName[NewsTrain$SubsectionName == "Politics"] = ""
NewsTrain$SubsectionName[NewsTrain$SubsectionName == "Fashion & Style"] = ""

NewsTest$SectionName[NewsTest$SectionName == "Magazine"] = ""

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)
NewsTrain$Weekday = as.factor(NewsTrain$Weekday)

NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)
NewsTest$SubsectionName = as.factor(NewsTest$SubsectionName)
NewsTest$Weekday = as.factor(NewsTest$Weekday)


CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
DtmHeadline = DocumentTermMatrix(CorpusHeadline)

CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
DtmAbstract = DocumentTermMatrix(CorpusAbstract)

SparsedDtmHeadline = removeSparseTerms(DtmHeadline, 0.975)
HeadlineWords = as.data.frame(as.matrix(SparsedDtmHeadline))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

SparsedDtmAbstract = removeSparseTerms(DtmAbstract, 0.975)
AbstractWords = as.data.frame(as.matrix(SparsedDtmAbstract))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
AbstractWordsTrain = head(AbstractWords, nrow(NewsTrain))
AbstractWordsTest = tail(AbstractWords, nrow(NewsTest))

HeadlineWordsTrain$UniqueID = NewsTrain$UniqueID
AbstractWordsTrain$UniqueID = NewsTrain$UniqueID

WordsTrain = joinAlmostEquivalentDataFrames(HeadlineWordsTrain,AbstractWordsTrain,"UniqueID")

HeadlineWordsTest$UniqueID = NewsTest$UniqueID
AbstractWordsTest$UniqueID = NewsTest$UniqueID

WordsTest = joinAlmostEquivalentDataFrames(HeadlineWordsTest,AbstractWordsTest,"UniqueID")

DataTrain = WordsTrain
DataTest = WordsTest
DataTrain$UniqueID = NULL
DataTest$UniqueID = NULL


DataTrain$Popular = NewsTrain$Popular
DataTrain$NewsDesk = NewsTrain$NewsDesk
DataTrain$SectionName = NewsTrain$SectionName
DataTrain$SubsectionName = NewsTrain$SubsectionName
DataTrain$WordCount = NewsTrain$WordCount
DataTrain$Hour = NewsTrain$Hour
DataTrain$Weekday = NewsTrain$Weekday

DataTest$NewsDesk = NewsTest$NewsDesk
DataTest$SectionName = NewsTest$SectionName
DataTest$SubsectionName = NewsTest$SubsectionName
DataTest$WordCount = NewsTest$WordCount
DataTest$Hour = NewsTest$Hour
DataTest$Weekday = NewsTest$Weekday


DataTrain$Hour0_8 = DataTrain$Hour >= 0 & DataTrain$Hour <= 8
DataTrain$Hour9_18 = DataTrain$Hour > 8 & DataTrain$Hour <= 18
DataTrain$Hour19_23 = DataTrain$Hour > 18 & DataTrain$Hour <= 23
DataTrain$Hour = NULL
DataTest$Hour0_8 = DataTest$Hour >= 0 & DataTest$Hour <= 8
DataTest$Hour9_18 = DataTest$Hour > 8 & DataTest$Hour <= 18
DataTest$Hour19_23 = DataTest$Hour > 18 & DataTest$Hour <= 23
DataTest$Hour = NULL