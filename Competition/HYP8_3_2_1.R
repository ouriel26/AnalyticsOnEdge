DataTrain$SubsectionName[DataTrain$SubsectionName=="Asia Pacific"]=""
DataTest$SubsectionName[DataTest$SubsectionName=="Asia Pacific"]=""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Culture"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Culture"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="National"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="National"] = ""
DataTrain$SectionName[DataTrain$SectionName=="World"] = ""
DataTest$SectionName[DataTest$SectionName=="World"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Travel"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Travel"] = ""

DataTrain$WordCountLevel[DataTrain$WordCount <= 100] = "low"
DataTrain$WordCountLevel[DataTrain$WordCount > 100 & DataTrain$WordCount <= 500] = "mid"
DataTrain$WordCountLevel[DataTrain$WordCount > 500 & DataTrain$WordCount <= 1000] = "hi"
DataTrain$WordCountLevel[DataTrain$WordCount > 1000] = "very hi"
DataTrain$WordCountLevel = as.factor(DataTrain$WordCountLevel)
DataTest$WordCountLevel[DataTest$WordCount <= 100] = "low"
DataTest$WordCountLevel[DataTest$WordCount > 100 & DataTest$WordCount <= 500] = "mid"
DataTest$WordCountLevel[DataTest$WordCount > 500 & DataTest$WordCount <= 1000] = "hi"
DataTest$WordCountLevel[DataTest$WordCount > 1000] = "very hi"
DataTrain$WordCount = NULL
DataTest$WordCount = NULL

x <- model.matrix(Popular ~ . -herald -tribun -two -archiv -unit -photo -work -world, data=DataTrain, family="binomial" )[,-1]


#DataTrainXMatrix = as.matrix(data.frame(DataTrain[,-c("daili","today","archiv","articl","bank","can","compani","execut","first","intern","like","look","make","obama","offer","one","photo","presid","said","say","senat","share","show","state","take","time","unit","will","work","world","year","day","fashion","new","report","week","york","NewsDesk","SectionName","SubsectionName","WordCount","Weekday", "Hour0_8","Hour9_18","Hour19_23")]))
y = as.vector(data.frame(DataTrain[,c("Popular")])) 

HYP8_3_2_1_Model = cv.glmnet(x,y=as.factor(DataTrain[,c("Popular")]), type.measure="auc" ,family="binomial")


HYP8_3_2_1_predictions = predict(HYP8_3_2_1_Model, newx=x,s=c(0.001,0.003), type="response")
HYP8_3_2_1_predictions = predict(HYP8_3_2_1_Model, newx=x,s="lambda.min", type="response")
#table(y[,1],HYP8_3_2_predictions[,1] >0.5)
HYP8_3_2_1_ROCRpred = prediction(HYP8_3_2_1_predictions[,1], y[,1])
HYP8_3_2_1_ROCRperf = performance(HYP8_3_2_1_ROCRpred, "tpr", "fpr")
plot(HYP8_3_2_1_ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
HYP8_3_2_1_AUC = as.numeric(performance(HYP8_3_2_1_ROCRpred, "auc")@y.values)
HYP8_3_2_1_AUC

xTest <- model.matrix( ~ . -herald -tribun -two -archiv -unit -photo -work -world, data=DataTest, family="binomial" )[,-1]


HYP8_3_2_1_TestPredictions = predict(HYP8_3_2_1_Model, newx=xTest,s=c(0.001,0.003), type="response")
HYP8_3_2_1_Submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = HYP8_3_2_1_TestPredictions[,1])
write.csv(HYP8_3_2_1_Submission, "HYP8_3_2_1_Submission.csv", row.names=FALSE)

