DataTrain$SubsectionName[DataTrain$SubsectionName=="Asia Pacific"]=""
DataTest$SubsectionName[DataTest$SubsectionName=="Asia Pacific"]=""
DataTest$SubsectionName[DataTest$SubsectionName=="The Public Editor"]=""
DataTrain$SubsectionName[DataTrain$SubsectionName=="The Public Editor"]=""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Culture"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Culture"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Metro"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Metro"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Business"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Business"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="National"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="National"] = ""
DataTrain$SectionName[DataTrain$SectionName=="World"] = ""
DataTest$SectionName[DataTest$SectionName=="World"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Travel"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Travel"] = ""

DataTrain$LowWordCountLevel[DataTrain$WordCount < 300] = TRUE
DataTrain$LowWordCountLevel[DataTrain$WordCount >= 300] = FALSE
DataTrain$LowWordCountLevel =as.logical(DataTrain$LowWordCountLevel)

DataTest$LowWordCountLevel[DataTest$WordCount < 300] = TRUE
DataTest$LowWordCountLevel[DataTest$WordCount >= 300] = FALSE
DataTest$LowWordCountLevel =as.logical(DataTest$LowWordCountLevel)

xfactors <- model.matrix(Popular ~ . -said -say -one -york -make -can -herald -tribun -two -archiv -unit -photo -work -world -WordCount, data=DataTrain, family="binomial" )[,-1]
x <- as.matrix(data.frame(DataTrain$WordCount, xfactors))

#DataTrainXMatrix = as.matrix(data.frame(DataTrain[,-c("daili","today","archiv","articl","bank","can","compani","execut","first","intern","like","look","make","obama","offer","one","photo","presid","said","say","senat","share","show","state","take","time","unit","will","work","world","year","day","fashion","new","report","week","york","NewsDesk","SectionName","SubsectionName","WordCount","Weekday", "Hour0_8","Hour9_18","Hour19_23")]))
y = as.vector(data.frame(DataTrain[,c("Popular")])) 

HYP8_3_2_2_Model = cv.glmnet(x,y=as.factor(DataTrain[,c("Popular")]), type.measure="auc" ,family="binomial")

#HYP8_3_2_2_predictions = predict(HYP8_3_2_2_Model, newx=x[,1],s=c(0.001,0.003), type="response")
HYP8_3_2_2_predictions = predict(HYP8_3_2_2_Model, newx=x,s="lambda.min", type="response")
#table(y[,1],HYP8_3_2_2_predictions[,1] >0.5)
HYP8_3_2_2_ROCRpred = prediction(HYP8_3_2_2_predictions[,1], y[,1])
HYP8_3_2_2_ROCRperf = performance(HYP8_3_2_2_ROCRpred, "tpr", "fpr")
plot(HYP8_3_2_2_ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
HYP8_3_2_2_AUC = as.numeric(performance(HYP8_3_2_2_ROCRpred, "auc")@y.values)
HYP8_3_2_2_AUC

xTestfactors <- model.matrix( ~ . -said -say -one -york -make -can -herald -tribun -two -archiv -unit -photo -work -world -WordCount, data=DataTest, family="binomial" )[,-1]
xTest <- as.matrix(data.frame(DataTest$WordCount, xTestfactors))

HYP8_3_2_2_TestPredictions = predict(HYP8_3_2_2_Model, newx=xTest,s="lambda.min", type="response")
HYP8_3_2_2_Submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = HYP8_3_2_2_TestPredictions[,1])
write.csv(HYP8_3_2_2_Submission, "HYP8_3_2_2_Submission.csv", row.names=FALSE)

