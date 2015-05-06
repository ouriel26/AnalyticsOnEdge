DataTrain$SubsectionName[DataTrain$SubsectionName=="Asia Pacific"]=""
DataTest$SubsectionName[DataTest$SubsectionName=="Asia Pacific"]=""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Culture"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Culture"] = ""
DataTrain$SectionName[DataTrain$SectionName=="World"] = ""
DataTest$SectionName[DataTest$SectionName=="World"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Travel"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Travel"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Metro"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Metro"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Business"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Business"] = ""
DataTrain$SectionName[DataTrain$SectionName=="Open"]=""
DataTest$SectionName[DataTest$SectionName=="Open"]=""

DataTrain$Weekday = NewsTrain$Weekday


DataTest$Weekday = NewsTest$Weekday



HYP8_5_Model = glm(Popular ~. -herald -tribun -two -archiv -unit -photo -work -world, data=DataTrain, family="binomial")

HYP8_5_predictions = predict(HYP8_5_Model, type="response")
HYP8_5_ROCRpred = prediction(HYP8_5_predictions, DataTrain$Popular)
HYP8_5_ROCRperf = performance(HYP8_5_ROCRpred, "tpr", "fpr")
plot(HYP8_5_ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
HYP8_5_AUC = as.numeric(performance(HYP8_5_ROCRpred, "auc")@y.values)
HYP8_5_AUC

HYP8_5_TestPredictions = predict(HYP8_5_Model, newdata=DataTest, type="response")
HYP8_5_Submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = HYP8_5_TestPredictions)
write.csv(HYP8_5_Submission, "HYP8_5_Submission.csv", row.names=FALSE)
