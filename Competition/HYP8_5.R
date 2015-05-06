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

DataTrain$Weekday = NULL
DataTrain$Weekday[NewsTrain$Weekday=="Tuesday" | NewsTrain$Weekday=="Wednesday" | NewsTrain$Weekday=="Thursday" | NewsTrain$Weekday=="Saturday" ] ="Other"
DataTrain$Weekday[NewsTrain$Weekday=="Sunday"] ="Sunday"
DataTrain$Weekday[NewsTrain$Weekday=="Monday"] ="Monday"
DataTrain$Weekday[NewsTrain$Weekday=="Friday"] ="Friday"
DataTrain$Weekday = as.factor(DataTrain$Weekday)

DataTest$Weekday = NULL
DataTest$Weekday[NewsTest$Weekday=="Tuesday" | NewsTest$Weekday=="Wednesday" | NewsTest$Weekday=="Thursday" | NewsTest$Weekday=="Saturday" ] ="Other"
DataTest$Weekday[NewsTest$Weekday=="Sunday"] ="Sunday"
DataTest$Weekday[NewsTest$Weekday=="Monday"] ="Monday"
DataTest$Weekday[NewsTest$Weekday=="Friday"] ="Friday"
DataTest$Weekday = as.factor(DataTest$Weekday)

HYP8_4_Model = glm(Popular ~. -herald -tribun -two -archiv -unit -photo -work -world, data=DataTrain, family="binomial")

HYP8_4_predictions = predict(HYP8_4_Model, type="response")
HYP8_4_ROCRpred = prediction(HYP8_4_predictions, DataTrain$Popular)
HYP8_4_ROCRperf = performance(HYP8_4_ROCRpred, "tpr", "fpr")
plot(HYP8_4_ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
HYP8_4_AUC = as.numeric(performance(HYP8_4_ROCRpred, "auc")@y.values)
HYP8_4_AUC

HYP8_4_TestPredictions = predict(HYP8_4_Model, newdata=DataTest, type="response")
HYP8_4_Submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = HYP8_4_TestPredictions)
write.csv(HYP8_4_Submission, "HYP8_4_Submission.csv", row.names=FALSE)
