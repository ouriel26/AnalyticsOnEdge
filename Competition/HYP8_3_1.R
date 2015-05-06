DataTrain$SubsectionName[DataTrain$SubsectionName=="Asia Pacific"]=""
DataTest$SubsectionName[DataTest$SubsectionName=="Asia Pacific"]=""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Culture"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Culture"] = ""
DataTrain$SectionName[DataTrain$SectionName=="World"] = ""
DataTest$SectionName[DataTest$SectionName=="World"] = ""
DataTrain$NewsDesk[DataTrain$NewsDesk=="Travel"] = ""
DataTest$NewsDesk[DataTest$NewsDesk=="Travel"] = ""

HYP8_3_Model = glm(Popular ~. -herald -tribun -two -archiv -unit -photo -work -world, data=DataTrain, family="binomial")

HYP8_3_predictions = predict(HYP8_3_Model, type="response")
HYP8_3_ROCRpred = prediction(HYP8_3_predictions, DataTrain$Popular)
HYP8_3_ROCRperf = performance(HYP8_3_ROCRpred, "tpr", "fpr")
plot(HYP8_3_ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
HYP8_3_AUC = as.numeric(performance(HYP8_3_ROCRpred, "auc")@y.values)
HYP8_3_AUC

HYP8_3_TestPredictions = predict(HYP8_3_Model, newdata=DataTest, type="response")
HYP8_3_Submission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = HYP8_3_TestPredictions)
write.csv(HYP8_3_Submission, "HYP8_3_Submission.csv", row.names=FALSE)
