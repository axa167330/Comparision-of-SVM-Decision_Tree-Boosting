building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#============================================================

# Load the dataset from file.

fname <- "C:/AML - BUAN 6341/HR-Employee-Attrition.csv" 
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

#============================================================

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=1470 train=1029 validate=220 test=221

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("X.U.FEFF.Age", "BusinessTravel", "DailyRate",
                   "Department", "DistanceFromHome", "Education",
                   "EducationField", "EnvironmentSatisfaction", "Gender",
                   "HourlyRate", "JobInvolvement", "JobLevel", "JobRole",
                   "JobSatisfaction", "MaritalStatus", "MonthlyIncome",
                   "MonthlyRate", "NumCompaniesWorked", "OverTime",
                   "PercentSalaryHike", "PerformanceRating",
                   "RelationshipSatisfaction", "StockOptionLevel",
                   "TotalWorkingYears", "TrainingTimesLastYear",
                   "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole",
                   "YearsSinceLastPromotion", "YearsWithCurrManager")

crs$numeric   <- c("X.U.FEFF.Age", "DailyRate", "DistanceFromHome",
                   "Education", "EnvironmentSatisfaction", "HourlyRate",
                   "JobInvolvement", "JobLevel", "JobSatisfaction",
                   "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked",
                   "PercentSalaryHike", "PerformanceRating",
                   "RelationshipSatisfaction", "StockOptionLevel",
                   "TotalWorkingYears", "TrainingTimesLastYear",
                   "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole",
                   "YearsSinceLastPromotion", "YearsWithCurrManager")

crs$categoric <- c("BusinessTravel", "Department", "EducationField",
                   "Gender", "JobRole", "MaritalStatus", "OverTime")

crs$target    <- "Attrition"
crs$risk      <- NULL
crs$ident     <- "EmployeeNumber"
crs$ignore    <- c("EmployeeCount", "Over18", "StandardHours")
crs$weights   <- NULL

#============================================================


# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Attrition) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="vanilladot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm


#============================================================

# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.


crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
print(riskchart(crs$pr, 
                na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, 
                title="Performance Chart SVM HR-Employee-Attrition.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================


# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
print(riskchart(crs$pr, 
                na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, 
                title="Performance Chart SVM HR-Employee-Attrition.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================


# Evaluate model performance on the testing dataset. 

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the SVM model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
  for (j in seq_along(perf1@x.values[[i]]))
  {
    lines(c(0,1),c(perf1@y.values[[i]][j],
                   perf1@x.values[[i]][j]),
          col=terrain.colors(10)[i],lty=3)
  }
}
perf<-performance(pred, "ecost")

# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve SVM HR-Employee-Attrition.csv [test]")

#============================================================


# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Generate a Lift Chart for the ksvm model on HR-Employee-Attrition.csv [train].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Also convert rate of positive predictions to percentage

per <- performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition),"lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Now plot the lift.

# Calling the function directly (.plot.performance) does work.

.plot.performance(per, col="#00CCCCFF", lty=2, add=TRUE)

# Add a legend to the plot.

legend("topright", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="SVM", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  HR-Employee-Attrition.csv ")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM HR-Employee-Attrition.csv [test] Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#============================================================
 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the ksvm model on HR-Employee-Attrition.csv [train].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ksvm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  HR-Employee-Attrition.csv ")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the ksvm model on HR-Employee-Attrition.csv [train].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ksvm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  HR-Employee-Attrition.csv ")
grid()

#============================================================
 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Attrition) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 0.64 secs

#============================================================



# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Attrition) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="laplacedot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 0.75 secs

#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Attrition ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(minbucket=5,
                                         cp=0.000010,
                                         usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.10 secs

#============================================================


# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree HR-Employee-Attrition.csv $ Attrition")
#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Attrition ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(minbucket=5,
                                         usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.08 secs

#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# The `ada' package implements the boost algorithm.

# Build the Extreme Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Attrition ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 2.32 secs

# Display tree number 1.

drawTreesAda(crs$ada, 1, ": HR-Employee-Attrition.csv $ Attrition")

Plot the error rate as we increase the number of trees.

plot(crs$ada)

# Display tree number 1.

listTreesAda(crs$ada, 1)

#============================================================


# Extreme Boost 

# The `ada' package implements the boost algorithm.

# Build the Extreme Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Attrition ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))



# Display tree number 1.

listTreesAda(crs$ada, 1)

#============================================================


# Extreme Boost 

# The `ada' package implements the boost algorithm.

# Build the Extreme Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Attrition ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 1.85 secs


#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.



crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
print(riskchart(crs$pr, 
                crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, 
                title="Performance Chart Decision Tree HR-Employee-Attrition.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.


crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
print(riskchart(crs$pr, 
                crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition, 
                title="Performance Chart Extreme Boost HR-Employee-Attrition.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================


# Evaluate model performance on the testing dataset. 

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the Decision Tree model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
  for (j in seq_along(perf1@x.values[[i]]))
  {
    lines(c(0,1),c(perf1@y.values[[i]][j],
                   perf1@x.values[[i]][j]),
          col=terrain.colors(10)[i],lty=3)
  }
}
perf<-performance(pred, "ecost")

# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve Decision Tree HR-Employee-Attrition.csv [test]")

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the Extreme Boost model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
  for (j in seq_along(perf1@x.values[[i]]))
  {
    lines(c(0,1),c(perf1@y.values[[i]][j],
                   perf1@x.values[[i]][j]),
          col=terrain.colors(10)[i],lty=3)
  }
}
perf<-performance(pred, "ecost")


# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve Extreme Boost HR-Employee-Attrition.csv [test]")

#============================================================


# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the rpart model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#00CCCCFF", lty=2, xlab="Caseload (%)", add=TRUE)

# Add a legend to the plot.

legend("topright", c("rpart","ada"), col=rainbow(2, 1, .8), lty=1:2, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  HR-Employee-Attrition.csv [test]",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Generate a Lift Chart for the ada model on HR-Employee-Attrition.csv [train].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Also convert rate of positive predictions to percentage

per <- performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition),"lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Now plot the lift.

# Calling the function directly (.plot.performance) does work.

.plot.performance(per, col="#00CCCCFF", lty=2, add=TRUE)

# Add a legend to the plot.

legend("topright", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="Extreme Boost", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  HR-Employee-Attrition.csv ")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Extreme Boost HR-Employee-Attrition.csv [test] Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#============================================================


# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the ada model on HR-Employee-Attrition.csv [train].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ada", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  HR-Employee-Attrition.csv ")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the ada model on HR-Employee-Attrition.csv [train].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ada", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  HR-Employee-Attrition.csv ")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the rpart model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#00CC00FF", lty=2, xlab="Caseload (%)", add=TRUE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#0000CCFF", lty=3, xlab="Caseload (%)", add=TRUE)

# Add a legend to the plot.

legend("topright", c("rpart","ada","ksvm"), col=rainbow(3, 1, .8), lty=1:3, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  HR-Employee-Attrition.csv [test]")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree HR-Employee-Attrition.csv [test] Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Extreme Boost HR-Employee-Attrition.csv [test] Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM HR-Employee-Attrition.csv [test] Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#============================================================


# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rpart model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#00CC00FF", lty=2, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#0000CCFF", lty=3, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","ksvm"), col=rainbow(3, 1, .8), lty=1:3, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  HR-Employee-Attrition.csv [test]")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rpart model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ada model on HR-Employee-Attrition.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#00CC00FF", lty=2, add=TRUE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ksvm model on HR-Employee-Attrition.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#0000CCFF", lty=3, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","ksvm"), col=rainbow(3, 1, .8), lty=1:3, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  HR-Employee-Attrition.csv [test]")
grid()

# Load the dataset from file.

fname <- "C:/AML - BUAN 6341/diabetes.csv" 
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

#============================================================


# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=768 train=537 validate=115 test=116

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("Pregnancies", "Glucose", "BloodPressure",
                   "SkinThickness", "Insulin", "BMI",
                   "DiabetesPedigreeFunction", "Age")

crs$numeric   <- c("Pregnancies", "Glucose", "BloodPressure",
                   "SkinThickness", "Insulin", "BMI",
                   "DiabetesPedigreeFunction", "Age")

crs$categoric <- NULL

crs$target    <- "Outcome"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================


# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Outcome) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="vanilladot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 0.87 secs

#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Outcome) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm


#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Outcome) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="laplacedot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm



#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.


crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
print(riskchart(crs$pr, 
                na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, 
                title="Performance Chart SVM diabetes.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================


# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.


crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
print(riskchart(crs$pr, 
                na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome, 
                title="Performance Chart SVM diabetes.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================


# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on diabetes.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM diabetes.csv [test] Outcome")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#============================================================


# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ksvm model on diabetes.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Generate a Lift Chart for the ksvm model on diabetes.csv [train].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Also convert rate of positive predictions to percentage

per <- performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome),"lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Now plot the lift.

# Calling the function directly (.plot.performance) does work.

.plot.performance(per, col="#00CCCCFF", lty=2, add=TRUE)

# Add a legend to the plot.

legend("topright", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="SVM", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  diabetes.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Outcome ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(cp=0.000010,
                                         usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")



#============================================================


# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree diabetes.csv $ Outcome")

#============================================================


# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Outcome ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(cp=0.000010,
                                         usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Outcome ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Outcome ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")


#============================================================


# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree diabetes.csv $ Outcome")

#============================================================


# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Outcome ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Outcome ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")



#============================================================


# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree diabetes.csv $ Outcome")

#============================================================


# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Outcome ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")



#============================================================


# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.



crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
print(riskchart(crs$pr, 
                crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, 
                title="Performance Chart Decision Tree diabetes.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================


# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the rpart model on diabetes.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Generate a Lift Chart for the rpart model on diabetes.csv [train].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Also convert rate of positive predictions to percentage

per <- performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome),"lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Now plot the lift.

# Calling the function directly (.plot.performance) does work.

.plot.performance(per, col="#00CCCCFF", lty=2, add=TRUE)

# Add a legend to the plot.

legend("topright", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="Decision Tree", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  diabetes.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on diabetes.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree diabetes.csv [test] Outcome")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

# Extreme Boost 

# The `ada' package implements the boost algorithm.

# Build the Extreme Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Outcome ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=60)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 0.94 secs

# Display tree number 1.

drawTreesAda(crs$ada, 1, ": diabetes.csv $ Outcome")

#============================================================


# Extreme Boost 

# The `ada' package implements the boost algorithm.

# Build the Extreme Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Outcome ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=60)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))



# Plot the error rate as we increase the number of trees.

plot(crs$ada)
# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$sample, c(crs$input, crs$target)]$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================


# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.



crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
print(riskchart(crs$pr, 
                crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome, 
                title="Performance Chart Extreme Boost diabetes.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================


# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ada model on diabetes.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Generate a Lift Chart for the ada model on diabetes.csv [train].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Also convert rate of positive predictions to percentage

per <- performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome),"lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Now plot the lift.

# Bug in ROCR 1.0-3 plot does not obey the add command.# Calling the function directly (.plot.performance) does work.

.plot.performance(per, col="#00CCCCFF", lty=2, add=TRUE)

# Add a legend to the plot.

legend("topright", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="Extreme Boost", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  diabetes.csv ")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on diabetes.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Extreme Boost diabetes.csv [test] Outcome")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")
# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the rpart model on diabetes.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ada model on diabetes.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#00CC00FF", lty=2, xlab="Caseload (%)", add=TRUE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ksvm model on diabetes.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#0000CCFF", lty=3, xlab="Caseload (%)", add=TRUE)

# Add a legend to the plot.

legend("topright", c("rpart","ada","ksvm"), col=rainbow(3, 1, .8), lty=1:3, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  diabetes.csv [test]")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rpart model on diabetes.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on diabetes.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#00CC00FF", lty=2, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on diabetes.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#0000CCFF", lty=3, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","ksvm"), col=rainbow(3, 1, .8), lty=1:3, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  diabetes.csv [test]")
grid()

#============================================================


# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rpart model on diabetes.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ada model on diabetes.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#00CC00FF", lty=2, add=TRUE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ksvm model on diabetes.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Outcome)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#0000CCFF", lty=3, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","ksvm"), col=rainbow(3, 1, .8), lty=1:3, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  diabetes.csv [test]")
grid()

