# Npolishchuk March 10, 2019
# Using csv files from the Chicago Data Portal, demonstrate how to use caret package for classification.

# The people involved
Traffic_Crashes_._People <- read.csv("Traffic_Crashes_-_People.csv", na.strings="")
View(Traffic_Crashes_._People)
str(Traffic_Crashes_._People)
summary(Traffic_Crashes_._People$INJURY_CLASSIFICATION)
 #FATAL    INCAPACITATING INJURY  NO INDICATION OF INJURY 
 # 239                     4603      555392 
 # NONINCAPACITATING INJURY    REPORTED, NOT EVIDENT 
 # 22551                    14624 
 #                   NA's 
 #                   296 
Traffic_Crashes_._People$CRASH_DATE <- as.Date(Traffic_Crashes_._People$CRASH_DATE, format = '%m/%d/%Y')
min(Traffic_Crashes_._People$CRASH_DATE)
# [1] "2013-03-03"
max(Traffic_Crashes_._People$CRASH_DATE)
# [1] "2019-03-04"
summary(Traffic_Crashes_._People$PERSON_TYPE)
# BICYCLE -  3828   DRIVER- 471389 NON-CONTACT VEHICLE - 107    NON-MOTOR VEHICLE -  500   PASSENGER -  115359     PEDESTRIAN-  652                  

# The circumstances of the accident
Traffic_Crashes_._Crashes <- read.csv("Traffic_Crashes_-_Crashes.csv", encoding="UTF-8", na.strings="", stringsAsFactors=FALSE)
str(Traffic_Crashes_._Crashes)
cols <- c("CRASH_MONTH", "CRASH_DAY_OF_WEEK","CRASH_HOUR", "NUM_UNITS","WORKERS_PRESENT_I","WORK_ZONE_I","DOORING_I","SEC_CONTRIBUTORY_CAUSE","PRIM_CONTRIBUTORY_CAUSE","HIT_AND_RUN_I","NOT_RIGHT_OF_WAY_I","INTERSECTION_RELATED_I","ROAD_DEFECT", "ROADWAY_SURFACE_COND", "ALIGNMENT","LANE_CNT","TRAFFICWAY_TYPE","FIRST_CRASH_TYPE","LIGHTING_CONDITION","WEATHER_CONDITION","DEVICE_CONDITION","TRAFFIC_CONTROL_DEVICE","POSTED_SPEED_LIMIT","X.U.FEFF.RD_NO")
crashes <- Traffic_Crashes_._Crashes[,cols]

# join together for just the bicycle crashes
cyclists <- Traffic_Crashes_._People[Traffic_Crashes_._People$PERSON_TYPE == 'BICYCLE',]
summary(cyclists)
cols <- c('RD_NO', 'CITY','SEX','AGE','SAFETY_EQUIPMENT','EJECTION','INJURY_CLASSIFICATION','DRIVER_ACTION','DRIVER_VISION','PEDPEDAL_ACTION','PEDPEDAL_VISIBILITY','PEDPEDAL_LOCATION')
cyclists[,cols]
cycCrash <- merge(cyclists, crashes, by.x = 'RD_NO', by.y = 'X.U.FEFF.RD_NO')
cols <- c('POSTED_SPEED_LIMIT', 'INTERSECTION_RELATED_I','NOT_RIGHT_OF_WAY_I','WORK_ZONE_I','WORKERS_PRESENT_I','NUM_UNITS')
cycCrash <- cycCrash[,!(names(cycCrash) %in% cols)]
cols <- c('DOORING_I','SEC_CONTRIBUTORY_CAUSE', 'PRIM_CONTRIBUTORY_CAUSE','HIT_AND_RUN_I','ROAD_DEFECT','ROADWAY_SURFACE_COND','ALIGNMENT','TRAFFICWAY_TYPE',   'FIRST_CRASH_TYPE','LIGHTING_CONDITION','WEATHER_CONDITION','DEVICE_CONDITION','TRAFFIC_CONTROL_DEVICE')
cycCrash[cols] <- lapply(cycCrash[cols], factor)

#Deal with NAs
cycCrash$AGE <- ifelse(is.na(cycCrash$AGE),29,cycCrash$AGE) # the median age
cycCrash$CITY[is.na(cycCrash$CITY)] = 'UNKNOWN'
cycCrash$SEX[is.na(cycCrash$SEX)] = 'U'
cycCrash$SAFETY_EQUIPMENT[is.na(cycCrash$SAFETY_EQUIPMENT)] = 'USAGE UNKNOWN'
cycCrash$EJECTION[is.na(cycCrash$EJECTION)] = 'NONE'
cycCrash$DRIVER_ACTION[is.na(cycCrash$DRIVER_ACTION)] = 'NONE'
cycCrash$HIT_AND_RUN_I[is.na(cycCrash$HIT_AND_RUN_I)] = 'N'
cycCrash$DOORING_I[is.na(cycCrash$DOORING_I)] = 'N' 
#for both doorings and hit and runs, assume lack of answer means no
cycCrash$LANE_CNT[is.na(cycCrash$LANE_CNT)] = 2.0
cycCrash$DRIVER_VISION[is.na(cycCrash$DRIVER_VISION)] = 'UNKNOWN'

#Convert to a binary classification problem and clean up a few more vars
cycCrash$Injured <- ifelse(cycCrash$INJURY_CLASSIFICATION == "NO INDICATION OF INJURY", "OK","Hurt")
cycCrash$Injured <- as.factor(cycCrash$Injured)
cols <- c("CITY", "INJURY_CLASSIFICATION")
cycCrash <- cycCrash[,!(names(cycCrash) %in% cols)]
cols <- c("CRASH_MONTH","CRASH_DAY_OF_WEEK","CRASH_HOUR")
cycCrash[cols] <- lapply(cycCrash[cols], factor)
cycCrash <- droplevels(cycCrash,reorder = TRUE)
#those levels that rarely occur will cause problems when they are only in test or train set
rare <- c('IMPROPER PARKING','CELL PHONE USE OTHER THAN TEXTING', 'EVADING POLICE VEHICLE','TEXTING')
cycCrash$DRIVER_ACTION[cycCrash$DRIVER_ACTION %in% rare] <- 'OTHER'
rare <- c('BLINDED - HEADLIGHTS','TREES, PLANTS','WINDSHIELD (WATER/ICE)')
cycCrash$DRIVER_VISION[cycCrash$DRIVER_VISION %in% rare] <- 'OTHER'
rare <- c('PLAYING/WORKING ON VEHICLE','SCHOOL BUS (WITHIN 50 FT.)','STANDING IN ROADWAY','PLAYING IN ROADWAY','TO/FROM DISABLED VEHICLE','WAITING FOR SCHOOL BUS','WORKING IN ROADWAY')
cycCrash$PEDPEDAL_ACTION[cycCrash$PEDPEDAL_ACTION %in% rare] <- 'OTHER ACTION'
rare <- c('PHYSICAL CONDITION OF DRIVER','ROAD CONSTRUCTION/MAINTENANCE','ROAD ENGINEERING/SURFACE/MARKING DEFECTS','TEXTING','TURNING RIGHT ON RED','IMPROPER BACKING','EQUIPMENT - VEHICLE CONDITION')
cycCrash$SEC_CONTRIBUTORY_CAUSE[cycCrash$SEC_CONTRIBUTORY_CAUSE %in% rare] <- 'UNABLE TO DETERMINE'
cycCrash$SEC_CONTRIBUTORY_CAUSE[cycCrash$SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING YIELD SIGN'] <- 'DISREGARDING OTHER TRAFFIC SIGNS'
cycCrash$SEC_CONTRIBUTORY_CAUSE[cycCrash$SEC_CONTRIBUTORY_CAUSE == 'ANIMAL'] <- 'DISTRACTION - FROM OUTSIDE VEHICLE'
InCar <- c('CELL PHONE USE OTHER THAN TEXTING','DISTRACTION - OTHER ELECTRONIC DEVICE (NAVIGATION DEVICE, DVD PLAYER, ETC.)')
cycCrash$SEC_CONTRIBUTORY_CAUSE[cycCrash$SEC_CONTRIBUTORY_CAUSE %in% InCar] <- 'DISTRACTION - FROM INSIDE VEHICLE'
rare <- c('PHYSICAL CONDITION OF DRIVER','ROAD CONSTRUCTION/MAINTENANCE','ROAD ENGINEERING/SURFACE/MARKING DEFECTS','EQUIPMENT - VEHICLE CONDITION','EXCEEDING SAFE SPEED FOR CONDITIONS')
cycCrash$PRIM_CONTRIBUTORY_CAUSE[cycCrash$PRIM_CONTRIBUTORY_CAUSE == 'ANIMAL'] <- 'DISTRACTION - FROM OUTSIDE VEHICLE'
InCar <- c('CELL PHONE USE OTHER THAN TEXTING','DISTRACTION - OTHER ELECTRONIC DEVICE (NAVIGATION DEVICE, DVD PLAYER, ETC.)','TEXTING')
cycCrash$PRIM_CONTRIBUTORY_CAUSE[cycCrash$PRIM_CONTRIBUTORY_CAUSE %in% InCar] <- 'DISTRACTION - FROM INSIDE VEHICLE'
cycCrash$PRIM_CONTRIBUTORY_CAUSE[cycCrash$PRIM_CONTRIBUTORY_CAUSE == 'HAD BEEN DRINKING (USE WHEN ARREST IS NOT MADE)'] <- 'OPERATING VEHICLE IN ERRATIC, RECKLESS, CARELESS, NEGLIGENT OR AGGRESSIVE MANNER'
cycCrash$PRIM_CONTRIBUTORY_CAUSE[cycCrash$PRIM_CONTRIBUTORY_CAUSE == 'UNDER THE INFLUENCE OF ALCOHOL/DRUGS (USE WHEN ARREST IS EFFECTED)'] <- 'OPERATING VEHICLE IN ERRATIC, RECKLESS, CARELESS, NEGLIGENT OR AGGRESSIVE MANNER'
cycCrash$ROAD_DEFECT[cycCrash$ROAD_DEFECT == 'DEBRIS ON ROADWAY'] <- 'OTHER'
cycCrash$ROADWAY_SURFACE_COND[cycCrash$ROADWAY_SURFACE_COND == 'ICE'] = 'SNOW OR SLUSH'
cycCrash$ROADWAY_SURFACE_COND[cycCrash$ROADWAY_SURFACE_COND == 'SAND, MUD, DIRT'] = 'OTHER'
cycCrash <- within(cycCrash, rm("FIRST_CRASH_TYPE"))
cycCrash <- within(cycCrash, rm("ALIGNMENT")) #pointless bc Chi is so flat
rare <- c('FOG/SMOKE/HAZE','SLEET/HAIL')
cycCrash$WEATHER_CONDITION[cycCrash$WEATHER_CONDITION %in% rare] <- 'OTHER'
rare <- c(' MISSING','WORN REFLECTIVE MATERIAL')
cycCrash$DEVICE_CONDITION[cycCrash$DEVICE_CONDITION %in% rare] <- 'OTHER'
rare <- c('DELINEATORS','POLICE/FLAGMAN','YIELD','RAILROAD CROSSING GATE','SCHOOL ZONE','OTHER REG. SIGN')
cycCrash$TRAFFIC_CONTROL_DEVICE[cycCrash$TRAFFIC_CONTROL_DEVICE %in% rare] <- 'OTHER WARNING SIGN'
cycCrash <- droplevels(cycCrash,reorder = TRUE)


#begin analysis by splitting the data  (uses code modified from Introduction to Machine Learning).  We don't actually require a stratified sample since classes are 2:1, but it's nice for imbalanced data sets and won't hurt.
require(caret)
set.seed(412)
indexes <- createDataPartition(cycCrash$Injured, times = 1,
    p = 0.7, list = FALSE)
cycTrain <- cycCrash[indexes, ]
cycTest <- cycCrash[-indexes, ]

 cycTrain <- na.omit(cycTrain) # despite all the cleaning, 4 nas somewhere.  Small enough no. to ignore
 cycTest <- na.omit(cycTest) # 1 in test set
 cycTrain <- within(cycTrain, rm("RD_NO"))
 cycTest <- within(cycTest, rm("RD_NO"))

 #categorical variables don't have to be encoded, train does it for you.  Here's how you would do it
 # dummies_model <- dummyVars(Injured ~ ., data=cycTrain)
# Create the dummy variables using predict. 
# cycTrain_mat <- predict(dummies_model, newdata = cycTrain)
# Convert to dataframe
# cyctrainData <- data.frame(cycTrain_mat)
# cycTest_mat <- predict(dummies_model, newdata = cycTest)
# cycTestData <- data.frame(cycTest_mat)
 
#run through 6 models - random ferns, Bayesian generalized linear model,  Logistic Model Trees,  two types of support vector machine kernels, and adaboost classification trees 
#Imbalanced vars, ie missing from cross val ? train_control <- trainControl(method="cv", sampling = "up")
classMods <- c("rFerns","bayesglm","LMT", "svmLinear", "svmRadial", "adaboost")
compare.model <- c()
for(i in classMods) {
print(i) # tells you what model is being worked on in case you have errors or what to check on progress
model <- train(Injured ~ ., data = cycTrain, method = i, metric = 'Accuracy')
pred <- predict(model, newdata = cycTest)
pred.metric <- postResample(pred, cycTest$Injured)
compare.model <- cbind(compare.model , pred.metric)
}
colnames(compare.model) <- classMods
compare.model
         pred.metric pred.metric pred.metric pred.metric pred.metric
# Metrics in same order as classMods (except adaboost)
# Accuracy   0.6315331   0.6663763   0.7500000   0.6628920   0.6698606
# Kappa      0.2361931   0.2364954   0.4038705   0.2215572   0.1783089
#Prev runs showed that AdaBoost did really well (0.72, 0.37) but takes 4+ hrs to run


# caretensemble  - combine a cpl of the models
# for help, see example at https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
require(caretEnsemble)
# boot strap train instead of cv bc some vars have rare levels
train.control <- trainControl(method = "boot", number = 5, savePredictions ="final", classProbs=TRUE, index=createResample(cycTrain$Injured, 5), summaryFunction=twoClassSummary)
model_list <- caretList(Injured ~., data=cycTrain, trControl = train.control, methodList =c("bayesglm","LMT"))
predEnsemb <- as.data.frame(predict(model_list,newdata=cycTest))
xyplot(resamples(model_list)) #confirm model is uncorrelated by graphing & getting a measure
modelCor(resamples(model_list)) # only did 5 runs but accept corr 0.327
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
    ))  # the simple linear blend of models (alternative is a caretStack)
varImp(greedy_ensemble) # which features make the most diff.  Both models think the same features are important, so they may be too similar after all.  Most important are Ejection, Driver Vision, Safety Equipment, and Primary Contributory Cause.
summary(greedy_ensemble)
#The following models were ensembled: bayesglm, LMT 
#They were weighted: 
#1.2745 -1.8121 -1.1776
#The resulting ROC is: 0.712
#The fit for each individual model on the ROC is: 
#   method       ROC      ROCSD
# bayesglm 0.6770838 0.01453120
#      LMT 0.6836742 0.01528476

#nice that ROC combined is slightly better than individual

ens_preds <- predict(greedy_ensemble, newdata=cycTest, type="prob")
totPreds <- as.factor(ifelse(ens_preds < 0.50, "OK", "Hurt"))
resultMat <- confusionMatrix(totPreds, cycTest$Injured)
resultMat
#Confusion Matrix and Statistics

 #         Reference
#Prediction Hurt  OK
#      Hurt  560 181
#      OK    175 232
                                          
 #              Accuracy : 0.6899          
 #                95% CI : (0.6622, 0.7166)
  #  No Information Rate : 0.6402          
  #  P-Value [Acc > NIR] : 0.0002238       
                                          
  #                Kappa : 0.3247          
 #Mcnemar's Test P-Value : 0.7910099       
                                          
 #           Sensitivity : 0.7619          
  #          Specificity : 0.5617          
  #       Pos Pred Value : 0.7557          
   #      Neg Pred Value : 0.5700          
    #         Prevalence : 0.6402          
    #     Detection Rate : 0.4878          
 #  Detection Prevalence : 0.6455          
 #     Balanced Accuracy : 0.6618          
                                          
  #     'Positive' Class : Hurt    