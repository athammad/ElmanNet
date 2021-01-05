rm(list=ls())

#ELMAN GRID SEARCH 
library(data.table)
library(lubridate)
library(caret)
library(signal)
library(RSNNS)
library(parallel)
library(pbapply)

allDistrics<-c('Abiansemal','Petang','Megwi','Kuta','KutaS','KutaU')


tabModels<-lapply(allDistrics, function(x){
  
  veg_outcomes<-fread(paste0(x,'.csv'))
  veg_outcomes$Date<-as.Date(veg_outcomes$Date)
  
  
  #-----------------------------------------------------
  #sine end cosine of time 
  #-----------------------------------------------------
  sinner<-as.data.table(cyclic_encoding(veg_outcomes$Date, c("week", "month","year")))
  veg_outcomes<-cbind(veg_outcomes,sinner)
  veg_outcomes$ydaynum<-yday(veg_outcomes$Date)
  
  #save the after lockdown for later
  veg_outcomesTest<-veg_outcomes[treat==1,]
  #select only before lockdown
  veg_outcomes<-veg_outcomes[treat==0,]
  veg_outcomes$treat<-NULL
  
  
  #-----------------------------------------------------
  # SELECT VARS
  #-----------------------------------------------------
  
  names(veg_outcomes)
  cols <- c("EVI","Rain","Hum","Temp","Wind","ET","ydaynum",
            grep("sin",names(veg_outcomes),value = T), 
            grep("cos",names(veg_outcomes),value = T)
  )
  veg_outcomes<-na.omit(veg_outcomes[,..cols])
  setDF(veg_outcomes)
  
  #define the target variable
  names(veg_outcomes)
  target <- "EVI"
  #define the predictor features
  predictors <-setdiff(names(veg_outcomes),target)
  
  veg_outcomes[3378,] # training 
  TrainDataTS = veg_outcomes[1:3378, ]
  dim(TrainDataTS)
  testDataTS = veg_outcomes[3379:nrow(veg_outcomes), ] #test 
  tail(testDataTS)
  dim(testDataTS)
  
  setDT(TrainDataTS)
  setDT(testDataTS)
  #=====================================================
  # ELMAN 
  #=====================================================

  #The number of hidden neurons should be between the size of the input layer and the size of the output layer.
  dim(TrainDataTS)[2]-1
  #The number of hidden neurons should be 2/3 the size of the input layer, plus the size of the output layer.
  round(((dim(TrainDataTS)[2]+1)/3)*2)
  #The number of hidden neurons should be less than twice the size of the input layer.
  
  
  hyper_grid <- expand.grid(
    #Sens
    Points=c(7,15,31),
    Myorders=c(1:4),
    #Elmar Tuner
    nHid= c(9:13),
    inFunc=c('JE_BP','JE_Rprop','JE_Quickprop'),
    learnfun=seq.default(0.00001,0.001,0.00005),
    epoche=c(c(5:20),seq.default(200,1000,50)),
    stringsAsFactors = FALSE)
  
  setDT(hyper_grid)
  hyper_grid<-hyper_grid[sample(.N,300)]
  fit_and_extract_metrics <- function(x) {
    
    dataframe.segment =as.data.frame(x)
    cols<-c("EVI","Rain","Hum","Temp","Wind","ET")
    TrainDataTS[ , (cols) := lapply(.SD,sgolayfilt,p=dataframe.segment$Myorders, n=dataframe.segment$Points, ts=1), .SDcols = cols]
    testDataTS[, (cols) := lapply(.SD, sgolayfilt,p=dataframe.segment$Myorders, n=dataframe.segment$Points, ts=1), .SDcols = cols]
    
    setDF(TrainDataTS)
    setDF(testDataTS)
    
    megaList<-list('inputsTrain'=TrainDataTS[,predictors],
                   'targetsTrain'=TrainDataTS[,target],
                   'inputsTest'=testDataTS[,predictors],
                   'targetsTest'=testDataTS[,target]
    )
    NormLIST<-normTrainingAndTestSet(megaList, dontNormTargets = FALSE, type = "norm")
    
    

    set.seed(1989)
    model <- elman(NormLIST$inputsTrain,
                   NormLIST$targetsTrain,
                   size = c(dataframe.segment$nHid,dataframe.segment$nHid,dataframe.segment$nHid),#c(10,10,10)   c(20,20,20)
                   learnFuncParams =dataframe.segment$learnfun ,#c(0.0001),
                   maxit = dataframe.segment$epoche,#1000
                   learnFunc = dataframe.segment$inFunc,
                   linOut = TRUE,
                   inputsTest =NormLIST$inputsTest,
                   targetsTest =NormLIST$targetsTest)
    
    
    return( data.frame(RMSEtrain =  caret::RMSE(model$fitted.values,NormLIST$targetsTrain),
                       RMSEtest =  caret::RMSE(model$fittedTestValues,NormLIST$targetsTest),
                       MAEtrain =  caret::MAE(model$fitted.values,NormLIST$targetsTrain),
                       MAEtest =  caret::MAE(model$fittedTestValues,NormLIST$targetsTest),
                       R2train =  round(summary(lm(model$fitted.values ~NormLIST$targetsTrain))$r.squared,4),
                       R2Etest =  round(summary(lm(model$fittedTestValues~NormLIST$targetsTest))$r.squared,4)
    ))
  }
  
  
#run all the analysis
  
  resGrid <-pblapply(cl=10,split(hyper_grid, 1:nrow(hyper_grid)), fit_and_extract_metrics)
  resGrid <-rbindlist(resGrid)
  hyper_grid<-cbind(hyper_grid,resGrid)

  hyper_grid

})


names(tabModels)<-allDistrics

FinalTabRes<-rbindlist(tabModels,idcol = 'District')

fwrite(FinalTabRes,'FinalTabRes.csv')



#=================================================
FinalTabRes<-fread('FinalTabRes.csv')
