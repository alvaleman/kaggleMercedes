require(xgboost)
require(Matrix)


setwd("F:/Kaggle/Mercedes")


dat = read.csv("train.csv")

test = read.csv("test.csv")


y = dat$y

logy = log(y)


dat$y = NULL

totalDat = rbind(dat, test)




#namingConvention => 1: dat$bo_OrigVarName.BinaryVar, 2: dat$bo_BinaryVar, 3: dat$bo_binaryvar
convertLevelsToBinaries = function(mergedDat, vectorName, nameConv=1){
  
  topN = levels(mergedDat[,vectorName])
  
  topNVar = gsub(" ", "", topN, fixed = TRUE)
  topNVar = gsub("/", "", topNVar, fixed = TRUE)
  topNVar = gsub("&", "", topNVar, fixed = TRUE)
  topNVar = gsub(":", "", topNVar, fixed = TRUE)
  topNVar = gsub(",", "", topNVar, fixed = TRUE)
  topNVar = gsub("-", "", topNVar, fixed = TRUE)
  topNVar = gsub("*", "", topNVar, fixed = TRUE)
  topNVar = gsub("[(]", "", topNVar, fixed = TRUE)
  topNVar = gsub("[)]", "", topNVar, fixed = TRUE)
  topNVar = gsub("[@]", "", topNVar, fixed = TRUE)
  topNVar = gsub("[!]", "", topNVar, fixed = TRUE)
  
  
  for(i in 1:length(topN)){
    
    
    if(nameConv == 1){
      #Go through each level and create binary variable
      eval(parse(text = paste0("mergedDat$bo_",gsub(" ", "",paste0(vectorName,".",topNVar[i])),"=ifelse(mergedDat$",vectorName,"=='",topN[i],"',1,0)")))
      
      #Make NAs be 0 
      eval(parse(text = paste0("mergedDat$bo_",gsub(" ", "",paste0(vectorName,".",topNVar[i])),"=factor(ifelse(is.na(mergedDat$bo_",gsub(" ", "",paste0(vectorName,".",topNVar[i])),"),0,mergedDat$bo_",gsub(" ", "",paste0(vectorName,".",topNVar[i])),"))")))
    }else{
      
      
      if(nameConv== 2){
        
        
        #Go through each level and create binary variable
        eval(parse(text = paste0("mergedDat$bo_",gsub(" ", "",paste0(topNVar[i])),"=ifelse(mergedDat$",vectorName,"=='",topN[i],"',1,0)")))
        
        #Make NAs be 0 
        eval(parse(text = paste0("mergedDat$bo_",gsub(" ", "",paste0(topNVar[i])),"=factor(ifelse(is.na(mergedDat$bo_",gsub(" ", "",paste0(topNVar[i])),"),0,mergedDat$bo_",gsub(" ", "",paste0(topNVar[i])),"))")))
        
        
      }else{
        
        if(nameConv== 3){
          
          
          #Go through each level and create binary variable
          eval(parse(text = paste0("mergedDat$bo_",gsub(" ", "",tolower(paste0(topNVar[i]))),"=ifelse(mergedDat$",vectorName,"=='",topN[i],"',1,0)")))
          
          #Make NAs be 0 
          eval(parse(text = paste0("mergedDat$bo_",gsub(" ", "",tolower(paste0(topNVar[i]))),"=factor(ifelse(is.na(mergedDat$bo_",gsub(" ", "",tolower(paste0(topNVar[i]))),"),0,mergedDat$bo_",gsub(" ", "",tolower(paste0(topNVar[i]))),"))")))
        }
        
      }
    }
  }
  
  return(mergedDat)
}

#e.g. convert all majors to binary
#mergedDat = convertLevelsToBinaries(mergedDat, "Major", 1)



convertNumericToFactors = function(data){
  #Convert variables that should be factors to factors
  for(i in 1:length(data[1,])){
    if(is.numeric(data[,i])){
      if(max(data[,i], na.rm=TRUE)==1 & min(data[,i], na.rm = TRUE)==0 & 
         (nlevels(as.factor(data[,i])))==2){
        data[,i] <- factor(data[,i])
        
      }
    } 
    print(nlevels(data[,i]))}
  data
}


dat = convertNumericToFactors(dat)


factorCols = c(0:6,8)

for(i in 1:length(factorCols)){

  eval(parse(text = paste0("totalDat = convertLevelsToBinaries(totalDat, 'X",factorCols[i],"')")))

}

totalDat = convertNumericToFactors(totalDat)

modelDat = totalDat[,c(10:ncol(totalDat))]

trainDat = modelDat[1:4209,]

testDat = modelDat[4210:nrow(modelDat),]

#trainDat$y = logy

testDat = sapply(testDat, as.numeric)
trainDat = sapply(trainDat, as.numeric)

dtrain = xgb.DMatrix(as.matrix(trainDat), label=logy)
dtest = xgb.DMatrix(as.matrix(testDat))

#mercedesboost = xgboost(data = trainMat, label = trainDat$y , nthread = 3, nrounds = 9)

xgb_params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.9,
  eta = 0.005,
  objective = 'reg:linear',
  max_depth = 4,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = mean(logy)
)


ids = test$ID


best_nrounds = 678

gbdt = xgb.train(xgb_params, dtrain, best_nrounds)

testPreds = predict(gbdt, dtest)

testPreds = exp(testPreds)

kagglePreds = data.frame(ids, testPreds)
names(kagglePreds) = c("ID", "y")

