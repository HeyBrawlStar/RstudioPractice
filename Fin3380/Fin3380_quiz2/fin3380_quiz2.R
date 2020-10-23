#Q1
a = matrix(1:7,10,10)
a[7,3] = NA
print(a)
b = a[which(a[,3]>3),]
print(b)

#Q2
subsetMatrix = function(matrixName, rowIndex, columnIndex){
  return(matrixName[rowIndex,columnIndex, drop = F])
}

subsetMatrix(a,1,3)
subsetMatrix(a,3:4,5)
subsetMatrix(a,-5,3)




#Q3
subsetMatrix2 = function(matrixName, rowIndex, columnIndex){
  rowLogic = rowIndex %in% -nrow(matrixName):nrow(matrixName)
  columnLogic = columnIndex %in% -ncol(matrixName):ncol(matrixName)
  if(rowLogic&&columnLogic){
    newMatrix = matrixName[rowIndex,columnIndex,drop=F]
    return(newMatrix)
  }
  else{
    return('Dimension Smaller than Index!')
  }
}
print(subsetMatrix2(a,11,1))
print(subsetMatrix2(a,20:21,20:21))




#Q4
SmallMatrices = function(matrixName){
  matrixList=list()
  for(i in 1:min(dim(matrixName))-1){
    newMatrix = list(matrixName[i:(i+1),i:(i+1)])
    matrixList = append(matrixList, newMatrix)
  }
  return(matrixList)
}
print(SmallMatrices(a))





#Q5
a = factor(c(0,1,3.5))
as.numeric(as.character(a))




