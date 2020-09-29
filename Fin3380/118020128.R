###Q1###
matrix1 = matrix((1:7),10,10)
matrix1[7,3] = NA
matrix1

matrix2 =  matrix1[which(matrix1[,3] > 3),]
matrix2


###Q2###
MatrixSubsection = function(rawMatrix, rowIndex, columnIndex){
  ##In case the matrix is converted into vector
  newMatrix = rawMatrix[rowIndex,columnIndex,drop=F]
  return(newMatrix)
}

#test1
print(MatrixSubsection(matrix1,1,3))

#test2
print(MatrixSubsection(matrix1,3:4,5))

#test3
print(MatrixSubsection(matrix1,-5,3))


###Q3###
MatrixSubsection2 = function(rawMatrix, rowIndex, columnIndex){
  ##-11 is also invalid
  rowLogic = rowIndex %in% -10:10
  columnLogic = columnIndex %in% -10:10
  ##If one of them is false, then the final answer is false
  if(rowLogic&&columnLogic){
    newMatrix = rawMatrix[rowIndex,columnIndex,drop=F]
    return(newMatrix)
  }
  else{
    return('Dimension Smaller than Index!')
  }
}

#test1
print(MatrixSubsection2(matrix1,11,1))

#test2
print(MatrixSubsection2(matrix1,20:21,20:21))


###Q4###
SmallMatrices = function(rawMatrix){
  ##In case the matrix is not a square matrix
  minDimension=min(dim(rawMatrix))-1
  matrixList=list()
  for(i in 1:minDimension){
    ##list() can make sure of the functionality of append()
    newMatrix = list(rawMatrix[i:(i+1),i:(i+1)])
    matrixList = append(matrixList, newMatrix)
  }
  return(matrixList)
}
print(SmallMatrices(matrix1))


###Q5###
a = factor(c(0,1,3.5))
as.numeric(as.character(a))