
# in the NotSuit path
a <- list.files()

b <- as.data.frame(gsub('©'," ", a))
colnames(b) <- 'newName'

a <- as.data.frame(a)
colnames(a) <- 'oldName'

Names <- cbind(a,b)
Names$oldName <- as.character(Names$oldName)
Names$newName <- as.character(Names$newName)

path <- "C:/Users/m/Desktop/ML in R and Python/PhotosAnalysis/Augmented 224 Suit NotSuit/rawdata/NotSuit"

Names$oldName <- paste(path,Names$oldName, sep='/')
Names$newName <- paste(path,Names$newName, sep='/')

ChangeFileName <- function(x, y){
  for(i in seq_along(x)){
    file.rename(x[i], y[i])
}}
  
ChangeFileName(Names$oldName,Names$newName)

########################################################################################################
# in the suit path
a <- list.files()

b <- as.data.frame(gsub('©'," ", a))
colnames(b) <- 'newName'

a <- as.data.frame(a)
colnames(a) <- 'oldName'

Names <- cbind(a,b)
Names$oldName <- as.character(Names$oldName)
Names$newName <- as.character(Names$newName)

path <- "C:/Users/m/Desktop/ML in R and Python/PhotosAnalysis/Augmented 224 Suit NotSuit/rawdata/suit"

Names$oldName <- paste(path,Names$oldName, sep='/')
Names$newName <- paste(path,Names$newName, sep='/')

ChangeFileName <- function(x, y){
  for(i in seq_along(x)){
    file.rename(x[i], y[i])
  }}

ChangeFileName(Names$oldName,Names$newName)
