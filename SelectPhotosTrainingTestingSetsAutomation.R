# this program classifies the types of 3 for the suit, workshirt, or dressy attire
# in 390 photo images of the same person and similar poses for professional head shots
# the field that has the attire type is written to a separate csv file, and
# the photos are split into 273 training set images in a train folder and
# 117 testing set images in the test folder

# this will then be used to run the CNN kaggle code to see how well it works 
# for these image sizes

# Run while in the proPhotos folder PhotosAnalysis folder

list.files()
filePhotos <- list.files()

filePics <- as.data.frame(filePhotos)

suit <- as.data.frame(filePics[c(003:031,071:096,116:136,172:202,237:278,316:338),])
colnames(suit) <- 'dressAttire'
Type <- as.data.frame(rep('suit',172))
colnames(Type) <- 'type'

dressy <- as.data.frame(filePics[c(032:070,137:171,203:236,339:390),])
colnames(dressy) <- 'dressAttire'
dressType <- as.data.frame(rep('dressy', 160))
colnames(dressType) <- 'type'

workShirt <- as.data.frame(filePics[c(001:002,097:115,279:315),])
colnames(workShirt) <- 'dressAttire'
shirtType <- as.data.frame(rep('work shirt', 58))
colnames(shirtType) <- 'type'

Attire <- rbind(shirtType,dressType,Type)
fileName <- rbind(workShirt,dressy,suit)
###################################################################################

# write to parent directory outside of the proPhotos folder in the PhotosAnalysis folder

Photos <- cbind(fileName,Attire)
write.csv(Photos,'photosType.csv',row.names=FALSE)

train <- sample(1:390, 273, replace=FALSE)
trainingSet <- Photos[train,]
testingSet <- Photos[-train,]


#There are 390 photos, 3 classes of the image files as either 'dressy','workShirt',or'suit'
#the training set has 273 photos, and the testing set has 117 photos

trainingFiles <- t(as.data.frame(trainingSet$dressAttire))
testingFiles <- t(as.data.frame(testingSet$dressAttire))

# write to a parent folder outside the proPhotos folder
write.csv(trainingFiles,'trainingFiles.csv',row.names=FALSE)
write.csv(testingFiles,'testingFiles.csv', row.names=FALSE)

#use the csv comma separated entries of the file names to paste into python for CNN analysis
#ignoring the generic column names
############################################################


#run while in the PhotosAnalysis parent folder

testingFiles <- read.csv('testingFiles.csv', sep=',', header=TRUE)
trainingFiles <- read.csv('trainingFiles.csv', sep=',', header=TRUE)

trainn <- t(trainingFiles);row.names(trainn) <- NULL
colnames(trainn) <-'train'

testn <- t(testingFiles); row.names(testn) <- NULL
colnames(testn) <- 'test'

testResizedFiles <- sort(list.files('./testResized'))
trainResizedFiles <- sort(list.files('./trainResized'))
trainSortedFiles <- sort(trainn)
testSortedFiles <- sort(testn)

test1 <- testResizedFiles==testSortedFiles #files in testing table same as test folder
test2 <- trainResizedFiles==trainSortedFiles#files in training table same as train folder

write.csv(trainResizedFiles,'trainResizedFiles.csv', row.names=FALSE)
write.csv(testResizedFiles,'testResizedFiles.csv', row.names=FALSE)

ifelse(!file.exists('test'), dir.create('test'), test <- './test')
ifelse(!file.exists('train'), dir.create('train'), train <- './train')

proPhotos <- './proPhotos'
test <- './test'
train <- './train'

file.copy(proPhotos, test, overwrite=TRUE, recursive=TRUE)#copies all 390 files and the folder
file.copy(proPhotos, train, overwrite=TRUE, recursive=TRUE)#copies all 390 files and the folder

teList <- paste(test,'proPhotos',sep='/')
trList <- paste(train,'proPhotos', sep='/')

setwd(trList)# training set photos folder
file.remove(testn);list.files()#273 files not 390, correct

setwd('../');setwd('../');setwd(teList)# testing set photos folder
file.remove(trainn);list.files()#117 files not 390, correct

setwd('../');setwd('../') #PhotosAnalysis folder is up two folders


##########################################################################################
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html

install.packages('magick')

library(magick)

im1 <- image_read('./train/proPhotos/388_Janis Corona©KaoriSuzuki.jpg')
im2 <- image_scale(im1,"28x28")#scales to height 28", 19x28
im2 <- image_scale(im1,"28")#scales to width "28", 28X42
im3 <- image_resize(im1,"28x28")#19x28

print(im2)

# I originally wanted to resize the images inside each folder of train and test, but found
# this instead, and uploaded into separate repositories for photos only in github, to run this
# function

#https://www.ben-johnston.co.uk/bulk-resizing-images-with-r/
imageResizeR <- function(x, y, z, a){
  require(magick)
  for(i in seq_along(x)){
    download.file(x[i], y[i], mode = "wb")
  }
  listOfFiles <- list.files(path = z,
                            full.names = TRUE)
  imageResizing <- for(i in seq_along(listOfFiles)){
    imFile <- image_read(listOfFiles[i])
    resized <- image_scale(imFile, a)
    image_write(resized,
                paste(listOfFiles[i]))
  }
}

#for above function pulled from the web, it needs the directory location
#to grab the images, the name to save as, the directory location to store the images, 
# dimensions to resize all in one csv file for each of training and testing files

#merge the trainn and Photos data tables
trainPhotos <- merge(trainn, Photos, by.x='train', by.y='dressAttire')#273X2

#merge the testn and Photos data tables
testPhotos <- merge(testn, Photos, by.x='test', by.y='dressAttire')#117X2

#get directory of the train photos in trList make a 273 long vector of path
ifelse(!file.exists('testResized'), dir.create('testResized'), testResized <- './testResized')
ifelse(!file.exists('trainResized'), dir.create('trainResized'), trainResized <- './trainResized')

trList <- 'C:/Users/m/Desktop/ML in R and Python/PhotosAnalysis/train/proPhotos'
teList <- 'C:/Users/m/Desktop/ML in R and Python/PhotosAnalysis/test/proPhotos'

#the files of images have to be downloaded from a 'url' not file path

# trList <- 'https://github.com/JanJanJan2018/trainCNN'
# teList <- 'https://github.com/JanJanJan2018/testCNN'

# the x part of function
trainPath <- as.data.frame(rep(trList,273))#273X1
colnames(trainPath) <- 'trainPath'

testPath <- as.data.frame(rep(teList, 117))#117X1
colnames(testPath) <- 'testPath'

# the Z part of function, is declared in the function not in the csv file unless you want to
trainPathTo <- as.data.frame(rep('./trainResized',273))
colnames(trainPathTo) <- 'trainPathTo'

testPathTo <- as.data.frame(rep('./testResized', 117))
colnames(testPathTo) <- 'testPathTo'

# the 'a' part of the function is the dimensions 'wrapped in inverted commas <tilde key>
# and end in an exclamation ie. ``28x28!``

# for the 'y' part of the function the new file name to save as is needed

path1 <- 'C:/Users/m/Desktop/ML in R and Python/PhotosAnalysis/trainResized/'
g <- as.character(trainPhotos$train)#273
gg <- as.vector(gsub('Janis Corona©KaoriSuzuki.jpg',"Resized.jpg", g))
gh <- paste(path1,gg,sep='')

newTrainName <- as.data.frame(gh)
colnames(newTrainName) <- 'newTrainName'

path2 <- 'C:/Users/m/Desktop/ML in R and Python/PhotosAnalysis/testResized/'
h <- as.character(testPhotos$test)#117
hh <- as.vector(gsub('Janis Corona©KaoriSuzuki.jpg',"Resized.jpg", h))
hi <- paste(path2, hh, sep='')

newTestName <- as.data.frame(hi)
colnames(newTestName) <- 'newTestName'

trainResizeDF <- cbind(trainPath,newTrainName,trainPhotos)#273X4
testResizeDF <- cbind(testPath, newTestName, testPhotos)#117X4


jkTrain <- as.vector(trainResizeDF$trainPath)
jkTrain2 <- as.vector(trainResizeDF$train)
trainPath <- paste(jkTrain, jkTrain2, sep='/')

jkTest <- as.vector(testResizeDF$testPath)
jkTest2 <- as.vector(testResizeDF$test)
testPath <- paste(jkTest, jkTest2, sep='/')

trainResizeDF <- cbind(trainPath,newTrainName,trainPhotos[,2])#273X3
colnames(trainResizeDF)[3] <- 'type'
testResizeDF <- cbind(testPath, newTestName, testPhotos[,2])#117X3
colnames(testResizeDF)[3] <- 'type'

#make sure in parent folder PhotosAnalysis for this one
write.csv(trainResizeDF,'trainingMeta.csv', row.names=FALSE)
write.csv(testResizeDF, 'testingMeta.csv', row.names=FALSE)

#still have to change to the directory you want the files to go trainResized
#error 429 too many requests, and the files are not supported as a format in windows jpg

# imageResizeR(as.character(trainResizeDF$trainPath[1:60]), as.character(trainResizeDF$newTrainName),
#              './trainResized', "28x42!")
# imageResizeR(as.character(trainResizeDF$trainPath[61:120]), as.character(trainResizeDF$newTrainName),
#              './trainResized', "28x42!")
# imageResizeR(as.character(trainResizeDF$trainPath[121:180]), as.character(trainResizeDF$newTrainName),
#              './trainResized', "28x42!")


# modify the function for file.copy from to 
ImageResizeR <- function(x, y, z, a){
  require(magick)
  for(i in seq_along(x)){
    file.copy(x[i], y)
  }
  listOfFiles <- list.files(path = z,
                            full.names = TRUE)
  imageResizing <- for(i in seq_along(listOfFiles)){
    imFile <- image_read(listOfFiles[i])
    resized <- image_scale(imFile, a)
    image_write(resized,
                paste(listOfFiles[i]))
  }
}

# z is './trainResized' or './testResized'
# a is "28X42!"
trainPath <- './trainResized'
ImageResizeR(trainResizeDF$trainPath, trainPath, './trainResized',
             '28x42!')

testPath <- './testResized'
ImageResizeR(testResizeDF$testPath, testPath, './testResized',
             '28x42!')

