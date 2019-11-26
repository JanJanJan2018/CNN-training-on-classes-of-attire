# CNN-training-on-3-classes-of-attire
uses the photos in the trainCNN and testCNN repositories of this account FOR ORIGINALS
AND FOR THE resized images of 28x28 pixels or 224X224 pixels use the testCNN_resized28 and the trainCNN_resized28
and the testCNN_resized224 and the trainCNN_resized224.There is an R script to access the resizing commands with magick R package 


for(i in seq_along(x)){
    download.file(x[i], y[i], mode = "wb")
  }
where x is the vector of file names and x is the vector of new file names to place, there is a limit beyond 60 downloads. The script, SelectPhotosTrainingTestingSetsAutomation.R, in this folder has this function pulled from web with source, but didn't use because of the 429 too many request error.

The python code is in progress, taken from the microcourse on deep learning from Kaggle, it uses python 3, and the image sizes need resized. 

The program froze on the big image sizes or non-resized images when running the code to practice. 
