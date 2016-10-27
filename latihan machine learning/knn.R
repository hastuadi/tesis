rm(list = ls())
wbcd <- read.csv('d:/thesis/r/latihan machine learning/wdbc.data', 
                 sep = ',', header = FALSE)

measurement <- c(
   'radius','texture','perimeter','area','smoothness','compactness',
   'concavity','concave points','symmetry','fractal dimension'
)

type.meas <- c('mean','std.err','worst')

exp.gr <- expand.grid(measurement,type.meas,stringsAsFactors = FALSE)
exp.gr <- paste(exp.gr[,1],exp.gr[,2])

colnames(wbcd) <- c('id','diagnosis',exp.gr)

wbcd$diagnosis <- factor(wbcd$diagnosis,levels = c('B','M'),
                         labels = c('Benign','Malignant'))

round(
   prop.table(
      table(wbcd$diagnosis)
   ) * 100, 
   digits = 2
)

normalize <- function(x) {
   return((x - min(x)) / (max(x) - min(x)))
}

wbcd.in <- lapply(wbcd[-c(1,2)], normalize)
wbcd.in <- as.data.frame(wbcd.in)
wbcd.labels <- wbcd[2]

train.data <- wbcd.in[1:469,]
train.labels <-as.factor(wbcd.labels[1:469,])
test.data <- wbcd.in[470:569,]
test.label <-as.factor(wbcd.labels[470:569,])

library(class)

knn.res.normalization <- knn(
   train = train.data, test = test.data,
   cl = train.labels, k = 21
)

library(gmodels)

ct.norm <- CrossTable(test.label,knn.res.normalization, prop.chisq = FALSE)

train.data.z <- scale(wbcd[,3:ncol(wbcd)])
summary(train.data.z[,'radius mean'])

test.data.z <- train.data.z[470:569,]
train.data.z <- train.data.z[1:469,]


knn.res.standardize <- knn(
   train = train.data.z, test = test.data.z,
   cl = train.labels, k = 21
)

ct.stand <- CrossTable(
   test.label,knn.res.standardize, prop.chisq = FALSE
)