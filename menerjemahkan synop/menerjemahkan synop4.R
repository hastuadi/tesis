# rm(list = ls())
# 
# library(stringr)
# library(lubridate)
# 
# setwd('D:/thesis/data/synop mentah/')
# 
# nama.file <- 'aaxx2014.csv'
# 
# isi.file <- read.csv2(
#    file = nama.file,
#    header = FALSE,
#    colClasses = 'character',sep = ';'
# )
# 
# parsing <- function(input) {
#    if(!is.na(input)) {
#       data.synop <- strsplit(input,' ')
#       letak.333 <- match('333',data.synop[[1]])
#       if(!is.na(letak.333)) {
#          letak.6 <- data.synop[[1]][3:letak.333]
#          letak.6 <- grep(
#             pattern = "^6[0-9]{3}4$",x = letak.6,value = TRUE
#          )
#          if(length(letak.6) > 1)
#             letak.6 <- letak.6[1]
#          if(length(letak.6) == 0)
#             letak.6 <- NA
#          return(letak.6)
#       }
#       else
#          return(NA)
#    }
#    else
#       return(NA)
# }
# 
# code.to.precip.val <- function(input) {
#    if(!is.na(input)) {
#       retval <- as.numeric(substr(as.character(input),2,4))
#       if(retval >= 991) {
#          return((retval - 990) / 10)
#       }
#       if(retval == 990)
#          return(NA)
#       return(retval)
#    }
#    else return(NA)
# }
# 
# data.duplikat <- isi.file
# 
# data.duplikat[,6] <- gsub('\t',' ',data.duplikat[,6])
# data.duplikat <- data.duplikat[,-c(1,5)]
# 
# ch <- vector(length = dim(data.duplikat)[1])
# 
# for (a in 1:dim(data.duplikat)[1]) {
#    ch[a] <- parsing(data.duplikat[a,4])
# }
# 
# for (a in 1:length(ch)) {
#    ch[a] <- code.to.precip.val(input = ch[a])
# }
# 
# data.ready <- cbind(
#    as.character(data.duplikat[,2]),
#    as.character(data.duplikat[,1]),
#    ch
# )
# 
# rm(ch,data.duplikat,isi.file,a,nama.file)
# 
# data.ready <- cbind(
#    paste(data.ready[,1],data.ready[,2]),
#    data.ready
# )
# 
# colnames(data.ready) <- c('id','date','station','ch')
# 
# 
# year.in <- year(data.ready[,'date'])[1]
# 
# complete.id <- as.character(
#    seq.Date(
#       from = as.Date(paste(year.in,'01','01',sep = '-'),format = '%Y-%m-%d'),
#       to = as.Date(paste(year.in,'12','31',sep = '-'),format = '%Y-%m-%d'),
#       by = 'day'
#    )
# )
# 
# station.level <- as.character(levels(as.factor(data.ready[,3])))
# 
# complete.id <- expand.grid(complete.id,station.level)
# complete.id[,1] <- as.character(complete.id[,1])
# complete.id[,2] <- as.character(complete.id[,2])
# 
# complete.id <- cbind(
#    as.character(paste(complete.id[,1],complete.id[,2])),
#    complete.id,NA
# )
# 
# complete.id[,1] <- as.character(complete.id[,1])
# complete.id[,2] <- as.character(complete.id[,2])
# complete.id[,3] <- as.character(complete.id[,3])
# complete.id[,4] <- as.character(complete.id[,4])
# 
# colnames(complete.id) <- c('id','date','station','ch')
# 
# for (a in 1:length(complete.id[,'ch'])) {
#    print(paste(a,'from',length(complete.id[,'ch'])))
#    n <- which(complete.id[a,'id'] == data.ready[,'id'])
#    if(length(n) == 1) {
#       complete.id$ch[a] <- data.ready[n,'ch']
#    }
# }

# ------------------------------------------------------------------------------------------

rm(list = ls())

library(stringr)
library(lubridate)

setwd('D:/thesis/data/synop mentah 2003-2009 dan 2010-2016/')

nama.file <- 'aaxx 2003 2009.csv'

isi.file <- read.csv2(
   file = nama.file,
   header = FALSE,
   colClasses = 'character',sep = ';'
)

parsing <- function(input) {
   # if(!is.na(input)) {
   #    data.synop <- strsplit(input,' ')
   #    letak.333 <- match('333',data.synop[[1]])
   #    if(!is.na(letak.333)) {
   #       letak.6 <- data.synop[[1]][3:letak.333]
   #       letak.6 <- grep(
   #          pattern = "^6[0-9]{3}4$",x = letak.6,value = TRUE
   #       )
   #       if(length(letak.6) > 1)
   #          letak.6 <- letak.6[1]
   #       if(length(letak.6) == 0)
   #          letak.6 <- NA
   #       return(letak.6)
   #    }
   #    else
   #       return(NA)
   # }
   # else
   #    return(NA)
   
}

code.to.precip.val <- function(input) {
   if(!is.na(input)) {
      retval <- as.numeric(substr(as.character(input),2,4))
      if(retval >= 991) {
         return((retval - 990) / 10)
      }
      if(retval == 990)
         return(NA)
      return(retval)
   }
   else return(NA)
}

data.duplikat <- isi.file

data.duplikat[,6] <- gsub('\t',' ',data.duplikat[,6])
data.duplikat <- data.duplikat[,-c(1,5)]

ch <- vector(length = dim(data.duplikat)[1])

for (a in 1:dim(data.duplikat)[1]) {
   ch[a] <- parsing(data.duplikat[a,4])
}

for (a in 1:length(ch)) {
   ch[a] <- code.to.precip.val(input = ch[a])
}

data.ready <- cbind(
   as.character(data.duplikat[,2]),
   as.character(data.duplikat[,1]),
   ch
)

rm(ch,data.duplikat,isi.file,a,nama.file)

data.ready <- cbind(
   paste(data.ready[,1],data.ready[,2]),
   data.ready
)

colnames(data.ready) <- c('id','date','station','ch')


year.in <- year(data.ready[,'date'])[1]

complete.id <- as.character(
   seq.Date(
      from = as.Date(paste(year.in,'01','01',sep = '-'),format = '%Y-%m-%d'),
      to = as.Date(paste(year.in,'12','31',sep = '-'),format = '%Y-%m-%d'),
      by = 'day'
   )
)

station.level <- as.character(levels(as.factor(data.ready[,3])))

complete.id <- expand.grid(complete.id,station.level)
complete.id[,1] <- as.character(complete.id[,1])
complete.id[,2] <- as.character(complete.id[,2])

complete.id <- cbind(
   as.character(paste(complete.id[,1],complete.id[,2])),
   complete.id,NA
)

complete.id[,1] <- as.character(complete.id[,1])
complete.id[,2] <- as.character(complete.id[,2])
complete.id[,3] <- as.character(complete.id[,3])
complete.id[,4] <- as.character(complete.id[,4])

colnames(complete.id) <- c('id','date','station','ch')

complete.in.ready <- complete.id$id %in% data.ready[,'id']
ready.in.complete <- unique(data.ready[,'id']) %in% complete.id$id

complete.id$ch[which(complete.in.ready == TRUE)] <-
   data.ready[which(ready.in.complete == TRUE),'ch']

# for (a in 1:length(complete.id[,'ch'])) {
#    print(paste(a,'from',length(complete.id[,'ch'])))
#    n <- which(complete.id[a,'id'] == data.ready[,'id'])
#    if(length(n) == 1) {
#       complete.id$ch[a] <- data.ready[n,'ch']
#    }
# }