# #menerjemahkan synop
# 
# rm (list = ls())
# 
# enam.atau.bukan <- function(x) {
#    if (substr(x,1,1) == '6')
#       return(TRUE)
#    else
#       return(FALSE)
# }
# 
# index.enam <- function(x) {
#    retval <- vector(length = length(x))
#    for (a in 1:length(x)) {
#       retval[a] <- ifelse(test = substr(x[a],1,1) == '6',yes = TRUE, no = FALSE) #enam.atau.bukan(x[a])
#    }
#    return(retval)
# }
# 
# 
# setwd('D:/thesis/data/synop mentah/')
# 
# nama.file.synop <- 'station.96015.2016.03.txt'
# 
# isi.file <- read.csv2(
#    file = nama.file.synop,
#    header = FALSE,
#    sep = "\t",
#    fill = TRUE,
#    colClasses = 'character'
# )
# 
# isi.file <- isi.file[(isi.file[,1] != 'hastu'),]
# isi.file <- isi.file[(isi.file[,2] == 'AAXX'),]
# isi.file <- unique(isi.file)
# enam <- t(apply(isi.file, 1, index.enam))
# 
# data.synop <- as.character(as.Date(as.character(isi.file[,1])) )
# yyggi <- as.character(isi.file[,3])
# yyggi <- substr(yyggi,3,4)
# data.synop <- data.frame(data.synop,yyggi)

# ------------------------------------------------------------------------------------------------------
# rm(list = ls())
# 
# index.enam <- function(x) {
#    retval <- vector(length = length(x))
#    for (a in 1:length(x)) {
#       retval[a] <- ifelse(test = substr(x[a],1,1) == '6',yes = TRUE, no = FALSE) #enam.atau.bukan(x[a])
#    }
#    return(retval)
# }
# 
# index.tiga <- function(x) {
#    return(which(x == '333'))
# }
# 
# ambil.hujan.harian <- function(x) {
#    retval <- NA
#    if(substr(x,5,5) == '4') {
#       angka <- as.numeric(substr(x,2,4))
#       if(angka < 990) {
#          retval <- angka
#       }
#       else {
#          if(angka > 990) {
#             retval <- (angka - 990) / 10
#          }  
#       }
#    }  
#    return(retval)
# }
# 
# setwd('D:/thesis/data/synop mentah/')
# 
# nama.file.synop <- 'station.96015.2016.03.txt'
# 
# data.synop <- list()
# 
# data.synop$isi.file <- read.csv2(
#    file = nama.file.synop,
#    header = FALSE,
#    sep = "\t",
#    fill = TRUE,
#    colClasses = 'character'
# )
# 
# data.synop$isi.file <- data.synop$isi.file[(data.synop$isi.file[,1] != 'hastu'),]
# data.synop$isi.file <- data.synop$isi.file[(data.synop$isi.file[,2] == 'AAXX'),]
# data.synop$isi.file <- unique(data.synop$isi.file)
# 
# data.synop$letak.enam <- t(apply(data.synop$isi.file,1,index.enam))
# data.synop$letak.tiga <- apply(data.synop$isi.file,1,index.tiga)
# data.synop$tanggal <- data.synop$isi.file[,1]
# data.synop$jam <- as.numeric(
#    substr(
#       data.synop$isi.file[,3],3,4
#    )
# )
# data.synop$ch24 <- rep(NA,length(data.synop$jam))
# 
# ----------------------------------------------------------------------------------------------

rm(list = ls())

library(stringr)
library(lubridate)

setwd('D:/thesis/data/synop mentah/')

nama.file <- 'aaxx2014.csv'

isi.file <- read.csv2(
   file = nama.file,
   header = FALSE,
   colClasses = 'character',sep = ';'
)

parsing <- function(input) {
   if(!is.na(input)) {
      data.synop <- strsplit(input,' ')
      letak.333 <- match('333',data.synop[[1]])
      if(!is.na(letak.333)) {
         letak.6 <- data.synop[[1]][3:letak.333]
         letak.6 <- grep(
            pattern = "^6[0-9]{3}4$",x = letak.6,value = TRUE
         )
         if(length(letak.6) > 1)
            letak.6 <- letak.6[1]
         if(length(letak.6) == 0)
            letak.6 <- NA
         return(letak.6)
      }
      else
         return(NA)
   }
   else
      return(NA)
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

# year.in.data <- format(as.Date(data.duplikat[1,2]),"%Y")
# 
# complete.date <- seq.Date(as.Date(paste(year.in.data,'01','01',sep = '-'),format = "%Y-%m-%d"),
#                           as.Date(paste(year.in.data,'12','31',sep = '-'),format = "%Y-%m-%d"),
#                           by = "day")
# complete.station <- levels(as.factor(data.duplikat[,1]))
# 
# complete.data <- expand.grid(complete.date,complete.station)
# 
# complete.data <- cbind(complete.data,NA)
# 
# date.station.duplikat <- paste(data.duplikat[,2],data.duplikat[,1])
# date.station.complete <- paste(complete.data[,1],complete.data[,2])
# 
# extended.date.station <- date.station.complete %in% date.station.duplikat
# which.true <- which(extended.date.station == TRUE)
# 
# 

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

# complete.id <- as.character(complete.id)

colnames(complete.id) <- c('id','date','station','ch')

status.data <- complete.id[,'id'] %in% data.ready[,'id']

status.data.2 <- data.ready[,'id'] %in% complete.id[,'id']

complete.id.exist <- complete.id$id[which(status.data == TRUE)]

# complete.id[status.data,'ch'] <- data.ready[,'ch']

for (a in 1:length(complete.id[,'ch'])) {
   print(paste(a,'from',length(complete.id[,'ch'])))
   n <- which(complete.id[a,'id'] == data.ready[,'id'])
   if(length(n) == 1) {
      complete.id$ch[a] <- data.ready[n,'ch']
   }
}


# for (a in 1:length(complete.id.exist)) {
#    if(complete.id.exist[a] != data.ready[a,1])
#    {
#       retval <- a
#       break
#    }
# }

# complete.id$ch[which(status.data == TRUE)] <- data.ready$ch


