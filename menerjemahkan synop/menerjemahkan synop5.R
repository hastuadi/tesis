# --------
# mengekstrak data synop keseluruhan

rm(list = ls())

library(stringr)
library(lubridate)
library(plyr)

setwd('D:/thesis/data/synop mentah 2002-2009 dan 2010-2016/')

nama.file <- 'aaxx 2002 2009.csv'

isi.file <- read.csv2(
   file = nama.file,
   header = FALSE,
   colClasses = 'character',sep = ';'
)

parsing <- function(input) {
   retval <- cbind(
      input[,1:5],
      gsub('\t',' ',input[,6])
   )
   colnames(retval) <- c(paste('col',1:ncol(retval)))
   return(retval)
}

ambil.data.yg.dibutuhkan <- function(input) {
   retval <- cbind(
      input[,2],input[,3],input[,4],NA,NA,NA
   )
   
   synop <- strsplit(as.character(input[,6]),' ')
   
   letak333 <- sapply(synop, match, '333')
   letak333 <- sapply(letak333, function(x) { 
         if(length(which(x == 1)) > 1)
            return(which(x == 1)[1])
         if(length(which(x == 1)) == 1)
            return(which(x == 1))
         if(length(which(x == 1)) < 1)
            return(NA)
            
      }
   )

   for (a in 1:nrow(input)) {
      # print(a)
      if(input[a,4] == 0 && !is.na(letak333[a]) && !is.na(length(synop[[a]])))
      {
         letak6 <- grep(
            pattern = "^6[0-9]{3}4$",x = synop[[a]][1:(letak333[a])],value = TRUE
         )
         if(length(letak6) > 1)
            letak6 <- letak6[1]
         if(length(letak6) == 0)
            letak6 <- NA

         letak2 <- grep(
            pattern = "^2[0-9]{1}",
            x = synop[[a]][(letak333[a]:length(synop[[a]]))],
            value = TRUE
         )
         if( length(letak2) > 1){
            letak2 <- letak2[1]
         }
         if (length(letak2) == 0) {
            letak2 <- NA
         }
         if (!is.na(letak6))
            retval[a,4] <- letak6
         if(!is.na(letak2))
            retval[a,6] <- letak2
      }
      if (input[a,4] == 12 && !is.na(letak333[a]) && !is.na(length(synop[[a]]))) {
         letak1 <- grep(
            pattern = "^1[0-9]{1}",
            x = synop[[a]][(letak333[a]:length(synop[[a]]))],
            value = TRUE
         )
         if (length(letak1) > 1) {
            letak1 <- letak1[1]
         }
         if (length(letak1) == 0) {
            letak1 <- NA
         }
         if (!is.na(letak1)) {
            retval[a,5] <- letak1
         }
      }
   }

   jam0 <- retval[which(retval[,3] == 0),]
   jam12 <- retval[which(retval[,3] == 12),]

   jam0 <- as.data.frame(matrix(jam0,nrow = nrow(jam0)))
   jam12 <- as.data.frame(matrix(jam12,nrow = nrow(jam12)))

   retval.2 <- join(jam0,jam12,type = 'left', by = c('V1','V2'))[,c(1,2,4,6,9)]
   return(retval.2)
}

valid.333 <- function(input) {
   retval <- as.character(input[,6])
   for (a in 1:length(retval)) {
      jumlah333 <- match('333',strsplit(retval[a],split = ' '))
      print(jumlah333)
   }
   return(retval)
}

data.copy <- parsing(isi.file)
hasil <- ambil.data.yg.dibutuhkan(input = data.copy[1:nrow(data.copy),])

write.csv(
   x = hasil, 
   file = 'data 2002 2009 belum diolah.txt',
   sep = ' ',
   na = 'NA',
   row.names = FALSE
)

# ---- 
# menerjemahkan data curah hujan dari synop mentah yang sudah terekstrak

rm(list = ls())
setwd('D:/thesis/data/synop mentah 2002-2009 dan 2010-2016/')
nama.file <- 'data 2002 2009 belum diolah.txt'
nama.file[2] <- 'data 2010 2016 belum diolah.txt'

baca.data <- function(input) {
   # print(baris)
   input <- as.vector(input)
   ch <- NA
   tmin <- NA
   tmax <- NA
   if(!is.na(input[3]) &&
      substr(input[3],1,1) == '6' &&
      substr(input[3],5,5) == '4' &&
      substr(input[3],2,4) != '///') {
      ch <- as.numeric(substr(input[3],2,4))
      if(!is.na(ch)) {
         if(ch == 990) { ch <- NA}
         if(ch > 990) { ch <- (ch - 990) / 10 }
      }
      
   }
   if(!is.na(input[4]) &&
      substr(input[4],1,1) == '2' &&
      substr(input[4],3,5) != '///') {
      tmin <- as.numeric(substr(input[4],3,5))
      tmin <- tmin / 10
   }
   
   if(!is.na(input[5]) &&
      substr(input[5],1,1) == '1' &&
      substr(input[5],3,5) != '///') {
      tmax <- as.numeric(substr(input[5],3,5))
      tmax <- tmax / 10
   }
   
   retval <- c(
      input[1:2],
      as.character(c(ch, tmin, tmax))
   )
   

   return(retval)
}


for (jf in 1:1) { #length(nama.file)) {
   isi.file <- read.csv2(
      file = nama.file[jf],
      header = TRUE, sep = ',', na.strings = 'NA'
   )
   
   isi.file <- as.matrix(isi.file,nrow = nrow(isi.file))
   
   ch <- rep(NA,nrow(isi.file))
   
   for (a in 1:length(ch)) {
      if(!is.na(isi.file[a,3])) {
         if(substr(isi.file[a,3],1,1) == '6' &&
            substr(isi.file[a,3],5,5) == '4' &&
            substr(isi.file[a,3],2,4) != '///') {
            ch[a] <- as.numeric(substr(isi.file[a,3],2,4))
         }
      }
   }
   
   for (a in 1:length(ch)) {
      if(!is.na(ch[a])) {
         if(ch[a] == 990)  { ch[a] <- NA }
         else if(ch[a] > 990) { ch[a] <- (ch[a] - 990) / 10 }
      }
   }
   
   tmin <- rep(NA, nrow(isi.file))
   
   for (a in 1:length(tmin)) {
      if(!is.na(isi.file[a,4])) {
         if(substr(isi.file[a,4],1,1) == '2'  && 
            substr(isi.file[a,4],3,5) != '///')
         {
            tmin[a] <- as.numeric(substr(isi.file[a,4],3,5)) / 10
         }
      }
   }
   
   tmax <- rep(NA, nrow(isi.file))
   
   for (a in 1:length(tmax)) {
      if(!is.na(isi.file[a,5])) {
         if(substr(isi.file[a,5],1,1) == '1'  && 
            substr(isi.file[a,5],3,5) != '///')
         {
            tmax[a] <- as.numeric(substr(isi.file[a,5],3,5)) / 10
         }
      }
   }
   
   ch <- as.character(ch)
   tmin <- as.character(tmin)
   tmax <- as.character(tmax)
   file.disimpan <- cbind(isi.file[,1:2],ch,tmin,tmax)
   # rm(ch, tmin, tmax)
   
   nama.file.simpan <- paste('data_synop_diterjemahkan',jf,'.txt',sep = '')
   
   write.csv2(
      x = file.disimpan, file = nama.file.simpan,
      sep = ' ',row.names = FALSE,col.names = FALSE,
      na = 'NA'
   )
}
