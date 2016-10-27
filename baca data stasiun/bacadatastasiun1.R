rm(list = ls())
setwd('D:/thesis/station index/')
nama.file <- 'stationindex.csv'
file.stasiun <- read.csv(
   file = nama.file, sep = ','
)
awal <- as.numeric(
   substr(
      as.character(
         file.stasiun$Latitude
      ),1,2
   )
)
menit <- as.numeric(
   substr(
      as.character(
         file.stasiun$Latitude
      ),4,5
   )
) / 60

indeks.detik <- which(nchar(as.character(file.stasiun$Latitude)) == 9)
detik <- rep(0,length(file.stasiun$Latitude))
detik[indeks.detik] <-
   as.numeric(
      substr(
         as.character(file.stasiun$Latitude[indeks.detik])
         ,7,8)
   ) / 3600
indeks.selatan <- which(
   substr(
      as.character(file.stasiun$Latitude),
      nchar(as.character(file.stasiun$Latitude)),
      nchar(as.character(file.stasiun$Latitude))
   ) == 'S'
)
data.latitude.desimal <- awal+menit+detik
data.latitude.desimal[indeks.selatan] <- data.latitude.desimal[indeks.selatan] * (-1)

awal <- as.numeric(substr(as.character(file.stasiun$Longitude),1,3))
menit <- as.numeric(substr(as.character(file.stasiun$Longitude),5,6)) / 60
indeks.detik <- which(nchar(as.character(file.stasiun$Longitude)) == 10)
detik <- rep(0,length(file.stasiun$Longitude))
detik[indeks.detik] <- as.numeric(substr(as.character(file.stasiun$Longitude[indeks.detik]),8,9)) / 3600
data.longitude.desimal <- awal + menit + detik

file.stasiun$Latitude.desimal <- data.latitude.desimal
file.stasiun$Longitude.desimal <- data.longitude.desimal

hapus <- which(as.character(file.stasiun$WMO.INDEX) == '-----')
file.stasiun <- file.stasiun[-hapus,]
hapus <- which(as.character(file.stasiun$Notes) != '----')
hapus <- matrix(data = hapus, ncol = 2, byrow = TRUE)
file.stasiun <- file.stasiun[-hapus[,1],]
file.stasiun <- file.stasiun[,-c(2,4,5,6,8)]
write.csv(x = file.stasiun,file = 'filestasiundiolah.csv', row.names = FALSE)

library(rworldmap)
newmap <- getMap(resolution = 'high')

plot(
   newmap,
   xlim = c(min(file.stasiun$Longitude.desimal),
            max(file.stasiun$Longitude.desimal)),
   ylim = c(min(file.stasiun$Latitude.desimal),
            max(file.stasiun$Latitude.desimal)),
   asp = 1, main = 'Data BMKG & CHIRPS', cex.main = 2
)

points(file.stasiun$Longitude.desimal, file.stasiun$Latitude.desimal, col = 'red', cex = 1.25, pch = 19)
points(file.stasiun$Longitude.desimal, file.stasiun$Latitude.desimal, col = 'black', cex = 1.25, pch = 1)

library(RNetCDF)

file.netcdf <- open.nc(con = '../data chirps/chirps-v2.0.2016.days_p25_cdf.nc')

dim.lon <- dim.inq.nc(file.netcdf,'lon')
dim.lat <- dim.inq.nc(file.netcdf,'lat')
dim.t <- dim.inq.nc(file.netcdf,'time')

var.lon <- var.get.nc(file.netcdf,'lon')
var.lat <- var.get.nc(file.netcdf,'lat')
var.time <- var.get.nc(file.netcdf,'time')

# var.lon <- var.lon - min(var.lon)
low.lon <- length(which(var.lon <= min(file.stasiun$Longitude.desimal)))
high.lon <- which(var.lon >= max(file.stasiun$Longitude.desimal))[1]
lons <- var.lon[c(low.lon:high.lon)]
start.index.lon <- which(var.lon == lons[1])

low.lat <- length(which(var.lat <= min(file.stasiun$Latitude.desimal)))
high.lat <- which(var.lat >= max(file.stasiun$Latitude.desimal))[1]
lats <- var.lat[c(low.lat:high.lat)]
start.index.lat <- which(var.lat == lats[1])

var.data <- var.get.nc(
   ncfile = file.netcdf, variable = 'hujan',
   start = c(start.index.lon,start.index.lat,1),
   count = c(length(lons),length(lats),1)
)

var.to.write <- var.data
var.to.write[!is.na(var.to.write)] <- 1
var.to.write[is.na(var.to.write)] <- 0

write.csv(x = var.to.write,file = 'datahujanindonesia.csv')

# var.data.vector <- t(var.data[rev(1:dim(var.data)[1]),rev(1:dim(var.data)[2])])
var.data.vector <- t(var.data)
var.data.vector <- matrix(var.data.vector,ncol = 1)
list.lat.lon <- expand.grid(lats,lons)

indeks.var.data.vector.not.NA <- which(!is.na(var.data.vector))

points(
   x = list.lat.lon[indeks.var.data.vector.not.NA,2],
   y = list.lat.lon[indeks.var.data.vector.not.NA,1],
   pch = 19, col = 'blue', cex = 0.25
)

legend(
   x = min(file.stasiun$Longitude.desimal), y = min(file.stasiun$Latitude.desimal) + 2,legend = c('BMKG','CHIRPS'),
   cex = .85, pch = 19, col = c('red','blue')
)

#---------------ubah format 
nama.direktori <- 'D:/thesis/data/edit/data_bmkg/data2013/'
nama.file.synop <- paste(nama.direktori,'new_attachment_synop_96075_2013-01-01_2013-12-31.txt',sep = '')

handle.file <- file(nama.file.synop,'r')
baris <- readLines(handle.file)

# text(x = file.stasiun$Longitude.desimal + 0.5, y = file.stasiun$Latitude.desimal,
#      labels = c(1:length(file.stasiun$Longitude.desimal)), cex = 1)
# ubah.format.lat.lon <- function(text.input) {
#    retval <- ''
#    awal <- as.numeric(substr(text.input,1,2))
#    menit <- 0
#    detik <- 0
#    if(nchar(text.input) == 6) {
#       menit <- as.numeric(substr(text.input,4,5)) / 60
#       if(substr(text.input,nchar(text.input),nchar(text.input)) == 'S')
#          retval <- -(awal + menit)
#       else
#          retval <- awal + menit
#    }
#    if(nchar(text.input) == 9) {
#       menit <- as.numeric(substr(text.input,4,5)) / 60
#       detik <- as.numeric(substr(text.input,7,8)) / 3600
#       if(substr(text.input,nchar(text.input),nchar(text.input)) == 'S')
#          retval <- -(awal + menit + detik)
#       else
#          retval <- awal + menit + detik
#    }
#    if(nchar(text.input) == 10) {
#       awal <- as.numeric(substr(text.input,1,3))
#       menit <- as.numeric(substr(text.input,5,6)) / 60
#       detik <- as.numeric(substr(text.input,8,9)) / 3600
#       if(substr(text.input,nchar(text.input),nchar(text.input)) == 'E')
#          retval <- (awal + menit + detik)
#       else
#          retval <- -(awal + menit + detik)
#    }
#    return(retval)
# }
# 
# data.latitude <- ubah.format.lat.lon(text.input = file.stasiun$Latitude[2])
