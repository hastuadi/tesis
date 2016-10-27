# ----
rm(list = ls())

setwd('D:/thesis/')

# ----
# mendapatkan data lokasi stasiun pengamatan BMKG
# untuk koordinat file stasiun yang belum diolah

nama.file.stasiun <- 'pekerjaan 1/stasiun bmkg.csv'
isi.file.stasiun <- read.csv(
   file = nama.file.stasiun, sep = ','
)

isi.file.stasiun <- isi.file.stasiun[,-c(2)]
lat.tmp <- as.numeric(
   substr(isi.file.stasiun$Latitude,4,5)
) / 60 
lat.tmp <- lat.tmp + as.numeric(
   substr(isi.file.stasiun$Latitude,1,2)
)

minlat <- which(substr(isi.file.stasiun$Latitude,6,6) == 'S')

lat.tmp[minlat] <- -lat.tmp[minlat]

lon.tmp <- as.numeric(
   substr(isi.file.stasiun$Longitude,5,6)
) / 60

lon.tmp <- lon.tmp + as.numeric(substr(isi.file.stasiun$Longitude,1,3))

library(rworldmap)
library(rworldxtra)

library(rworldmap)
newmap <- getMap(resolution = 'high')

plot(
   newmap,
   xlim = c(min(lon.tmp),
            max(lon.tmp)),
   ylim = c(min(lat.tmp),
            max(lat.tmp)),
   asp = 1, main = 'Data BMKG & CHIRPS', cex.main = 2
)

points(lon.tmp, 
       lat.tmp, col = 'red', cex = 1.25, pch = 19)
points(lon.tmp, lat.tmp, 
       col = 'black', cex = 1.25, pch = 1)

# ----
# mendapatkan data lokasi stasiun pengamatan BMKG
# untuk koordinat file stasiun yang sudah diolah

nama.file.stasiun <- 'pekerjaan 1/filestasiundiolah.csv'
isi.file.stasiun <- read.csv(
   file = nama.file.stasiun, sep = ','
)

newmap <- getMap(resolution = 'high')

plot(
   newmap, xlim = c(min(isi.file.stasiun$Longitude.desimal), 
                    max(isi.file.stasiun$Longitude.desimal)),
   ylim = c(min(isi.file.stasiun$Latitude.desimal),
            max(isi.file.stasiun$Latitude.desimal)),
   asp = 1
)

points(x = isi.file.stasiun$Longitude.desimal,
       y = isi.file.stasiun$Latitude.desimal, col = 'red', pch = 19)
points(x = isi.file.stasiun$Longitude.desimal,
       y = isi.file.stasiun$Latitude.desimal, col = 'black', pch = 1, cex = 1.1)

rm(list = ls())
setwd('D:/thesis/')
# ----
# mendapatkan data di dalam file chirps yang sudah dikonvert

library(RNetCDF)

nama.file.chirps <- 'data/master/data chirps/p05/chirps2008subset2.nc'
file.netcdf <- open.nc(con = nama.file.chirps)
keterangan.file.netcdf <- file.inq.nc(file.netcdf)
dim.lon <- dim.inq.nc(file.netcdf,'lon')
dim.lat <- dim.inq.nc(file.netcdf,'lat')
dim.t <- dim.inq.nc(file.netcdf,'time')

keterangan.variabel <- list()
for (a in 1:keterangan.file.netcdf$nvars) {
   keterangan.variabel[[a]] <- var.inq.nc(
      ncfile = file.netcdf, variable = a-1
   )
}

var.lon <- var.get.nc(file.netcdf,'lon')
var.lat <- var.get.nc(file.netcdf,'lat')
var.time <- var.get.nc(file.netcdf,'time')

var.data <- var.get.nc(
   ncfile = file.netcdf, variable = 'p',
   start = c(1,1,1),
   count = c(length(var.lon),length(var.lat),1)
)

var.data <- t(var.data)
list.lat.lon <- expand.grid(var.lat,var.lon)
var.data.not.na <- matrix(!is.na(var.data), ncol = 1)
var.data.not.na <- as.vector(var.data.not.na)

library(rworldmap)

newmap <- getMap(resolution = 'high')

plot(
   newmap, xlim = c(min(list.lat.lon[,2]),max(list.lat.lon[,2])),
   ylim = c(min(list.lat.lon[,1]),max(list.lat.lon[,1])),
   asp = 1
)

plot(
   newmap, xlim = c(min(var.lon),max(var.lon)),
   ylim = c(min(var.lat),max(var.lat)),
   asp = 1
)

points(
   x = list.lat.lon[which(var.data.not.na == TRUE),2],
   y = list.lat.lon[which(var.data.not.na == TRUE),1],
   pch = 21, col = 'red', cex = 0.5
)
# var.to.write <- var.data
# var.to.write[!is.na(var.to.write)] <- 1
# var.to.write[is.na(var.to.write)] <- 0
# 
# write.csv(x = var.to.write,file = 'datahujanindonesia.csv')
# 
# # var.data.vector <- t(var.data[rev(1:dim(var.data)[1]),rev(1:dim(var.data)[2])])
# var.data.vector <- t(var.data)
# var.data.vector <- matrix(var.data.vector,ncol = 1)
# list.lat.lon <- expand.grid(lats,lons)
# 
# indeks.var.data.vector.not.NA <- which(!is.na(var.data.vector))
