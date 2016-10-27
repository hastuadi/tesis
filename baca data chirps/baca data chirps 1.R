rm(list = ls())
library(RNetCDF)
setwd('D:/thesis/data chirps/')

# nama.file <- 'cobatulis.nc'
# 
# data.lon <- c(0,90,180)
# data.lat <- c(-50,-25,25,50)
# data.t <- c(1,2)
# 
# variabel <- array(
#    data = c(1:24),
#    dim = c(3,4,2)
# )
# 
# file.nc <- create.nc(nama.file)
# dim.def.nc(file.nc,'lon',length(data.lon))
# dim.def.nc(file.nc,'lat',length(data.lat))
# dim.def.nc(file.nc,'t',length(data.t))
# 
# var.def.nc(file.nc,'lon','NC_FLOAT','lon')
# var.def.nc(file.nc,'lat','NC_FLOAT','lat')
# var.def.nc(file.nc,'t','NC_FLOAT','t')
# var.def.nc(file.nc,'mydata','NC_FLOAT',c(0,1,2))
# 
# att.put.nc(ncfile = file.nc,
#            variable = 'lon',
#            name = 'units',
#            type = 'NC_CHAR',
#            value = 'degrees_east')
# 
# att.put.nc(ncfile = file.nc,
#            variable = 'lat',
#            name = 'units',
#            type = 'NC_CHAR',
#            value = 'degrees_north')
# 
# att.put.nc(ncfile = file.nc,
#            variable = 't',
#            name = 'units',
#            type = 'NC_CHAR',
#            value = 'hours since 2016-09-01 00:00')
# 
# att.put.nc(ncfile = file.nc,
#            variable = 'mydata',
#            name = 'units',
#            type = 'NC_CHAR',
#            value = 'degree celcius')
# 
# var.put.nc(ncfile = file.nc, variable = 'lon',data = data.lon)
# var.put.nc(ncfile = file.nc, variable = 'lat',data = data.lat)
# var.put.nc(ncfile = file.nc, variable = 't',data = data.t)
# var.put.nc(ncfile = file.nc, variable = 'mydata',data = variabel)
# 
# close.nc(file.nc)

nama.file <- 'cobatulis.nc'
file.nc <- open.nc(con = nama.file)
file.nc.report <- file.inq.nc(file.nc)
(file.nc.report$ndims)
dim.inq.nc(file.nc,0)
dim.inq.nc(file.nc,2)
var.lon <- var.get.nc(file.nc,'lon')
var.lat <- var.get.nc(file.nc,'lat')
var.t <- var.get.nc(file.nc,'t')

var.hujan <- var.get.nc(file.nc,'mydata',start = c(1,1,1), count = c(length(var.lon),length(var.lat),1))



lat.lon.expand <- expand.grid(var.lon,var.lat)

