# baca cru

rm(list = ls())

library(RNetCDF)
setwd('D:/thesis/data/master/cru/')

file.nc <- open.nc('HadCRUT.4.5.0.0.median.nc')

close.nc(file.nc)