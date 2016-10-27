# ----

rm(list = ls())

setwd('D:/thesis/data/master/data chirps/p05/')

nama.file <- 'chirps2008subset.nc'

library(RNetCDF)

file.nc <- open.nc(con = nama.file)

RNetCDF::