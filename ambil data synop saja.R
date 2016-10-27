# mengambil yang penting saja, baru contoh membaca kemudian 
# menulisnya, baru satu file saja
# ---------------------------------------------------------

# rm(list = ls())
# setwd('D:/thesis/')
# 
# nama.file.kode.stasiun <- 'station index/filestasiundiolah2.csv'
# 
# isi.file.kode.stasiun <- read.csv2(nama.file.kode.stasiun,header = FALSE)
# isi.file.kode.stasiun <- as.character(unlist(isi.file.kode.stasiun))
# 
# tahun <- '_2013-01-01_2013-12-31.txt'
# 
# vector.nama.file <- rep(
#    'data/edit/data_bmkg/data2013/new_attachment_synop_',
#    times = length(isi.file.kode.stasiun)
# )
# 
# vector.nama.file <- paste(vector.nama.file,isi.file.kode.stasiun,
#                           tahun, sep = '')
# 
# isi.file.synop <- file(vector.nama.file[1],'r')
# baris.file.synop <- readLines(isi.file.synop)
# akhir.tabel <- which(baris.file.synop == 'na = NOT AVAILABLE.') - 1
# 
# nama.file.penyimpan <- 'data/edit/data_bmkg_tabel/data2013/file1.txt'
# 
# writeLines(baris.file.synop[9:akhir.tabel],con = nama.file.penyimpan)

# membukan, mengedit, untuk mengekstrak data harian,
# baru contoh satu file saja, yang sebelumnya sudah ditulis
# --------------------------------------------------

rm(list = ls())

setwd('D:/thesis/data/edit/data_bmkg_tabel/data2013/')

nama.file <- 'file1.txt'

tanggal <- seq(as.POSIXlt('2013-01-01',tz = 'UTC'),as.POSIXlt('2013-12-31',tz = 'UTC'),by = '1 day')

jam <- c('00:00','03:00','06:00','09:00','12:00','15:00','18:00','21:00')
# jam <- c('00:00')

data.waktu <- expand.grid(jam,tanggal)
data.waktu <- paste(data.waktu$Var2, data.waktu$Var1)
data.waktu <- as.POSIXlt(data.waktu, format = '%Y-%m-%d %H:%M', tz = 'UTC')
data.waktu <- unclass(data.waktu)

tanggal.satu.tahun <- data.waktu$mday

isi.file <- read.csv2(nama.file,sep = '\t',skip = 2,header = FALSE,
                      na.strings = 'na',fill = TRUE)

data.diambil.di.file <- isi.file[,c(1,2,20)]
data.diambil.di.file <- data.diambil.di.file[data.diambil.di.file[,2] == 0,]
data.diambil.di.file <- data.diambil.di.file[!is.na(data.diambil.di.file[,1]),]

data.hujan.ready <- data.diambil.di.file[!is.na(data.diambil.di.file[,3]),]