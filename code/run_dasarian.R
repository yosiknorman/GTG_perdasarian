# notasi
# dummy =   "merg_2012010100_4km-pixel.nc4"
# FNdummy = "merg_2012123123_4km-pixel.nc4"
# dummyYH = substr(dummy, 6, 15)
# YYYY = substr(dummyYH, 1, 4)
# MM = substr(dummyYH, 5, 6)
# DD = substr(dummyYH, 7, 8)
# HH = substr(dummyYH, 9, 10)
# 
# dummyFN = substr(FNdummy, 6, 15)
# YYYY1 = substr(dummyFN, 1, 4)
# MM1 = substr(dummyFN, 5, 6)
# DD1 = substr(dummyFN, 7, 8)
# HH1 = substr(dummyFN, 9, 10)
# ST = as.POSIXct(paste0(YYYY, "-", MM, "-", DD, " ", HH, ":00:01"), tz = "UTC")
# FN = as.POSIXct(paste0(YYYY1, "-", MM1, "-", DD1, " ", HH1, ":00:01"), tz = "UTC")
# 
# STD = as.POSIXct(paste0(YYYY, "-", MM, "-", DD, " ", HH, ":00:00"), tz = "UTC")
# FND = as.POSIXct(paste0(YYYY1, "-", MM1, "-", DD1, " ", HH1, ":00:00"), tz = "UTC")
# 
# sequen = seq(ST, FN, by = "hours")
rm(list = ls())

datax = list.files("../data/")
datax[1]

dummyYH = substr(datax, 6, 15)
YYYY = substr(dummyYH, 1, 4)
MM = substr(dummyYH, 5, 6)
DD = substr(dummyYH, 7, 8)
HH = substr(dummyYH, 9, 10)

sequen = as.POSIXct(paste0(YYYY, "-", MM, "-", DD, " ", HH, ":00:00"), tz = "UTC")

bulan = 1:12
for(i in 1:12){
  if(nchar(bulan[i]) < 2 ){
    bulan[i] = paste0("0", bulan[i])
  }
}

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

is.character0 <- function(x)
{
  is.character(x) && length(x) == 0L
}

sM = substr(sequen, 6, 7)
M = "01"
filterdas = function(M, sequen ){
  das = c("01", "11", "21")
  sM = substr(sequen, 6, 7)
  SM_Filt = sequen[which(as.numeric(sM) == as.numeric(M))]
  sD = substr(SM_Filt, 9, 10)
  SM_Filt_next = sequen[which(as.numeric(sM) == (as.numeric(M)+1))]
 
  if( is.character0(as.character(SM_Filt_next)) ){
    SM_Filt1 = SM_Filt[which(as.numeric(sD) >= as.numeric(das[1]) & as.numeric(sD) <= as.numeric(das[2]))]
    SM_Filt2 = SM_Filt[which(as.numeric(sD) >= as.numeric(das[2]) & as.numeric(sD) <= as.numeric(das[3]))]
    SM_Filt3 = SM_Filt[which(as.numeric(sD) >= as.numeric(das[3]))]
  }else if( !is.character0(as.character(SM_Filt_next)) ){
    sD_next = substr(SM_Filt_next, 9, 10)
    SM_Filt1 = SM_Filt[which(as.numeric(sD) >= as.numeric(das[1]) & as.numeric(sD) <= as.numeric(das[2]))]
    SM_Filt2 = SM_Filt[which(as.numeric(sD) >= as.numeric(das[2]) & as.numeric(sD) <= as.numeric(das[3]))]
    SM_Filt3 = c(SM_Filt[which(as.numeric(sD) >= as.numeric(das[3]))], 
                 SM_Filt_next[which(as.numeric(sD_next) <= as.numeric(das[1]) )]) 
  }
  hasil = list(Das1 = SM_Filt1,  Das2 = SM_Filt2,Das3 = SM_Filt3)
  system(paste0("mkdir ../siap_running/", substr(SM_Filt1, 1, 4), substr(SM_Filt1, 6, 7), "D", "01"))
  system(paste0("mkdir ../siap_running/", substr(SM_Filt2, 1, 4), substr(SM_Filt2, 6, 7), "D", "02"))
  system(paste0("mkdir ../siap_running/", substr(SM_Filt3, 1, 4), substr(SM_Filt3, 6, 7), "D", "03"))
  
  
  copycode = function(xy, Das ){
    DAS = gsub(as.matrix(as.character(xy)), pattern = "\\:", replacement = "")
    DAS = gsub(DAS, pattern = "-", replacement = "")
    DAS = gsub(DAS, pattern = " ", replacement = "")
    DAS = substr(DAS, 1, 10)
    jadi = paste0 ("merg_", DAS, "_4km-pixel.nc4" )
    for(i in 1:length(xy)){
      system(paste0("cp ../data/",jadi[i],"  ../siap_running/", substr(xy, 1, 4), substr(xy, 6, 7), "D",Das))
    }
  }
  
  copycode(SM_Filt1, "01");
  copycode(SM_Filt2, "02")
  copycode(SM_Filt3, "03")
  # for(i in 1:length(SM_Filt1)){
  #   system(paste0("cp ../data/",jadi[i]," ../siap_running/", substr(SM_Filt1, 1, 4), substr(SM_Filt1, 6, 7), 01))
  # }
  
  # return(hasil)
}

filterdas(M = bulan[1], sequen = sequen)
