#    toLongDate Copyright (C) 2009, Oliver Kirchkamp
#    This program comes with ABSOLUTELY NO WARRANTY
#    This is free software, and you are welcome to redistribute it
#    under certain conditions. See the enclosed LICENSE for details.

toLongDate <-
function (shortDate) {
  sapply(as.character(shortDate),function(zz) {
    pre <- ifelse(substr(zz,1,2)<"80","20","19")
    if (nchar(zz)==8) {
#      hour  <- which(LETTERS==substr(zz,7,7))-1
      minute<- 60*which(LETTERS==substr(zz,7,7)) + (which(c(as.character(0:9),LETTERS)==substr(zz,8,8)))*2 - 21
      sprintf("%s%s-%02d:%02d",pre,substr(zz,1,6),minute%/%60,minute%%60)
    }
    else if (nchar(zz)==11) sprintf("%s%s-%s:%s",pre,substr(zz,1,6),substr(zz,8,9),substr(zz,10,11))
    else zz
  })
}
