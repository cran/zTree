#    zTreeSbj Copyright (C) 2009, Oliver Kirchkamp
#    This program comes with ABSOLUTELY NO WARRANTY
#    This is free software, and you are welcome to redistribute it
#    under certain conditions. See the enclosed LICENSE for details.

zTreeSbj <-
function(files,sep="\t",zTree.silent=getOption("zTree.silent"),zTree.encoding=getOption("zTree.encoding")) { 
    ldply (files, function(filename) {
        if(!zTree.silent) cat("reading ",filename,"...\n")
        Tfile<-file(filename,"r",encoding=zTree.encoding)
        aa<-readLines(Tfile)
        close(Tfile)
        Date<-sub(".sbj$","",sub(".*/","",filename))
        aa2<-strsplit(aa[-1],sep)
        cols<-max(unlist(lapply(aa2,length)))
        aa3<-t(matrix(unlist(lapply(aa2,function(x)
            c(x,rep("",cols-length(x))))),byrow=TRUE,nrow=length(aa2)))
        colnames(aa3)<-aa3[1,]
        data.frame(cbind(Date=Date,aa3[-1,]))
    })
}
