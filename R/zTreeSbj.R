#    zTreeSbj Copyright (C) 2009, Oliver Kirchkamp
#    This program comes with ABSOLUTELY NO WARRANTY
#    This is free software, and you are welcome to redistribute it
#    under certain conditions. See the enclosed LICENSE for details.

zTreeSbj <-
    function(files,sep="\t",zTree.silent=getOption("zTree.silent"),zTree.encoding=getOption("zTree.encoding"),ignore.errors=FALSE) {
        myWarnings<-NULL
        w.handler <- function(w) {
            myWarnings<<-c(myWarnings,list(w))
            invokeRestart("muffleWarning")
        }

    if(is.null(zTree.silent))
        zTree.silent <- FALSE
    if(is.null(zTree.encoding))
        zTree.encoding <- getOption("encoding")
    wrong.Enc<-NULL
    sbj<-ldply (files, function(filename) {
        if(!zTree.silent) cat("reading ",filename,"...\n")
        Tfile<-file(filename,"r",encoding=zTree.encoding)
        aa<-withCallingHandlers(readLines(Tfile),warning=w.handler)
        ## catch warning for invalid encoding:
        if(length(myWarnings)>0)
            if(sum(sapply(myWarnings,function(x) startsWith(x$message,"invalid input")))>0)
            wrong.Enc<<-c(wrong.Enc,filename)
        close(Tfile)
        aa.i<-iconv(aa,sub="byte") ## try to convert to native encoding anyway
        if(sum(aa.i!=aa)>0) {
            wrong.Enc<<-c(wrong.Enc,filename)
            if(!ignore.errors)
                stop("You are currently using encoding \"",zTree.encoding,"\".
Your data seem to use a different encoding.
*** Read the manual! Change \"zTree.encoding\"! ***")
        }
        Date<-sub(".sbj$","",sub(".*/","",filename))
        aa2<-strsplit(aa.i[-1],sep)
        cols<-max(unlist(lapply(aa2,length)))
        aa3<-t(matrix(unlist(lapply(aa2,function(x)
            c(x,rep("",cols-length(x))))),byrow=TRUE,nrow=length(aa2)))
        colnames(aa3)<-aa3[1,]
        data.frame(cbind(Date=Date,aa3[-1,]))
    })
    if(length(wrong.Enc)>0) {
        wText <- paste("You are currently using encoding \"",zTree.encoding,"\".\nYour data seem to use a different encoding.\n*** Some of your data could not be translated. ***\n*** Read the manual! Change \"zTree.encoding\"! ***",sep="")
        if(ignore.errors) {
            warning(wText)
        } else {
            stop(wText)
        }
    }
    sbj
}
