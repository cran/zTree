#    zTreeTables Copyright (C) 2009, Oliver Kirchkamp
#    This program comes with ABSOLUTELY NO WARRANTY
#    This is free software, and you are welcome to redistribute it
#    under certain conditions. See the enclosed LICENSE for details.

zTreeTables <-
    function(filelist,tables=c("globals","subjects"),sep = "\t",zTree.silent=getOption("zTree.silent"),zTree.encoding=getOption("zTree.encoding")) {
        if(is.null(zTree.silent)) zTree.silent<-FALSE
        if(is.null(zTree.encoding)) zTree.encoding<-"LATIN1"
  splittable <- function(filename,tables=c("globals","subjects")) {
    getTable <- function(start, stop) {
      if (!is.na(stop) && !is.na(start)) {
        names<-aa2[[start]][-3]
        names[1]<-"Date"
        names[2]<-"Treatment"
        tab<-as.data.frame(matrix(nrow=stop-start-1,ncol=length(names)))
        colnames(tab)<-names
        for( i in 1:(stop-start-1)) {
          tab[i,] <- aa2[[start+i]][-3]
        }
        for (n in colnames(tab)) {
          if (is.character(tab[[n]])) {
            tab[[n]][tab[[n]]=="-"] <- NA
            mm<-tryCatch(mean(as.numeric(tab[[n]]),na.rm=TRUE),warning=function(x) NA)
            if (!is.na(mm)) {
              tab[[n]]<-as.numeric(tab[[n]])
            }
          }
        }
        tab
      }
  }
    
    getTables <- function(name) {
      tab<-NULL
      for (i in which ((splitname==name))) {
          new<-getTable(splitpoints[i],splitpoints[i+1])
          fail<-names(new)==""
          if (sum(fail)>0) warning(sprintf("*** %s contains empty cells. This is not a z-Tree file ***",name))
          new<-new[,!fail]
          if (length(new)>0) {
              if (is.null(tab)) {
                  tab<-new
              } else {
                  tab <- rbind.fill(tab,new)
              }
          }
      }
      tab
    }
    if(!zTree.silent) cat("reading ",filename,"...\n")
    Tfile<-file(filename,"r",encoding=zTree.encoding)
    aa<-readLines(Tfile)
    close(Tfile)
    aa2<-strsplit(aa,sep)
    if(length(aa2[[1]])<3) 
        stop(sprintf("*** cells are not separated by '%s'. Proper z-Tree files use \\t as a separator. Use the \"sep\" option! ***",ifelse(sep=="\t","\\t",sep)))
    splitpoints<-array()
    splitname<-array()
    splittreat<-array()
    table(splitname)
    splitcols<-array()
    last<-0
    for (i in 1:length(aa2)) {
      if (last==0 || (aa2[[i]][3] != aa2[[i-1]][3])) {
        last<-last+1
        splitpoints[last]<-i
        splitname[last]<-aa2[[i]][3]
        splittreat[last]<-aa2[[i]][2]
        splitcols[last]<-length(aa2[[i]])
      }
      splitpoints[last+1]<-i+1
    }
    # cat(splitpoints)
    result<-list()
    if(is.null(tables))
        tables<-unique(splitname);
    do <- intersect(splitname,tables)
    miss <- setdiff(splitname,tables)
                                        #if (length(miss)>0)
    if(!zTree.silent) cat ("Skipping:",miss,"\n")
    for (name in do) {
      if(!zTree.silent) cat ("Doing:",name,"\n")
      aTable<-getTables(name)
      if (!is.null(aTable)) result[[name]]<-aTable
    }
    result
  }
  
  z<-splittable(filelist[1],tables)
  for (name in filelist[-1]) {
    if(!zTree.silent) cat (sprintf("*** %s is file %d / %d ***\n",name,which(name==filelist),length(filelist)))
    a<-splittable(name,tables)
    for(t in union(names(a),names(z))) {
      if (!is.null(a[[t]])) # there is no such table
        z[[t]]<-rbind.fill(z[[t]],a[[t]])
    }
  }
  # try to convert characters to numbers if this does not introduce more NAs:
  for (t in names(z))
    for(n in names(z[[t]]))
      if(typeof(z[[t]][[n]])=="character") 
        if(!is.null(q<-tryCatch(as.numeric(z[[t]][[n]]),warning=function(x) NULL))) z[[t]][[n]]<-q
  z
}
