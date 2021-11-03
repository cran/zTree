#    zTreeTables Copyright (C) 2009, Oliver Kirchkamp
#    This program comes with ABSOLUTELY NO WARRANTY
#    This is free software, and you are welcome to redistribute it
#    under certain conditions. See the enclosed LICENSE for details.

zTreeTables <-
    function(files,tables=c("globals","subjects"),sep = "\t",zTree.silent=getOption("zTree.silent"),zTree.encoding=getOption("zTree.encoding"),ignore.errors=FALSE) {
        if(is.null(zTree.silent))
            zTree.silent <- FALSE
        if(is.null(zTree.encoding))
            zTree.encoding <- getOption("encoding")
        manipulated.files <- list()
        wrong.Enc<-NULL
  splittable <- function(filename,tables=c("globals","subjects")) {
    getTable <- function(start, stop) {
      if (!is.na(stop) && !is.na(start)) {
        names<-f.table[[start]][-3]
        names[1]<-"Date"
        names[2]<-"Treatment"
        if(stop==start+1) ## empty table
            return(data.frame(NULL))
        tab<-as.data.frame(matrix(nrow=stop-start-1,ncol=length(names)))
        colnames(tab)<-names
        for( i in 1:(stop-start-1)) {
          tab[i,] <- f.table[[start+i]][-3]
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
          if (sum(fail)>0)
              manipulated.files[[filename]] <<- unique(c(unlist(manipulated.files[filename]),name))
          new<-new[,!fail]
          if (length(new)>0) {
              if (is.null(tab)) {
                  tab<-new
              } else {
                  tab <- plyr::rbind.fill(tab,new)
              }
          }
      }
      tab
    }
    
    myWarnings<-NULL
    w.handler <- function(w) {
        myWarnings<<-c(myWarnings,list(w))
        invokeRestart("muffleWarning")
    }
    ##
    advanceTable <- function() {
        last<<-last+1
        splitpoints[last]<<-i
        splitname[last]<<-f.table[[i]][3]
        splittreat[last]<<-f.table[[i]][2]
        splitcols[last]<<-length(f.table[[i]])
    }
    ##
    if(!zTree.silent) cat("reading ",filename,"...\n")
    f.size<-file.info(filename)$size
    f.content <- readChar(filename,f.size,useBytes=TRUE)
    Encoding(f.content)<-zTree.encoding
    fc.content <- iconv(f.content,zTree.encoding,sub="byte")
    if(fc.content != f.content) {
        wrong.Enc<<-c(wrong.Enc,filename)
        if(!ignore.errors)
            stop("You are currently using encoding \"",zTree.encoding,"\".
Your data seem to use a different encoding.
*** Read the manual! Change \"zTree.encoding\"! ***")
    }
    ##
    f.lines <- unlist(strsplit(fc.content,"\\n"))
    ## figure out table names with linebreaks:
    f.rr <- grep("\\r\\r$",f.lines)           ## which lines end with \\r\\r
    f.rr <- f.rr[grep("\\t",f.lines[f.rr+1])] ## make sure that next line starts with \\t
    if(length(f.rr>0)) {
        warning(paste0("*** Table name with linebreaks in lines ",paste(f.rr,collapse=", ")," ***"));
        f.lines[f.rr] <- paste(sub("\\r\\r$","",f.lines[f.rr]),f.lines[f.rr+1],sep="") ## copy up
        f.lines <- f.lines[-(f.rr+1)]             ## drop lines that we have copied up
    }
    f.lines <- sub("\r","",f.lines)
    ##
    f.table<-strsplit(f.lines,sep)
    if(length(f.table[[1]])<3) 
        stop(sprintf("*** cells are not separated by '%s'. Proper z-Tree files use \\t as a separator. Use the \"sep\" option! ***",ifelse(sep=="\t","\\t",sep)))
    splitpoints<-array()
    splitname<-array()
    splittreat<-array()
    table(splitname)
    splitcols<-array()
    last<-0
    for (i in 1:length(f.table)) {
        if (length(f.table[[i]])<3)
            stop(paste0("*** incomplete entry in line ",i," ***"));
        ##
        if (last==0) {## the first line in the file
            advanceTable();
        } else if ((f.table[[i]][3] != f.table[[i-1]][3]) || ## name of table has changed
                       sum(f.table[[i]] != f.table[[ splitpoints[last] ]]) == 0) { ## same name, but new header
                advanceTable();
        }
        splitpoints[last+1]<-i+1
    }
    result<-list()
    if(is.null(tables))
        tables<-unique(splitname);
    do <- intersect(splitname,tables)
    miss <- setdiff(splitname,tables)
    if(!zTree.silent & length(miss>0)) cat ("Skipping:",miss,"\n")
    for (name in do) {
      if(!zTree.silent) cat ("Doing:",name,"\n")
      aTable<-getTables(name)
      if (!is.null(aTable)) result[[name]]<-aTable
    }
    if(!is.null(manipulated.files[[filename]])) { ## add statistics on structure of table in case of manipulation
        myRange<-range(sapply(f.table,length))
        manipulated.files[[filename]] <<- c(manipulated.files[[filename]],sprintf("[%d,%d]",myRange[1],myRange[2]))
    }
    result
  }
        
  z<-splittable(files[1],tables)
  for (name in files[-1]) {
    if(!zTree.silent) cat (sprintf("*** %s is file %d / %d ***\n",name,which(name==files),length(files)))
    a<-splittable(name,tables)
    for(t in union(names(a),names(z))) {
      if (!is.null(a[[t]])) # there is no such table
        z[[t]]<-plyr::rbind.fill(z[[t]],a[[t]])
    }
  }
        ## wrong encoding:
        if(length(wrong.Enc)>0) {
            wText <- paste("You are currently using encoding \"",zTree.encoding,"\".\n Your data seem to use a different encoding.\n*** Some of your data could not be translated. ***\n*** Read the manual! Use the option \"zTree.encoding\"! ***",sep="")
            warning(wText)
        }

        ## manipulated Files:
        wText<-paste(sapply(names(manipulated.files),function(file)
            sprintf("*** File %s contains empty cells in %s. This is not a z-Tree file ***",
                    file,paste(manipulated.files[[file]],collapse=", "))),collapse="\n")
        if(length(manipulated.files)>0) {
            if (ignore.errors) {
                warning(wText)
            } else {
                stop(paste(wText,"\n*** use \"ignore.errors\" to continue ***"))
            }
        }
        
        ## try to convert characters to numbers if this does not introduce more NAs:
        for (t in names(z)) {
            for(n in names(z[[t]]))
                if(typeof(z[[t]][[n]])=="character") 
                    if(!is.null(q<-tryCatch(as.numeric(z[[t]][[n]]),warning=function(x) NULL))) z[[t]][[n]]<-q
        }
  z
}
