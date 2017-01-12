.onLoad <- function (libname, pkgname) {
    op <- options()
    op.zTree <- list(zTree.silent=FALSE,zTree.encoding="LATIN1")
    toset <- !(names(op.zTree) %in% names(op))
    if(any(toset)) options(op.zTree[toset])
    invisible()
}
