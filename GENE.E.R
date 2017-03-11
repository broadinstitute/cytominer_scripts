library(rhdf5)
library(RCurl)
write.gctx <- function(mdat, row.annotations=NULL, column.annotations=NULL, write.rownames=T, write.colnames=T, row.hclust=NULL, column.hclust=NULL, file) {
	if(!is.null(row.hclust)) {
		mdat <- mdat[row.hclust$order,]
		if(!is.null(row.annotations)) {
			 row.annotations <- row.annotations[row.hclust$order,]
		}
	}
	if(!is.null(column.hclust)) {
		mdat <- mdat[,column.hclust$order]
		if(!is.null(column.annotations)) {
			 column.annotations <- column.annotations[column.hclust$order,]
		}
	}
	h5createFile(file)
	# h5writeAttribute('GCTX1.0', file, '/', 'version')

	h5createGroup(file, '0')
	h5createGroup(file, '0/DATA')
	h5createGroup(file, '0/DATA/0')
	h5write(mdat, file,'0/DATA/0/matrix')

	h5createGroup(file, '0/META')
	h5createGroup(file, '0/META/COL')
	h5createGroup(file, '0/META/ROW')
	if(!is.null(row.hclust)) {
		id <- paste (0:(ncol(mdat)-1),'X',sep='')
		h5write(id[row.hclust$order], file,'0/META/ROW/gene id')
		r2atr(file, row.hclust, TRUE)
	}
	if(!is.null(column.hclust)) {
		id <- paste (0:(nrow(mdat)-1),'X',sep='')
		h5write(id[column.hclust$order], file,'0/META/COL/array id')
		r2atr(file, column.hclust, FALSE)
	}
	if(!is.null(row.annotations) || !is.null(rownames(mdat))) {
		write.meta(file, FALSE, rownames(mdat), row.annotations, write.rownames)
	}
	if(!is.null(column.annotations) || !is.null(colnames(mdat))) {
		write.meta(file, TRUE, colnames(mdat), column.annotations, write.colnames)
	}
}

r2atr <- function(file, hc, rows)
{
  height <- hc$height
  n <- length(height)
  node <- 1:n
  node <- paste ('NODE',node,'X',sep='')

  merge1  <- hc$merge[,1]
  merge11 <- paste ('NODE',merge1,'X',sep='')
  merge12 <- paste ( -1-merge1,'X',sep='')
  merge1[hc$merge[,1]>0] <- merge11[hc$merge[,1]>0]
  merge1[hc$merge[,1]<0] <- merge12[hc$merge[,1]<0]

  merge2  <- hc$merge[,2]
  merge11 <- paste ('NODE',merge2,'X',sep='')
  merge12 <- paste (-1-merge2,'X',sep='')
  merge2[hc$merge[,2]>0] <- merge11[hc$merge[,2]>0]
  merge2[hc$merge[,2]<0] <- merge12[hc$merge[,2]<0]

  path <- if(rows) '0/DATA/row_dendrogram' else '0/DATA/column_dendrogram'
  h5createGroup(file, path)

  h5write(node, file, paste(path, 'id', sep='/'))
  h5write(merge1, file, paste(path, 'left', sep='/'))
  h5write(merge2, file, paste(path, 'right', sep='/'))
  h5write(height, file, paste(path, 'distance', sep='/'))
}

write.meta <- function(file, isColumns, n, annotations, write.names) {
	path <- if(isColumns) '0/META/COL/' else '0/META/ROW/'
	if(!is.null(n) && write.names) {
		if(class(n) == 'factor' || class(n)== 'AsIs') {
			n <- as.character(n)
		}
		h5write(n, file, paste(path, 'Name', sep='')) # row or column names
	}
	if(!is.null(annotations)) {
		names <- colnames(annotations)
		number.of.names <- length(names)
		if(number.of.names > 0) {
			for(i in 1:number.of.names) {
				name = names[i]
				v <- annotations[[i]]
				if(class(v) == 'factor' || class(v)== 'AsIs') {
					v <- as.character(v)
				}
				h5write(v, file, paste(path, name, sep=''))
			}
		}
	}
}

read.gctx = function (f, matrix=TRUE) {
	mat <-  NULL
	if(matrix) {
		mat <-  h5read(f, "0/DATA/0/matrix")
	}
	row.meta <-  read.meta(f, "0/META/ROW")
	column.meta <- read.meta(f, "0/META/COL")
	return(list(row.annotations=row.meta, column.annotations=column.meta, matrix=mat))
}

read.meta = function (f, path) {
	obj <- h5read(f, path)
    fields <- names(obj)
    meta.data <-  data.frame(matrix(nrow = length(obj[[fields[1]]]), ncol = length(fields)))
 	colnames(meta.data) <-  fields
 	for(i in 1:length(fields)) {
 		field <- fields[i]
 		meta.data[,i] <- obj[[field]]
    }
    return(meta.data)
}

from.genee <- function(url='http://localhost:9998') {
    url <- get.genee.url(url)
	f <- tempfile()
	download.file(paste(url, 'api/from', sep='/'), f, mode = "wb", cacheOK=F)
	on.exit(unlink(f))
 	m <- read.gctx(f)
	return(m)
}


to.genee <- function (mdat, row.annotations=NULL, column.annotations=NULL, show.rownames=T, show.colnames=T, row.hclust=NULL, column.hclust=NULL, url='http://localhost:9998' ) {
    url <- get.genee.url(url)
	name <- deparse(substitute(mdat))
	dir = tempdir()
	if(file.exists('/tmp/shm') && file.access('/tmp/shm', mode=2) == 0) {
		dir = '/tmp/shm'
	}
	f <- tempfile(pattern = "file", tmpdir = dir, fileext = ".gctx")
	on.exit(unlink(f))
	write.gctx(mdat, row.annotations, column.annotations, show.rownames, show.colnames, row.hclust, column.hclust, f)
	#curl_setopt($curl, CURLOPT_HTTPHEADER, array('Expect:'));
	x <-postForm(paste(url, 'api/to', sep='/'), binary=T, "fileName"=name, "fileData" = fileUpload(f,  contentType = 'application/gctx'), .opts = list(verbose = FALSE, header = TRUE))
}


get.genee.url <- function(url)
{
    if (nchar(Sys.getenv("GENE_E_URL")))
        Sys.getenv("GENE_E_URL")
    else
        url
}



