#' Reads individual FreeSurfer .mgh files
#'
#' @param input.file input filename
#' @examples
#' fs.data <- load.mgh('./lh.thickness.mgh')
#'
load.mgh <- function(input.file) {
    
    to.read <- file(input.file, "rb")
    
    v <- readBin(to.read, integer(), endian = "big")
    ndim1 <- readBin(to.read, integer(), endian = "big")
    ndim2 <- readBin(to.read, integer(), endian = "big")
    ndim3 <- readBin(to.read, integer(), endian = "big")
    nframes <- readBin(to.read, integer(), endian = "big")
    type <- readBin(to.read, integer(), endian = "big")
    dof <- readBin(to.read, integer(), endian = "big")
    
    close(to.read)
    
    to.read <- file(input.file, "rb")
    dump <- readBin(to.read, double(), size = 4, n = 71, endian = "big")
    x <- readBin(to.read, double(), size = 4, n = ndim1, endian = "big")
    close(to.read)
    
    list(x = x, v = v, ndim1 = ndim1, ndim2 = ndim2, ndim3 = ndim3, nframes = nframes, type = type, dof = dof)
}


#' Reads FreeSurfer data for several individuals using parallel computing
#'
#' @param SUBJECTS_DIR freesurfer SUBJECTS_DIR
#' @param subjid individual subject ids
#' @param feature morphometric feature (e.g. thickness, area etc.)
#' @param fwhm smoothing kernes (e.g. 10)
#' @param hemi either 'lh.rh' or 'lh' or 'rh'
#' @param ncores number of cores used to read in data
#' @examples
#' ncores = parallel::detectCores() - 1 # identify number of cores
#' SUBJECTS_DIR <- c('/Users/sphache/Data/LEAP_wave1/')
#' brain.data <- ReadFreeSurferData(SUBJECTS_DIR, demographics$Subject, 'thickness', 10, 'lh.rh', 10)
#'
read.fs.data <- function(SUBJECTS_DIR, subjid, feature, fwhm, hemi = "lh.rh", ncores = 10) {
    nsubj = length(subjid)
    fwhm <- paste("fwhm", fwhm, sep = "")
    
    ### Create filenames
    filenames.lh <- 1:nsubj  # Generate Filenames
    filenames.rh <- 1:nsubj
    for (i in 1:nsubj) {
        filenames.lh[i] <- paste(SUBJECTS_DIR, subjid[i], "/surf/lh.", feature, ".", fwhm, ".fsaverage.mgh", sep = "")
        filenames.rh[i] <- paste(SUBJECTS_DIR, subjid[i], "/surf/rh.", feature, ".", fwhm, ".fsaverage.mgh", sep = "")
    }
    
    ### Read Data
    mydata.lh <- parallel::mclapply(filenames.lh, load.mgh, mc.cores = ncores)  #Load Data access with mydata[[1]]$x
    mydata.rh <- parallel::mclapply(filenames.rh, load.mgh, mc.cores = ncores)
    
    ### Concatenate left and right hemisphere into data frame
    nvertices <- mydata.lh[[1]]$ndim1
    mydata <- matrix(nrow = nsubj, ncol = 2 * nvertices)  # declare data matrix
    for (i in 1:nsubj) {
        mydata[i, ] <- c(mydata.lh[[i]]$x, mydata.rh[[i]]$x)
    }
    
    ## divide hemispheres if required
    if (hemi == "lh") {
        mydata <- mydata[, 1:nvertices]
    } else if (hemi == "rh") {
        mydata <- mydata[, (nvertices + 1):(2 * nvertices)]
    }
    
    return(mydata)
}


#' Writes individual FreeSurfer .mgh file
#'
#' @param input.file input filename
#' @examples
#' x <- runif(163842, 1, 10)
#' save.mgh(x, 'lh.test.mgh')
#'
save.mgh <- function(x, fname) {
    
    ndim1 <- length(x)
    vol <- list(x = x, v = 1, ndim1 = ndim1, ndim2 = 1, ndim3 = 1, nframes = 1, type = 3, dof = 0)
    
    MRI.UCHAR <- 0
    MRI.INT <- 1
    MRI.LONG <- 2
    MRI.FLOAT <- 3
    MRI.SHORT <- 4
    MRI.BITMAP <- 5
    MRI.TENSOR <- 6
    slices <- c(1:256)
    
    fid <- file(fname, open = "wb", blocking = TRUE)
    
    width <- vol$ndim1
    height <- vol$ndim2
    depth <- vol$ndim3
    
    writeBin(as.integer(1), fid, size = 4, endian = "big")
    writeBin(as.integer(width), fid, size = 4, endian = "big")
    writeBin(as.integer(height), fid, size = 4, endian = "big")
    writeBin(as.integer(depth), fid, size = 4, endian = "big")
    writeBin(as.integer(1), fid, size = 4, endian = "big")
    writeBin(as.integer(MRI.FLOAT), fid, size = 4, endian = "big")
    writeBin(as.integer(1), fid, size = 4, endian = "big")
    
    UNUSED.SPACE.SIZE <- 256
    USED.SPACE.SIZE <- (3 * 4 + 4 * 3 * 4)  # space for ras transform
    unused.space.size <- UNUSED.SPACE.SIZE - 2
    
    writeBin(as.integer(0), fid, size = 2, endian = "big")
    
    writeBin(as.integer(rep.int(0, unused.space.size)), fid, size = 1)
    bpv <- 4  # bytes/voxel
    nelts <- width * height  # bytes per slice
    writeBin(vol$x, fid, size = 4, endian = "big")
    close(fid)
}
