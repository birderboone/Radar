.removeTrailingSlash <- function(d) {
  if (substr(d, .nchar(d), .nchar(d)) == '/') { d <- substr(d, 1, .nchar(d)-1) }
  if (substr(d, .nchar(d), .nchar(d)) == '\\') { d <- substr(d, 1, .nchar(d)-1) }
  return(d)
}

removeTmpFiles<-function (h = 24) 
{
  warnopt <- getOption("warn")
  on.exit(options(warn = warnopt))
  tmpdir <- tmpDir(create = FALSE)
  if (!is.na(tmpdir)) {
    d <- .removeTrailingSlash(tmpdir)
    f <- list.files(path = d, pattern = "r_tmp*", full.names = TRUE, 
                    include.dirs = TRUE)
    fin <- file.info(f)
    dif <- Sys.time() - fin$mtime
    dif <- as.numeric(dif, units = "hours")
    dif[is.na(dif)] <- h + 1
    f <- f[dif > h]
    if (length(f) > 1) {
      unlink(f, recursive = TRUE)
    }
  }
  options(warn = warnopt)
}