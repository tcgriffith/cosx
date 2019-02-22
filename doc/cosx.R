## ---- echo=FALSE, results='hide'-----------------------------------------
old_loc <- Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE","English")
library("cosx")
library("utils")
f <- read.cosxs(system.file("cosxs", "cosxs.csv", package = "cosx"))
f$quote <- gsub('\\\\n', '\n',  f$quote)

Sys.setlocale("LC_CTYPE","Chinese")
n <- nrow(f)
f$n <- 1:n
f$vig <- paste(paste0('### ', f$n), f$quote, paste0('--- ', f$author, ' (', f$context, '), ', f$source, ', ', f$date), sep = '\n\n')

## ---- results='asis', echo=FALSE-----------------------------------------
cat(f$vig, sep = '\n\n\n\n')

## ---- echo=FALSE, results='hide'-----------------------------------------
Sys.setlocale("LC_CTYPE",old_loc)

