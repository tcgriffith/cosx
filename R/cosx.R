#' Read R cosx fortunes.
#'
#' @param file a character string giving a cosx fortune database in csv format (in UTF-8 encoding). By default all csv files in the data directory of the cosx package are used.
#'
#' @return a data frame of fortunes, each row contains:
#' - quote:	the quote, main part of the fortune,
#' - author: the author of the quote,
#' - context: the context in which it was quoted (if available, otherwise NA),
#' - source	: where it was quoted (if available, otherwise NA),
#' - date:	when it was quoted (if available, otherwise NA).
#' @importFrom utils read.table
#' @export
#'
#' @examples
#' read.cosxs()
read.cosxs <- function(file = NULL)
{
  old_loc <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE",old_loc))
  Sys.setlocale("LC_CTYPE","English")

  if(!is.null(file)) {
    cosxs <- file[file.exists(file)]
  } else {
    path <- system.file("cosxs", package = "cosx")
    datafiles <- list.files(path)
    if(!is.null(file) && file.exists(file.path(path, file))) {
      cosxs <- file.path(path, file)
    } else {
      if(length(file) > 0L) stop("sorry, ", sQuote(file), " not found")
      file <- datafiles[grep("\\.csv$", datafiles)]
      if(length(file) == 0L) stop("sorry, no cosxs data found")
      cosxs <- file.path(path, file)
    }
  }

  rval <- NULL
  for(file in cosxs) {
    rval <- rbind(rval, read.table(file, header = TRUE, sep = ";",
				   quote = "\"", colClasses = "character", encoding = 'UTF-8'))
  }
  rval
}

cosxs.env <- new.env()

#' Find R cosx fortunes.
#'
#' @param which an integer specifying the row number of `cosxs.data`. Alternatively `which`` can be a character and `grep`` is used to try to find a suitable row.
#' @param cosxs.data data frame containing a fortune in each row. By default the data from the 'cosx' package are used.
#' @param fixed logical passed to `grep` if `which`` is a character, indicating if it should work (if `TRUE`, as by default) with a simple character string or (if `FALSE`) with regular expressions.
#' @param showMatches if `which` is character, a logical indicating if `cosx()` should print all the row numbers of `cosxs.data` which match the `grep` search.
#' @param author a character string to match (via `grep`) to the "authors" column of `cosxs.data`.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return an object of class "cosx" which is a row from a data frame of fortunes (like those read in from read.cosxs).
#' @export
#'
#' @examples
#' cosx()
cosx <- function(which = NULL, cosxs.data = NULL, fixed = TRUE,
                    showMatches = FALSE, author = character(), ...)
{
  if(is.null(cosxs.data)) {
    if(is.null(cosxs.env$cosxs.data)) cosxs.env$cosxs.data <- read.cosxs()
    cosxs.data <- cosxs.env$cosxs.data
  }

  if(is.null(which) && !length(author)) {
    which <- sample.int(nrow(cosxs.data), 1)
  } else if(is.character(which) || length(author)) {
    if(length(author)) {
      if(is.null(fd.auth <- cosxs.data[, "author"])) {
        warning("'cosxs.data' does not have an \"author\" column")
      } else {
        cosxs.data <- cosxs.data[grep(author, fd.auth, useBytes = TRUE, fixed = fixed), ]
      }
    }
    if(is.character(which)) {
      fort <- apply(cosxs.data, 1, function(x) paste(x, collapse = " "))
      which1 <- grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
      if(length(which1) < 1) which1 <- grep(tolower(which), tolower(fort), useBytes = TRUE, fixed = TRUE, ...)
    } else {
      which1 <- seq_len(nrow(cosxs.data))
    }
    if(showMatches) cat("Matching row numbers:", paste(which1, collapse = ", "), "\n")
    which <- which1
    if(length(which) > 1) which <- sample(which, size = 1)
  }
  if(length(which) > 0 && which %in% seq(along = rownames(cosxs.data))) {
    structure(cosxs.data[which, ], class = "cosx")
  } else {
    character(0)
  }
}

#' Print R cosx fortunes.
#'
#' @param x an object of class "cosx", usually a single row from `cosxs.data`.
#' @param width integer specifying the character width. By default getOption("width") is used.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return print.
#' @export
print.cosx <- function(x, width = NULL, ...)
{
  if(is.null(width)) width <- getOption("width")
  if(width < 10) stop("'width' must be greater than 10")

  x$context <- if(is.na(x$context)) "" else paste(" (", x$context, ")", sep = "")
  if(is.na(x$source)) x$source <- ""
  x$date <- if(is.na(x$date)) "" else x$date <- paste(" (", x$date, ")", sep = "")
  if(anyNA(x)) stop("'quote' and 'author' are required")

  line1 <- x$quote
  line2 <- paste("   -- ", x$author, x$context, sep = "")
  line3 <- paste("      ", x$source, x$date, sep = "")

  # ## Problem: account for chase where line contains "\n"
  # linesplit <- function(line, width, gap = "      ") {
  #   if(nchar(line) < width) return(line)
  #   rval <- NULL
  #   while(nchar(line) > width) {
  #     line <- strsplit(line, " ")[[1]]
  #     if(any((nchar(line) + 1 + nchar(gap)) > width))
  #         stop("'width' is too small for cosx")
  #     breakat <- which.max(cumsum(nchar(line) + 1) > width) - 1L
  #     rval <- paste(rval, paste(line[1:breakat], collapse = " "), "\n", sep = "")
  #     line <- paste(gap, paste(line[-(1:breakat)], collapse = " "), sep = "")
  #   }
  #   paste(rval, line, sep = "")
  # }

  line1 <- strsplit(line1, "\\\\n")[[1]]
  # for(i in 1:length(line1))
  #   line1[i] <- linesplit(line1[i], width, gap = "")
  line1 <- paste(line1, collapse = "\n")
  # line2 <- linesplit(line2, width)
  # line3 <- linesplit(line3, width)

  cat(paste("\n", line1, "\n\n\n",
                  line2, "\n",
                  line3, "\n\n", sep = ""))
}


