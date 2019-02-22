#' Sling messages and warnings with flair
#'
#' @import cowsay
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @param what (character) What do you want to say? See details.
#' @param by (character) Type of thing, one of cow, chicken, poop, cat, facecat,
#' bigcat, longcat, shortcat, behindcat, longtailcat, anxiouscat, grumpycat,
#' smallcat, ant, pumpkin, ghost, spider, rabbit, pig, snowman, frog, hypnotoad,
#' signbunny, stretchycat, fish, trilobite, shark, buffalo, clippy, mushroom,
#' monkey, egret, or rms for Richard Stallman.
#' Alternatively, use "random" to have your message spoken by a random
#' character.
#' We use \code{\link{match.arg}} internally, so you can use unique parts of
#' words that don't conflict with others, like "g" for "ghost" because there's
#' no other animal that starts with "g".
#' @param type (character) One of message (default), warning, or string
#' (returns string). If multiple colors are supplied to \code{what_color} or
#' \code{by_color}, type cannot be warning. (This is a limitation of the \href{https://github.com/aedobbyn/multicolor}{multicolor} packcage :/.)
#' @param what_color (character or crayon function) One or more
#' \href{https://github.com/r-lib/crayon#256-colors}{\code{crayon}}-suported text color(s)
#' or \href{https://github.com/r-lib/crayon#styles}{\code{crayon style function}} to color
#'  \code{what}. You might try \code{colors()} or \code{?rgb} for ideas.
#' Use "rainbow" for c("red", "orange", "yellow", "green", "blue", "purple").
#' @param by_color (character or crayon function) One or more
#' \href{https://github.com/r-lib/crayon#256-colors}{\code{crayon}}-suported text color(s)
#' or \href{https://github.com/r-lib/crayon#styles}{\code{crayon style function}} to color
#'  \code{who}.
#'  Use "rainbow" for c("red", "orange", "yellow", "green", "blue", "purple").
#' @param length (integer) Length of longcat. Ignored if other animals used.
#' @param fortune An integer specifying the row number of cosxs.data.
#' Alternatively which can be a character and grep is used to try to find a
#' suitable row.
#' @param ... Further args passed on to \code{\link[fortunes]{fortune}}
#'
#' @details You can put in any phrase you like, OR you can type in one of a few
#' special phrases that do particular things. They are:
#'
#' \itemize{
#'  \item catfact A random cat fact from https://catfact.ninja
#'  \item fortune A random quote from an R coder, from fortunes library
#'  \item time Print the current time
#'  \item rms Prints a random 'fact' about Richard Stallman from the
#'  \code{\link[rmsfact]{rmsfact}}
#'  package. Best paired with \code{by = "rms"}.
#' }
#'
#' Note that if you choose \code{by='hypnotoad'} the quote is forced to be,
#' as you could imagine, 'All Glory to the HYPNO TOAD!'. For reference see
#' http://knowyourmeme.com/memes/hypnotoad
#'
#' Signbunny: It's not for sure known who invented signbunny, but this article
#' http://www.vox.com/2014/9/18/6331753/sign-bunny-meme-explained thinks
#' they found the first use in this tweet:
#' https://twitter.com/wei_bluebear/status/329101645780770817
#'
#' Trilobite: from http://www.retrojunkie.com/asciiart/animals/dinos.htm (site
#' down though)
#'
#' Note to Windows users: there are some animals (shortcat, longcat, fish,
#' signbunny, stretchycat, anxiouscat, longtailcat, grumpycat, mushroom) that
#' are not available because they use non-ASCII characters that don't display
#' properly in R on Windows.
#'
#' @examples
#' cosay()
cosay <- function(what="Hello world!", by="random",
                type="message",
                what_color=NULL, by_color=NULL,
                length=18, fortune=NULL, ...) {

  if (length(what) > 1) {
    stop("what has to be of length 1", call. = FALSE)
  }

  if (crayon::has_color() == FALSE) {
    message("Colors cannot be applied in this environment :( Try using a terminal or RStudio.")
    what_color <- NULL
    by_color <- NULL
  } else {
    what_color <- cowsay:::check_color(what_color)
    by_color <- cowsay:::check_color(by_color)
  }

  if (crayon::has_color() == FALSE && (!is.null(what_color) || !is.null(by_color))) {
    message("Colors cannot be applied in this environment :( Try using a terminal or RStudio.")
    what_color <- NULL
    by_color <- NULL
  } else {
    what_color <- cowsay:::check_color(what_color)
    by_color <- cowsay:::check_color(by_color)
  }

  if (what == "catfact") {
    check4pkg("jsonlite")
    what <-
      jsonlite::fromJSON(
        'https://catfact.ninja/fact')$fact
    by <- 'cat'
  }

  who <- cowsay:::get_who(by, length = length)

  # if (!is.null(fortune))
    what <- "fortune"

  if (what == "time")
    what <- as.character(Sys.time())
  if (what == "fortune") {
    f <- merge_text(method = 'console')
    if(is.null(fortune)) fortune <- sample(1:nrow(f), 1)
    what <- f$vig[fortune]
    what <- what[!is.na(what)] # remove missing pieces (e.g. "context")
    what <- gsub("\\\\n", "\n", paste(as.character(what), collapse = "\n "))
  }

  if (by == "hypnotoad" && what == "Hello world!") {
    what <- "All Glory to the HYPNO TOAD!"
  }

  if (what == "rms") {
    what <- rmsfact::rmsfact()
  }

  if ( what %in% c("arresteddevelopment", "doctorwho", "dexter", "futurama", "holygrail", "simpsons", "starwars", "loremipsum")) {
    check4pkg("jsonlite")
    what <-
      jsonlite::fromJSON(
        paste0('http://api.chrisvalleskey.com/fillerama/get.php?count=1&format=json&show=', what))$db$quote
  }

  what_pos_start <-
    regexpr('%s', who)[1] - 1

  what_pos_end <- what_pos_start + 3

  color_text <- function(txt, c) {
    if (is.null(c)) {
      out <- txt
    } else if (!is.null(c) && inherits(c, "crayon")) {
      out <- c(txt)
    } else if (!is.null(c) && is.character(c)) {
      if (length(c) <= 1) {
        c <- crayon::make_style(c)
        out <- c(txt)
      } else if (length(c) >= 1) {
        out <- multicolor::multi_color(txt, c,
                                       type = "string")
      }
    }
    return(out)
  }

  # TODO: when multicolor doesn't color every character individually, this should be possible
  # and we can get rid of what_pos_start and what_pos_end
  # what <- color_text(what, what_color)
  # who <- color_text(who, by_color)
  # out <- sprintf(who, what)

  # switch(type,
  #        message = message(sprintf(who, what)),
  #        warning = warning(sprintf(who, what)),
  #        string = sprintf(who, what))

  out <- paste0(color_text(substr(who, 1, what_pos_start),
                           by_color),
                color_text(what,
                           what_color),
                color_text(substr(who, what_pos_end, nchar(who)),
                           by_color))

  if (type == "warning") {
    if (nchar(out) < 100) {
      wl <- 100
    } else if (nchar(out) > 8170) {
      wl <- 8170
    } else {
      wl <- nchar(out) + 1
    }
    warn_op <- options(warning.length = wl)
    on.exit(options(warn_op))
  }

  switch(type,
         message = message(out),
         warning = warning(out),
         string = out)
}
