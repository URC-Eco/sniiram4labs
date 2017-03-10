generate_pw <- function(n, type = c("phrase", "character"),
               complexity = c("simple", "medium", "complex"),
               warn = TRUE) {
  type <- match.arg(type)
  complexity <- match.arg(complexity)

  switch(
    type,
    "phrase" = {
      load("./R/passphrase.RData")
      x <- switch(complexity,
                  simple = pass_fr_simple$x,
                  medium = pass_fr_medium$x,
                  complex = pass_fr_complex$x)
      sep <- " "
    },
    "character" = {
      x <- unlist(strsplit(rawToChar(as.raw(32:126)), ""))
      sep <- ""
    }
  )

  N <- length(x)
  cpx <- N^n

  set.seed(Sys.time())
  res <- sample(x, size = n, replace = TRUE)

  if (warn && cpx < 10^15) warning("Complexity is probably too low...\n",
                                   "Aim for values > 1e+15")
  structure(paste(res, collapse = sep), class = "pw", cpx = cpx)
}

password <- function(n) {
  generate_pw(n = n, type = "character")
}

passphrase <- function(n, complexity) {
  if (missing(complexity)) complexity <- "simple"
  generate_pw(n = n, complexity = complexity, type = "phrase")
}

print.pw <- function(x, ...) {
  cat(sprintf("Complexity: %.1e\n\n", attr(x, "cpx")))
  cat(x)
  invisible(x)
}
