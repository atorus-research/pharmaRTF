view_hf <- function(doc, type=NULL) {

  # Take out the lines
  lines = doc[[type]]

  # Stop if nothing to view
  assert_that(length(lines) > 0,
              msg = sprinf('No %s to view', type))

  # Get the row length
  rows <- length(lines)

  # Create the template data frame
  df <- data.frame(
    text1 = character(rows),
    text2 = character(rows),
    align = character(rows),
    bold = logical(rows),
    italic = logical(rows),
    font = character(rows),
    stringsAsFactors = FALSE
  )

  # Loop the lines and update rows in data frame
  for (i in 1:length(lines)) {
    l <- lines[[i]]
    df$text1[i] <- l$text[[1]]
    if (length(l$text) > 1) df$text2[[i]] <- l$text[[2]]
    df$align[i] <- attr(l, 'align')
    df$bold[i] <- attr(l, 'bold')
    df$italic[i] <- attr(l, 'italic')
    df$font[i] <- attr(l, 'font')
  }

  View(df)
}

# Simplified for titles
view_titles <- function(doc) {
  pharmaRTF:::view_hf(doc, type='titles')
}

# Simplified for footnotes
view_footnotes <- function(doc) {
  pharmaRTF:::view_hf(doc, type='footnoes')
}
