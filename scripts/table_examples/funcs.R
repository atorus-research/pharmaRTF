# funcs.R
#   File to store functions separately from data processing

example_custom_reader <- function(..., table_number=NULL) {

  # Make sure that readxl is installed before
  if (suppressWarnings(!require('readxl'))) {
    stop("This reader requires the package `readxl`. Install using `install.packages('readxl')")
  }

  # If a column isn't populated then the type may be guessed wrong so force it
  col_types <- c('text', 'numeric', 'text', 'text', 'text', 'text', 'logical', 'logical', 'text')
  # pass through arguments from ...
  df <- readxl::read_excel(..., col_types=col_types)

  # Subset and return that dataframe
  df[df$table_number==table_number, !names(df) == 'table_number']
}


get_meta <- function(df) {
  # Examines the metadata of a SAS imported tibble
  for (name in names(df)) {
    lab <- attr(df[[name]], 'label')
    name <- str_pad(name, 8)
    cat(name, lab, "\n", sep="\t")
  }
}

num_fmt <- function(var, digits=0, size=10, int_len=3) {
  # Formats summary stat strings to align display correctly

  # Set nsmall to input digits
  nsmall = digits

  # Incremement digits for to compensate for display
  if (digits > 0) {
    digits = digits + 1
  }

  # Form the string
  return(str_pad(
    format(
      # Round
      round(var, nsmall),
      # Set width of format string
      width=(int_len+digits),
      # Decimals to display
      nsmall=nsmall
    ),
    # Overall width padding
    side='right', size
  ))
}

n_pct <- function(n, pct, n_width=3, pct_width=3) {
  # n (%) formatted string. e.g. 50 ( 75%)
  return(
    # Suppress conversion warnings
    as.character(
      # Form the string using glue and format
      glue('{format(n, width=n_width)} ({format(round((n/pct) * 100), width=pct_width)}%)')
    )
  )
}

sum_subgrp <- function(subgroup_var, include.n=TRUE, pad.row=TRUE) {
  # Create n (%) subgroups by TRTPCD

  # Convert string subgroup into a symbol that can be
  # unquoted with !!

  # Pull from adsl with totals
  df <- adsl_ %>%
    # Keep only the gtwo group variables and group byC:\Users\16105\OneDrive - ATorus\Documents\Projects\Explore\test2.rtf
    select(TRTPCD, {{ subgroup_var }}) %>%
    filter(!is.na({{ subgroup_var }})) %>%
    group_by(TRTPCD, {{ subgroup_var }}) %>%
    # Summarize counts
    summarize(
      n = n()
    ) %>%
    # Merge with big Ns
    left_join(header_n_m, by = 'TRTPCD') %>%
    rowwise() %>%
    # Create the n (%) string
    mutate(
      res = n_pct(n, N)
    ) %>%
    # Drop unnecessary vars
    select(-n, -N, -labels) %>%
    # Transpose
    pivot_wider(names_from = TRTPCD, values_from = res) %>%
    # Take care of NA results
    replace(is.na(.), '  0       ') %>%
    # Rename row label column
    rename(rowlbl2 = {{ subgroup_var }})

  if (include.n){
    df <- rbind(desc_stats({{subgroup_var}}, include='n')[1,], df)
  }

  pad_row(df)

}

desc_stats <- function(var, na.rm=TRUE, int_len=3, size=10, include=c('n', 'Mean', 'SD', 'Median', 'Min', 'Max')) {
  # Provides descriptive statistics of provided variable, by TRTPCD
  # n, Mean, SD, Median, Min, Max

  # Ensure that the include argument was valid
  include = match.arg(include, several.ok=TRUE)

  # This is gonna get wonky - store each summary as an expression
  #TODO: Allow flexibility in significant digits - right now it's hard coded
  summaries <- list(
    n      = rlang::expr(num_fmt(      n()         , digits=0, int_len=int_len, size=size)),
    Mean   = rlang::expr(num_fmt(   mean({{ var }}), digits=1, int_len=int_len, size=size)),
    SD     = rlang::expr(num_fmt(     sd({{ var }}), digits=2, int_len=int_len, size=size)),
    Median = rlang::expr(num_fmt( median({{ var }}), digits=1, int_len=int_len, size=size)),
    Min    = rlang::expr(num_fmt(    min({{ var }}), digits=1, int_len=int_len, size=size)),
    Max    = rlang::expr(num_fmt(    max({{ var }}), digits=1, int_len=int_len, size=size))
  )[include] # this is a named list, so subset based on the input arguments

  # Pull from ADSL with totals
  adsl_ %>%
    # Pick of TRTPCD and the variable of interest
    select(TRTPCD, {{ var }}) %>%
    # Filter out missing values
    filter(!is.na({{ var }})) %>%
    # Group by treatment
    group_by(TRTPCD) %>%
    # Summarize each statistic and use num_fmt for rounding/formatting
    summarize(!!!summaries) %>% # unpack the expressions into syntax to be evaluated
    # Transpose statistics into one column
    pivot_longer(-TRTPCD, names_to = 'rowlbl2', values_to = 'temp') %>%
    # Transpose treatments into separate columns
    pivot_wider(names_from = TRTPCD, values_from = temp) %>%
    pad_row()
}

invert.list <- function (NL) {
  # Invert a list's items and names (assuming it's key value)
  L <- list()
  for (i in 1:length(NL)) {
    L[[NL[[i]]]] <- names(NL[i])
  }
  L
}

# P-value for anova test
aov_p <- function(data, forumula) {
  # Run the anova test
  a <- aov(forumula, data, na.action=na.omit)

  # Extract the P value
  p <- summary(a)[[1]][['Pr(>F)']][1]
  # Format the output

  format(round(p, 4), width=10, nsmall=4)
}

# P-value for chi-squared
chi_p <- function(data, results, categories) {
  # get the arguments as a off of the function call
  arguments <- as.list(match.call())
  # Evaluate the arguments within the dataframe environment
  # This is all just so I can allow acceptance of variables without quotes
  cats <- factor(eval(arguments$categories, data))
  res <- factor(eval(arguments$results, data))

  p <- chisq.test(res, cats)$p.value
  format(round(p, 4), width=10, nsmall=4)
}

# Attach P-value to the first row of a dataframe
attach_p <- function(data, p_value) {
  # Empty column
  data[['p']] = character(nrow(data))
  data[1, 'p'] = p_value

  data

}

# Add an empty row
pad_row <- function(df, n=1) {
  df[(nrow(df)+1):(nrow(df)+n), ] <- ""
  df
}














make_groups <- function(t, groupvar) {
  # KEEPING THIS JUST IN CASE - BUT REPLACED WITH AND OPTION ON gt()
  # Wrapper to apply multiple calls of tab_row_group for a specified
  # group variable. Groups are automatically determined based on
  # distinct values

  # Quosure for the string variable provided
  grpvar <- sym(groupvar)

  # Loop the unique values of the group variable
  for (g in unique(t$`_data`[[groupvar]])) {
    # Create the tab row group
    t <- tab_row_group(
      t, # gt object
      group = g, # Group label to be printed
      rows = !!grpvar == g # Subset rows by evaluating against group variable
    )
  }

  # Hide the group variable
  t <- cols_hide(t, columns = groupvar)

  return(t)
}
