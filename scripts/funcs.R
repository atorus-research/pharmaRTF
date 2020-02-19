# funcs.R
#   File to store functions separately from data processing

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

n_pct <- function(n, pct) {
  # n (%) formatted string. e.g. 50 ( 75%)
  return(
    # Suppress conversion warnings
    as.character(
      # Form the string using glue and format
      glue('{format(n, width=3)} ({format(round((n/pct) * 100), width=3)}%)')
    )
  )
}

sum_subgrp <- function(subgroup_var) {
  # Create n (%) subgroups by TRTPCD
  
  # Convert string subgroup into a symbol that can be
  # unquoted with !!

  # Pull from adsl with totals
  adsl_ %>% 
    # Keep only the gtwo group variables and group byC:\Users\16105\OneDrive - ATorus\Documents\Projects\Explore\test2.rtf
    select(TRTPCD, {{ subgroup_var }}) %>% 
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
}

desc_stats <- function(var, na.rm=TRUE) {
  # Provides descriptive statistics of provided variable, by TRTPCD
  # n, Mean, SD, Median, Min, Max

  # Pull from ADSL with totals
  adsl_ %>%
    # Pick of TRTPCD and the variable of interest
    select(TRTPCD, {{ var }}) %>% 
    # Filter out missing values 
    filter(!is.na({{ var }})) %>% 
    # Group by treatment
    group_by(TRTPCD) %>%
    # Summarize each statistic and use num_fmt for rounding/formatting
    summarize(
      n      = num_fmt(n()),
      Mean   = num_fmt(   mean({{ var }}), 1),
      SD     = num_fmt(     sd({{ var }}), 2),
      Median = num_fmt( median({{ var }}), 1),
      Min    = num_fmt(    min({{ var }}), 1),
      Max    = num_fmt(    max({{ var }}), 1)
    ) %>% 
    # Transpose statistics into one column
    pivot_longer(-TRTPCD, names_to = 'rowlbl2', values_to = 'temp') %>% 
    # Transpose treatments into separate columns
    pivot_wider(names_from = TRTPCD, values_from = temp)
}

invert.list <- function (NL) {
  # Invert a list's items and names (assuming it's key value)
  L <- list()
  for (i in 1:length(NL)) {
    L[[NL[[i]]]] <- names(NL[i])
  }
  L
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