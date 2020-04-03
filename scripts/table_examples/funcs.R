# funcs.R
#   File to store functions separately from data processing

require(tidyverse)
require(glue)

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
    left_join(header_n, by = 'TRTPCD') %>%
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
  if(round(p, 4) == 0) return("<.0001")
  format(round(p, 4), width=10, nsmall=4)
}

# P-vaule for fisher's test
fish_p <- function(data, results, categories, width = 10) {
  # get the arguments as a off of the function call
  arguments <- as.list(match.call())
  # Evaluate the arguments within the dataframe environment
  # This is all just so I can allow acceptance of variables without quotes
  cats <- factor(eval(arguments$categories, data))
  res <- factor(eval(arguments$results, data))

  p <- fisher.test(res, cats)$p.value
  if(round(p, 4) == 0) return("<.0001")
  format(round(p, 4), width=width, nsmall=4)
}

# Attach P-value to the first row of a dataframe
attach_p <- function(data, p_value, digits = 4) {
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


## Efficacy functions ----


# Create the summary data portion for 14-3.01 through XXXX ----
summary_data <- function(data, var, week, stub_header) {

  # Get the summary statistics
  df <- data %>%
    # Filter to analsis week
    filter(AVISITCD==week) %>%
    # Summary statistics
    group_by(TRTPCD) %>%
    summarize(n = n(),
              mean = mean({{var}}),
              sd = sd({{var}}),
              median = median({{var}}),
              min = min({{var}}),
              max = max({{var}})) %>%
    # Form into display strings
    mutate(
      N = num_fmt(n, size=12, int_len=2),
      mean_sd = as.character(
        glue('{num_fmt(mean, digits=1, int_len=2, size=4)} ({num_fmt(sd, digits=2, int_len=2, size=4)})')
      ),
      med_ran = as.character(
        glue('{num_fmt(median, digits=1, int_len=2, size=4)} ({num_fmt(min, int_len=3, size=3)};{num_fmt(max, int_len=2, size=2)})')
      )
    ) %>%
    # Drop the numeric values
    select(-n, -mean, -sd, -median, -min, -max) %>%
    # Make summary stats vertical
    pivot_longer(c(N, mean_sd, med_ran), names_to = "rowlbl1") %>%
    # Split out treatment groups into separate columns
    pivot_wider(names_from=TRTPCD, values_from=value) %>%
    # Fix the row labels
    mutate(rowlbl1 = case_when(
      rowlbl1 == 'N' ~ '  n',
      rowlbl1 == 'mean_sd' ~ '  Mean (SD)',
      rowlbl1 == 'med_ran' ~ '  Median (Range)',
    ))

  # Add in the stub header
  bind_rows(tibble(rowlbl1=c(stub_header)), df)
}


efficacy_models <- function(data, var=NULL, wk=NULL, model_type='ancova') {

  if (model_type == 'ancova') {
    # Need to set contrasts to work for Type III SS. See analysis results metadata for
    # table 14-3.01. Reference for R here: https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/
    op <- options(contrasts = c("contr.sum","contr.poly"))

    # Subset to analyze
    data <- data %>%
      filter(AVISITCD == as.character(glue('Wk{wk}')))
  }

  # Create an ordered factor variable for the models
  data['TRTPCD_F'] <- factor(data$TRTPCD, levels=c('Xan_Hi', 'Xan_Lo', 'Pbo'))
  data['AWEEKC'] = factor(data$AWEEK)


  # Set up the models
  if (model_type == 'ancova') {
    if (var == "CHG") {
      model1 <- lm(CHG ~ TRTDOSE + SITEGRP + BASE, data=data)
      model2 <- lm(CHG ~ TRTPCD_F + SITEGRP + BASE, data=data)
    } else {
      model1 <- lm(VAL ~ TRTDOSE + SITEGRP, data=data)
      model2 <- lm(VAL ~ TRTPCD_F + SITEGRP, data=data)
    }
  } else {
    model2 <- lme4::lmer(CHG ~ TRTPCD_F + SITEGRP + AWEEKC + TRTPCD_F:AWEEKC + BASE + BASE:AWEEKC + (AWEEK | USUBJID),
                         data=data)
  }

  ## Dose Response ---
  # NOTE: For statistics portions, I purposefully did not import the libraries to make it explicitly clear which
  # packages were being used to match P-values.
  # Use `car` package Anova test with type III SS.

  if (model_type == 'ancova') {
    ancova <- car::Anova(model1, type=3)

    # Pull it out into a table
    sect1 <- tibble(rowlbl1=c('p-value(Dose Response) [1] [2]'),
                            Xan_Hi = c(num_fmt(ancova[2, 'Pr(>F)'], int_len=4, digits=3, size=12))
    ) %>%
      pad_row()
  }

  ## Pairwise Comparisons ----
  # Here's a reference for the emmeans package and how to use it:
  #   https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html
  # Adjustments made are in line with the analysis results metadata in the analysis define
  # and PROC GLM documentation.

  # Linear model but use treatment group as a factor now

  if (model_type == 'ancova') {
    # LS Means and weight proportionately to match OM option on PROC GLM in SAS
    lsm <- emmeans::lsmeans(model2, ~TRTPCD_F, weights='proportional')
  } else { # Mixed model LS means values - Get the dataset from here
    # See analysis results metadata in analysis define (ARM-Leaf0046) for details on implementation in SAS
    # These numbers end up being slightly off, mostly in the P-values. To anyone that finds this and can match the numbers,
    # please feel free to submit a PR and correct our implementation!
    # To the best of my knowledge, I've matched what I could here. It's not explicit, but the default
    # covariance structure in the lme4 package in unstructured
    lsm <- emmeans::lsmeans(model2, ~TRTPCD_F, lmer.df='kenward-roger')
    # Build the section 1 data here instead of above because its from the same model
    sect1 <- as_tibble(lsm) %>%
      mutate(
        rowlbl1 = 'LS Means (SE)',
        values = as.character(glue('{num_fmt(lsmean, int_len=1, digits=1, size=3)} ({num_fmt(SE, int_len=1, digits=2, size=4)})'))
      ) %>%
      select(rowlbl1, TRTPCD_F, values) %>%
      pivot_wider(id_cols = rowlbl1, names_from=TRTPCD_F, values_from=values) %>%
      pad_row()
  }

  # Here on out - it's all the same data manipulation
  # Get pairwise contrast and remove P-values adjustment for multiple groups
  cntrst_p <- emmeans::contrast(lsm, method="pairwise", adjust=NULL)
  # 95% CI
  cntrst_ci <- confint(cntrst_p)

  # merge and convert into dataframe
  pw_data <- as_tibble(summary(cntrst_p)) %>%
    merge(as_tibble(cntrst_ci)) %>%
    # Create the display strings
    mutate(
      p = num_fmt(p.value, int_len=4, digits=3, size=12),
      diff_se = as.character(
        glue('{num_fmt(estimate, int_len=2, digits=1, size=4)} ({num_fmt(SE, int_len=1, digits=2, size=4)})')
      ),
      ci = as.character(
        glue('({num_fmt(lower.CL, int_len=2, digits=1, size=4)};{num_fmt(upper.CL, int_len=1, digits=1, size=3)})')
      )
    ) %>%
    # Clean out the numeric variables
    select(contrast, p, diff_se, ci) %>%
    # Transpose
    pivot_longer(c('p', 'diff_se', 'ci'), names_to = 'row_label')

  # Subset Xan_Lo - Pbo into table variables
  xan_lo <- pw_data %>%
    filter(contrast == 'Xan_Lo - Pbo') %>%
    # Rename to the table display variable
    select(Xan_Lo=value) %>%
    pad_row()
  # Add in rowlbl
  xan_lo['rowlbl1'] <- c('p-value(Xan - Placebo) [1] [3]', '  Diff of LS Means (SE)', '  95% CI', '')

  # Subset Xan_hi - Pbo into table variables
  xan_hi <- pw_data %>%
    filter(contrast == 'Xan_Hi - Pbo') %>%
    # Rename to the table display variable
    select(Xan_Hi=value) %>%
    pad_row()
  # Add in rowlbl
  xan_hi['rowlbl1'] <- c('p-value(Xan - Placebo) [1] [3]', '  Diff of LS Means (SE)', '  95% CI', '')
  xan_hi['ord'] <- c(1,2,3,4) # Order for sorting

  # Subset Xan_Hi - Xan_Lo into table variable
  xan_xan <- pw_data %>%
    filter(contrast == 'Xan_Hi - Xan_Lo') %>%
    # Rename to the table display variable
    select(Xan_Hi=value)
  # Add in rowlbl
  xan_xan['rowlbl1'] <- c('p-value(Xan High - Xan Low) [1] [3]', '  Diff of LS Means (SE)', '  95% CI')
  xan_xan['ord'] <- c(5,6,7) # Order for sorting

  # Pack it all together
  pw_final <- merge(xan_lo, xan_hi, by='rowlbl1') %>%
    bind_rows(xan_xan) %>%
    arrange(ord)

  # Return the statistics together
  return(bind_rows(sect1, pw_final))

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
