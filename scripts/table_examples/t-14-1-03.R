
library(plyr)
library(dplyr)
library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(huxtable)
library(pharmaRTF)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

# Read in the datasets
adsl <- read_xpt(glue("{adam_lib}/adsl.xpt"))

adsl_grp <- as.data.frame(adsl %>%
  group_by(SITEGRP, SITEID, TRTP, ITT, EFFICACY, COMPLT24) %>%
  summarise(n = n()))

df <- ddply(.data = adsl_grp, .variables = "SITEID", .fun = function(x) {
  siteid_i <- unique(x[, "SITEID"])
  sitegrp_i <- unique(x[, "SITEGRP"])
  data.frame(
    SITEGRP = sitegrp_i,
    PlaITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                        adsl_grp$TRTP == "Placebo" &
                        adsl_grp$ITT == "Y"), "n"]),
    PlaEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                         adsl_grp$TRTP == "Placebo" &
                         adsl_grp$EFFICACY == "Y"), "n"]),
    PlaCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                         adsl_grp$TRTP == "Placebo" &
                         adsl_grp$COMPLT24 == "Y"), "n"]),
    XanLowITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                             adsl_grp$TRTP == "Xanomeline Low Dose" &
                             adsl_grp$ITT == "Y"), "n"]),
    XanLowEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                             adsl_grp$TRTP == "Xanomeline Low Dose" &
                             adsl_grp$EFFICACY == "Y"), "n"]),
    XanLowCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                             adsl_grp$TRTP == "Xanomeline Low Dose" &
                             adsl_grp$COMPLT24 == "Y"), "n"]),
    XanHighITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                              adsl_grp$TRTP == "Xanomeline High Dose" &
                              adsl_grp$ITT == "Y"), "n"]),
    XanHighEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                              adsl_grp$TRTP == "Xanomeline High Dose" &
                              adsl_grp$EFFICACY == "Y"), "n"]),
    XanHighCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                              adsl_grp$TRTP == "Xanomeline High Dose" &
                              adsl_grp$COMPLT24 == "Y"), "n"]),
    TotITT = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                          adsl_grp$ITT == "Y"), "n"]),
    TotEff = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                          adsl_grp$EFFICACY == "Y"), "n"]),
    TotCom = sum(adsl_grp[(adsl_grp$SITEID == siteid_i &
                          adsl_grp$COMPLT24 == "Y"), "n"]),
    check.rows = FALSE, stringsAsFactors = FALSE
  )
}, .inform = TRUE)
df[,c(1,2)] <- df[,c(2,1)]
#sort by siteid
df[1:nrow(df),] <- df[sort(df[,2], index.return = TRUE)$ix,]
#then by grp
df[1:nrow(df),] <- df[sort(df[,1], index.return = TRUE)$ix, ]

df[nrow(df) + 1,] <- c(
  "Total",
  "",
  unname(apply(df[,3:ncol(df)], 2, sum))
)

names(df) <- c(
  "Pooled\\line Id",
  "Site\\line Id",
  rep(c("ITT", "Eff", "Com"), 4)
)

df[2:(nrow(df) + 1),] <- df[1:nrow(df),]
df[1,] <- names(df)
df <- df %>%
  add_row("Pooled\\line Id" = "", .before = 1) %>%
  add_row("Pooled\\line Id" = "", .before = 1)


### Add Headers
headers <- adsl %>%
  group_by(ARM) %>%
  summarise(N = n()) %>%
  mutate(labels = str_replace_all(str_wrap(glue('{ARM} (N={N})'), width=10), "\n", function(x) "\\line "))
headers[4,] <- list(
  ARM = "Total",
  N = nrow(adsl),
  labels = paste0("Total\\line(N=", nrow(adsl), ")")
)

df[1, 3] <- headers[1, "labels"]
df[1, 6] <- headers[2, "labels"]
df[1, 9] <- headers[3, "labels"]
df[1, 12] <- headers[4, "labels"]

ht <- df %>%
  huxtable::as_hux(add_colnames=FALSE) %>%
  merge_cells(1, 3:5) %>%
  set_bottom_border(2, 3:5, 1) %>%
  merge_cells(1, 6:8) %>%
  set_bottom_border(2, 6:8, 1) %>%
  merge_cells(1, 9:11) %>%
  set_bottom_border(2, 9:11, 1) %>%
  merge_cells(1, 12:14) %>%
  set_bottom_border(2, 12:14, 1) %>%
  set_escape_contents(FALSE) %>%
  set_width(1.1) %>%
  set_bottom_border(3, 1:14, 1) %>%
  huxtable::set_align(3, 1:14, "center") %>%
  huxtable::set_valign(3, 1:14, "bottom") %>%
  huxtable::set_align(4:nrow(ht), 3:ncol(ht), "right") %>%
  huxtable::set_align(1:nrow(ht), 1:2, "center") %>%
  huxtable::set_align(1, 1:14, "center") %>%
  huxtable::set_valign(1, 1:14, "bottom") %>%
  huxtable::set_col_width(1:ncol(ht), value = c(0.1, 0.1, rep(0.07, 12)))

doc <- rtf_doc(ht, header_rows = 2) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-1.03') %>%
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top = 1)

write_rtf(doc, file='./scripts/table_examples/outputs/14-1.03.rtf')

