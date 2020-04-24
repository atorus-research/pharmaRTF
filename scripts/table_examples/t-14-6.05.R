## 14-6.05


library(huxtable)
library(glue)
library(tidyverse, lib.loc = .libPaths()[2])
library(haven)
library(tibble)
library(coin)

source('./scripts/table_examples/config.R')
source('./scripts/table_examples/funcs.R')

pad_row <- function(df, r) {
  #df - dataframe to insert pad
  #r - row number to pad
  for(i in seq(along = r)) {
    if(r[i] + i - 1 < nrow(df)){
      df[seq(r[i] + i, nrow(df) + 1),] <- df[seq(r[i] + (i - 1), nrow(df)),]
      df[r[i] + (i - 1),] <- NA
    } else {
      df[r[i] + (i - 1),] <- NA
    }
  }
  df
}

n_pct <- function(n, pct, n_width=3, pct_width=3) {
  n <- unlist(n)
  pct <- unique(pct)
  # n (%) formatted string. e.g. 50 ( 75%)
  unlist(lapply(n, function(x) {
    if(x == 0) " 0      "
    else {
      as.character(
        # Form the string using glue and format
        glue('{format(x, width=n_width)}({format(round((x/pct) * 100), width=pct_width)}%)')
      )
    }
  }))
}

adlbc <- read_xpt(glue("{adam_lib}/adlbc.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
adlbh <- read_xpt(glue("{adam_lib}/adlbh.xpt")) %>%
  filter(SAFETY == "Y", BLTRFL != "")
comb <- rbind(adlbc, adlbh) %>%
  filter(LBTRMXFL == "Y")

#sort tests
comb$LBTEST <-ordered(comb$LBTEST, c("ALANINE AMINOTRANSFERASE",
                                     "ALBUMIN",
                                     "ALKALINE PHOSPHATASE",
                                     "ASPARTATE AMINOTRANSFERASE",
                                     "BILIRUBIN",
                                     "CALCIUM",
                                     "CHLORIDE",
                                     "CHOLESTEROL",
                                     "CREATINE KINASE",
                                     "CREATININE",
                                     "GAMMA GLUTAMYL TRANSFERASE",
                                     "GLUCOSE",
                                     "PHOSPHATE",
                                     "POTASSIUM",
                                     "PROTEIN",
                                     "SODIUM",
                                     "URATE",
                                     "UREA NITROGEN",
                                     "BASOPHILS",
                                     "EOSINOPHILS",
                                     "ERY. MEAN CORPUSCULAR HB CONCENTRATION",
                                     "ERY. MEAN CORPUSCULAR HEMOGLOBIN",
                                     "ERY. MEAN CORPUSCULAR VOLUME",
                                     "ERYTHROCYTES",
                                     "HEMATOCRIT",
                                     "HEMOGLOBIN",
                                     "LEUKOCYTES",
                                     "LYMPHOCYTES",
                                     "MONOCYTES",
                                     "PLATELET"))
comb$TRTP <- ordered(comb$TRTP, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
comb$BLTRFL <- ordered(comb$BLTRFL, c("N", "H"))
comb$LBTRFL <- ordered(comb$LBTRFL, c("N", "H"))


total_bltrfl1 <- comb%>%
  filter(!is.na(TRTP), !is.na(BLTRFL), !is.na(LBTRFL), !is.na(LBTEST)) %>%
  group_by(LBTEST, TRTP, BLTRFL) %>%
  complete(nesting(TRTP, BLTRFL)) %>%
  summarise(N = n())
total_bltrfl <- total_bltrfl1 %>%
  mutate(LBTRFL = ordered("T", c("T", "N", "H"))) %>%
  pivot_wider(id_cols = c(LBTEST, LBTRFL), names_from = c(TRTP, BLTRFL), values_from = N)

comb2 <- comb %>%
  filter( !is.na(TRTP), !is.na(BLTRFL), !is.na(LBTRFL), !is.na(LBTEST)) %>%
  group_by(LBTEST, TRTP, BLTRFL, LBTRFL) %>%
  complete(nesting(BLTRFL, LBTRFL)) %>%
  summarise(N = n()) %>%
  arrange(LBTEST, BLTRFL, LBTRFL, TRTP)

comb2_test <- comb2 %>%
  filter(LBTEST == "BILIRUBIN")

pvalues <- c()
for(i in seq(nrow(comb2)/12)) {
  arr <- array(
    unlist(comb2[((i-1)*12 + 1):(i * 12), "N"]),
    dim = c(3,2,2)
  )

  if(all(arr[,,2] == 0)) {
    pvalues[i] <- ""
  } else {
    pvalues[i] <- num_fmt(as.numeric(pvalue(cmh_test(as.table(arr)))), int_len = 1, digits = 3, size = 5)
  }

}


comb3 <- comb2 %>%
  mutate(n2 = n_pct(N, total_bltrfl1[total_bltrfl1$LBTEST == LBTEST &
                                       total_bltrfl1$TRTP == TRTP     &
                                       total_bltrfl1$BLTRFL == BLTRFL, "N"], n_width = 2)) %>%
  pivot_wider(id_cols = c(LBTEST, LBTRFL), names_from = c(TRTP, BLTRFL), values_from = n2)

comb4 <- comb3[!apply(comb3, 1, function(x) {
  all(x[4:8] ==  " 0      ") & all(x[2] == "H")
}), ]


comb4$LBTRFL <- ordered(comb4$LBTRFL, c("T", "N", "H"))

total_bltrfl$Placebo_N <- num_fmt(total_bltrfl$Placebo_N, size = 2, int_len = 2)
total_bltrfl$Placebo_H <- num_fmt(total_bltrfl$Placebo_H, size = 2, int_len = 2)
total_bltrfl$`Xanomeline Low Dose_N` <- num_fmt(total_bltrfl$`Xanomeline Low Dose_N`, size = 2, int_len = 2)
total_bltrfl$`Xanomeline Low Dose_H` <- num_fmt(total_bltrfl$`Xanomeline Low Dose_H`, size = 2, int_len = 2)
total_bltrfl$`Xanomeline High Dose_N` <- num_fmt(total_bltrfl$`Xanomeline High Dose_N`, size = 2, int_len = 2)
total_bltrfl$`Xanomeline High Dose_H` <- num_fmt(total_bltrfl$`Xanomeline High Dose_H`, size = 2, int_len = 2)

comb5 <- comb4 %>%
  rbind(total_bltrfl) %>%
  arrange(LBTEST, LBTRFL)

comb5$LBTRFL <- as.character(recode(comb5$LBTRFL,
                                    "T" = "n",
                                    "N" = "Normal",
                                    "H" = "High"))

comb5[unlist(comb5[,2] == "n")[,1], 9] <- pvalues
comb5 <- pad_row(comb5, which(comb5[,2] == "n")) %>%
  ungroup() %>%
  add_row("LBTEST" = NA, .before = 1) %>%
  add_row("LBTEST" = NA, .before = 1)
comb5 <- comb5 %>%
  add_row("LBTEST" = NA, .before = 65) %>%
  add_row("LBTEST" = NA, .before = 65)

comb5[,1] <- as.character(comb5$LBTEST)
comb5[2,1] <- "CHEMISTRY"
comb5[3,1] <- "----------"
comb5[66,1] <- "HEMATOLOGY"
comb5[67,1] <- "----------"

comb5[!(unlist(comb5[,2]) %in% "n") , 1] <- NA

names(comb5) <- c(
  "",
  "Shift\\line[1]",
  "Normal at Baseline",
  "High at Baseline",
  "Normal at Baseline",
  "High at Baseline",
  "Normal at Baseline",
  "High at Baseline",
  "p-\\line value\\line[2]"
)

comb5 <- comb5[!apply(comb5, 1, function(x) {
  all(x[4:9] ==  " 0      ") & all(x[3] == "High")
}), ]


dm <- read_xpt(glue("{sdtm_lib}/dm.xpt"))
headers <- dm %>%
  filter(ARM != "Screen Failure") %>%
  group_by(ARM) %>%
  summarise(N = n()) %>%
  mutate(label = paste0(recode(ARM,
                               "Placebo" = "Placebo",
                               "Xanomeline Low Dose" = "Xan. Low",
                               "Xanomeline High Dose" = "Xan. High"), " (N=", N, ")"))

ht <- comb5 %>%
  huxtable::as_hux(add_colnames=TRUE)

ht <- pad_row(ht, c(1,1))
ht[1, 3] <- headers[1, "label"]
ht[1, 5] <- headers[2, "label"]
ht[1, 7] <- headers[3, "label"]

ht2 <- ht %>%
  huxtable::merge_cells(1, 3:4) %>%
  huxtable::merge_cells(1, 5:6) %>%
  huxtable::merge_cells(1, 7:8) %>%
  huxtable::set_bottom_border(2, 3:4, 1) %>%
  huxtable::set_bottom_border(2, 5:6, 1) %>%
  huxtable::set_bottom_border(2, 7:8, 1) %>%
  huxtable::set_bottom_border(3, 1:9, 1) %>%
  huxtable::set_width(1.5) %>%
  huxtable::set_escape_contents(FALSE) %>%
  huxtable::set_bold(1:3, 1:9, TRUE) %>%
  huxtable::set_valign(1:3, 1:9, "bottom") %>%
  huxtable::set_align(3, 1:9, "center") %>%
  huxtable::set_align(1, 1:9, "center") %>%
  huxtable::set_align(4:102, 9, "right") %>%
  huxtable::set_col_width(1:9, c(0.25, rep(0.09, 7), 0.06))


# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht2, header_rows = 3) %>% titles_and_footnotes_from_df(
  from.file='./scripts/table_examples/titles.xlsx',
  reader=example_custom_reader,
  table_number='14-6.05') %>%
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top = 1) %>%
  set_header_height(0.75) %>%
  set_footer_height(1)


# Write out the RTF
write_rtf(doc, file='./scripts/table_examples/outputs/14-6.05.rtf')

