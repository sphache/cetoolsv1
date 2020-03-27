#' Creates a Participant Demographics Table in Word
#'
#' @param data_frame data frame with a grouping variable called group
#' @param filename path and filename
#' @examples
#'CreateDescriptivesTable(df, "TableDescriptives.docx")
#'

CreateDescriptivesTable <- function(df,
                                    filename="TableDescriptives.docx") {

library(tidyverse)
library(reshape2)

group <- df$group
index.group1 <- which(group == levels(group)[1])
index.group2 <- which(group == levels(group)[2])

df.y <- df %>%
  select_if(is.numeric)

n.var <- dim(df.y)[2]

# compute descriptiveas -------------------------------------------------------------------

df.descriptives <- data.frame(
  mean.group1 = apply(df.y[index.group1,], 2, mean),
  sd.group1 = apply(df.y[index.group1,], 2, sd),
  min.group1 = apply(df.y[index.group1,], 2, min),
  max.group1 = apply(df.y[index.group1,], 2, max),
  mean.group2 = apply(df.y[index.group2,], 2, mean),
  sd.group2 = apply(df.y[index.group2,], 2, sd),
  min.group2 = apply(df.y[index.group2,], 2, min),
  max.group2 = apply(df.y[index.group2,], 2, max)
)

# compute t test and p value -------------------------------------------------------------------

df.t <- df.y %>%
  summarise_each(list(~t.test(.[group == levels(group)[1]], .[group == levels(group)[2]])$statistic), 1:dim(.)[2]) %>%
  melt %>%
  rename(t = value) %>%
  transmute(t = unname(t))

df.p <- df.y %>%
  summarise_each(list(~t.test(.[group == levels(group)[1]], .[group == levels(group)[2]])$p.value), 1:dim(.)[2]) %>%
  melt %>%
  select(-variable) %>%
  rename(p = value)

# concatenate data into one data frame -------------------------------------------------------------------

df.descriptives <- cbind(df.descriptives, df.t, df.p)
df.descriptives$variable <- rownames(df.descriptives)

df.out <- df.descriptives %>%
  select(variable, everything()) %>% # move variable to the front
  mutate_if(is.numeric, funs(round(., digits=2))) # round all numerical variables

# create table -------------------------------------------------------------------

library(flextable)

extrafont::loadfonts(quiet = T)

# create column keys where "b" stands for "blank"
col_keys <- names(df.out) %>%
  append("b1", after = 1) %>%
  append("b2", after = 6) %>%
  append("b3", after = 11)

# create flextable
tb <- flextable(df.out,
                col_keys = col_keys
                #theme_fun = theme_booktabs
)

# Set the initial labels above the data
tb <- set_header_labels(tb,
                        variable = "Variable",
                        b1 = "",
                        mean.group1 = "M",
                        sd.group1 = "SD",
                        min.group1 = "Min",
                        max.group1 = "Max",
                        b2 = "",
                        mean.group2 = "M",
                        sd.group2 = "SD",
                        min.group2 = "Min",
                        max.group2 = "Max",
                        b3 = "",
                        t = "t statistic",
                        p = "p value")

# Add a secondary row of labels
tb <- add_header(tb,
                 variable = "Variable",
                 b1 = "",
                 mean.group1 = levels(group)[1],
                 sd.group1 = levels(group)[1],
                 min.group1 = levels(group)[1],
                 max.group1 = levels(group)[1],
                 b2 = "",
                 mean.group2 = levels(group)[2],
                 sd.group2 = levels(group)[2],
                 min.group2 = levels(group)[2],
                 max.group2 = levels(group)[2],
                 b3 = "",
                 t = "Significance",
                 p = "Significance",
                 top = T )

# Add header and footer
tb <- add_header_lines(tb, values = "Table x. blah ...")
tb <- add_footer_lines(tb, values = "Note. blah ... ")

# Create Table

ft <- tb %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  empty_blanks() %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "all") %>%
  align_text_col(align = "left", header = TRUE, footer = TRUE) %>% # align header and footer to left
  width(j = ~ variable, width = 1.2) %>%
  autofit()


# format lines -------------------------------------------------------------------

library(officer)

big_b <- fp_border(color="black", width = 1)

ft <- ft %>%
  border_remove() %>%
  hline(i = 1, j = 1, border = big_b, part = "header") %>% # adds line below header
  hline(i = 2, j = 3:length(ft$col_keys), border = big_b, part = "header") %>%  # adds line below header
  hline_top(j = 1, border = big_b, part = "footer") %>% # adds line on top of header
  hline_top(j = 1:length(ft$col_keys), border = big_b, part = "body") # adds line above body

# save table -------------------------------------------------------------------

doc <- read_docx()
doc <- body_add_flextable(doc, value = ft, align = "center")
print(doc, target=filename)

}
