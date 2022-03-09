library(haven)
library(dplyr)
library(tidyr)
library(gt)
library(tidyverse)


the_date <- as.character(Sys.Date())

# read the data
adsl <- read_sas("C:\\shift\\adsl.sas7bdat")
adlb <- read_sas("C:\\shift\\adlb.sas7bdat")


# get the big N in column headers from adsl
bign <- table(group=adsl$TRT01A)

# get the post-baseline data in ADLB
post <- adlb[(adlb$ADT>adlb$TRTSDT),]

# sort post by USUBJID, PARAM and ATOXGRN
post <- post[order(post$USUBJID,post$PARAM, post$ATOXGRN),]
post$lastdot <- !duplicated(post[c("USUBJID","PARAM")],fromLast=TRUE)

# keep the most severe record for each USUBJID, PARAM
post <- post[(post$lastdot==TRUE),]


# do the counting by TRT01A, PARAM, BTOXGRN, ATOXGRN

count0 <- 
  post %>%                   
  group_by(TRT01A, PARAM, BTOXGRN, ATOXGRN) %>%
  summarise(unique_subj = n_distinct(USUBJID))

# generate a frame data called comb
# it has all the combinations of ATOXGRN, BTOXGRN, PARAM and TRT01A

mat <- matrix(NA, nrow = 25, ncol = 4)
xy <- data.frame(mat)
xy1 <- data.frame(mat)

for (i in seq(1,25)){
     xy[[i,1]] <- data.frame(X1=i%%5)
     xy[[i,2]] <- data.frame(X2=ceiling(i/5)-1)
     }

for (i in seq(1:length(unique(count0$PARAM)))){
  xy$X3 <- unique(count0$PARAM)[i]
  assign(paste0('xy', i, sep=''),  xy)
}

comb <- do.call("rbind", mget(sprintf("xy%d", 1:length(unique(count0$PARAM)))))

for (i in seq(1:length(unique(count0$TRT01A)))){
  comb$X4 <- unique(count0$TRT01A)[i]
  assign(paste0('comb', i, sep=''),  comb)
}

comb <- do.call("rbind", mget(sprintf("comb%d", 1:length(unique(count0$TRT01A)))))

comb$PARAM <- comb$X3
comb$TRT01A <- comb$X4
comb$BTOXGRN <- as.numeric(unlist(comb$X1))
comb$ATOXGRN <- as.numeric(unlist(comb$X2))

# merge comb with count0
m_count0 <- merge(count0, comb, by=c("TRT01A", "PARAM", "BTOXGRN", "ATOXGRN"), all=TRUE)
m_count0$denom <- ifelse(m_count0$TRT01A=='grp1', bign[1], bign[2])
m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$denom, 1), nsmall = 1), ")"))
print(m_count0)

# do the transpose 
count1 <- m_count0[(m_count0$TRT01A=="grp1"),]
count2 <- m_count0[(m_count0$TRT01A=="grp2"),]

a1 <- count1 %>%
 pivot_wider(id_cols=c(PARAM, BTOXGRN), names_from = ATOXGRN, values_from = value,
                  names_prefix = "grade")

a2 <- count2 %>%
  pivot_wider(id_cols=c(PARAM, BTOXGRN), names_from = ATOXGRN, values_from = value,
              names_prefix = "grad")

a1 <- a1[c("PARAM","BTOXGRN","grade0", "grade1", "grade2", "grade3", "grade4" )]
a2 <- a2[c("grad0", "grad1", "grad2", "grad3", "grad4" )]


# combine a1 and a2

df_merge <- cbind(a1,a2)
df_merge$prefix <- 'Grade'
df_merge$BTOXGRC <- paste(df_merge$prefix, df_merge$BTOXGRN)
df <- df_merge[c("PARAM","BTOXGRC","grade0", "grade1", "grade2", "grade3", "grade4","grad0", "grad1", "grad2", "grad3", "grad4"  )]

print(df)


df %>% 
  gt(groupname_col="PARAM") 


# use gt to do the reporting 
tab_html <- df %>% 
  gt(groupname_col="PARAM") %>%
  
  tab_header(
    title = "Table 14.3.4. Shift Table from CTCAE Grade at Baseline to the Most Severe CTCAE Grade at Post-basline",
    subtitle = "Safety Population"
  ) %>%
  
  tab_source_note(
    source_note = paste('Program Source: shift.R            Executed: (Draft)',  the_date)
  ) %>%
  
  cols_label(
    
    
    BTOXGRC = html("Baseline CTCAE Grade"),
    grade0 = html("Grade 0"), 
    grade1 = html("Grade 1"),
    grade2 = html("Grade 2"),
    grade3 = html("Grade 3"),
    grade4 = html("Grade 4"),
    grad0 = html("Grade 0"), 
    grad1 = html("Grade 1"),
    grad2 = html("Grade 2"),
    grad3 = html("Grade 3"),
    grad4 = html("Grade 4")
    
    
  ) %>%
  
  tab_options(
    table.border.top.color = "white",
    heading.border.bottom.color = "black",
    table.border.bottom.color = "white",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "white", 
    row_group.border.bottom.color = "white", 
    row_group.border.top.color = "white", 
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
  ) %>%
  tab_spanner(
    label = html(paste("Group 1 Most Severe Post-baseline <br> (N=", bign[1], ")")),
    columns = c(grade0, grade1, grade2, grade3, grade4)
  ) %>%
  tab_spanner(
    label = html(paste("Group 2 Most Severe Post-baseline <br> (N=", bign[2], ")")),
    columns = c(grad0, grad1, grad2, grad3, grad4)
  ) %>%

cols_align(
  align = "left",
  columns = c(BTOXGRC)
)

# output the HTML table

tab_html %>%
  gtsave("shift.html", path = "C:\\shift" )
















