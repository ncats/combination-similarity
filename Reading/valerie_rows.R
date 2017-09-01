valerie_data <- read_excel("~/Sarita/Valerie_data.xlsx",
                           col_names = FALSE)

dim(Valerie_data)

valerie_data_all <- valerie_data
min(valerie_data_all)

negative1 <- valerie_data_all$X0 < 0
negative2 <- valerie_data_all$X1 < 0 

valerie_data_all$n1 <- negative1
valerie_data_all$n2 <- negative2

valerie_data_pos <- subset(valerie_data_all, n1==FALSE & n2==FALSE, select=c(X0, X1))
min(valerie_data_pos)


write.table(valerie_data_pos, "valerie_data_pos.txt", sep="\t")


install.packages("xlsx", dependencies = TRUE)
library(xlsx)

install.packages("rJava", dependencies = TRUE)
library(rJava)

write.xlsx



install.packages("WriteXLS", dependencies = TRUE)
library(WriteXLS)

WriteXLS(valerie_data_pos, "valerie_data_pos.xls")

install.packages("r2excel", dependencies = TRUE)

