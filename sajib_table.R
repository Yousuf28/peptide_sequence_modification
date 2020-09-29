
# function for making all possible value
make_table <- function(small_peptide, long_peptide) {
    small_peptide <- unlist(strsplit(small_peptide, ""))
    final_table <- matrix(nrow = length(small_peptide), ncol = length(long_peptide))
    column_name <- list()
    for (i in seq_along(small_peptide)) {
        for (j in seq_along(long_peptide)) {
            replace_value <- replace(small_peptide, i , long_peptide[j])
            final_table[i,j] <- paste(replace_value, collapse = "")
            column_name_string <- replace(small_peptide, i, "_")
            column_name[i] <- paste(column_name_string, collapse = "")
        }
    }
    final_table <- t(final_table)
    final_table <- as.data.frame(final_table)
    colnames(final_table) <- column_name
    final_table
}

############################################################################################
# change here 
small_peptide <- c("ABCDEFGH") 

# change here
long_peptide <- c("i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z") 

############################################################################################


# call function
table_csv <- make_table(small_peptide, long_peptide)

# save table as csv in current working directory
write.csv(table_csv, file = "table.csv")
