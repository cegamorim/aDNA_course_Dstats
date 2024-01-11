---
title: "plots"
author: "Cruz-Dávalos, Diana I."
---

# The function "clean_my_input" will remove the first few lines of the Dstat output file
# and will leave only the relevant lines with results.
# Select all the lines from 10 to 29 with your mouse and press the run button on the top right of this panel ^
 
clean_my_input <- function(original_input, clean_file){
  
  lines_to_skip <- as.numeric(
    system(
      paste0("grep -n result: ", original_input, " | cut -f1 -d:"),
      intern = T)
  )[1] 
  
  message(paste0(lines_to_skip - 1, " first lines will be removed"))

  # remove first lines and lines with no data
  system(
    paste0(
      "grep -v 'no data' ", original_input, 
      "| tail -n +", lines_to_skip,"> ", clean_file
    )
  )
  message(paste0(original_input, " was cleaned and saved as ", clean_file))
  
}

# install ggplot2 if you need

install.packages("ggplot2")

# load ggplot library

library(ggplot2)

# Dstat

input_dstat <- "subset3ancient.Dstat.out"
clean_dstat <- paste0(input_dstat, ".clean")
clean_my_input(original_input = input_dstat, clean_file = clean_dstat)

dstat <- read.table(
  clean_dstat,
  sep = "", 
  colClasses = c(rep("NULL", 1),
                 rep("character", 4), # population names
                 rep("numeric", 5) # values
  ),
  header = F,
  col.names = c("", "Pop1",  "Pop2",  "Pop3",  "Pop4",  
                "Dstat",	"Z",	"BABA",	"ABBA",	"SNPs"),
  stringsAsFactors = F
)

#-----------------------------------------------------------------------------#
# Visualize the values of D

x_label <- expression(
  atop(
    "D",
    paste("(", Pop[2], ", ", "Mixe", ") < --- > (",Pop[2], ", ", Pop[4], ")")
  )
)

# a nice title
my_title <- expression(
  bold(
    paste(
      "D(Yoruba, ", Pop[2], "; ", Pop[3]," = Mixe, ", Pop[4],")"
      )
  )
)

ggplot(dstat, aes(x = Dstat, y = Pop4, color = Pop2)) +
  geom_point(shape = 1) +
  labs(
    x = x_label,
    y = expression(Pop[4]),
    title = my_title)

#-----------------------------------------------------------------------------#
# Print table with D-stat, Z-score, ABBA, BABA
write.table(dstat,"dstat.tab", append = FALSE, sep = "\t", dec = ".",
            row.names = TRUE, col.names = TRUE,quote=FALSE)
