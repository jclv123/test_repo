# Import libraries necessary creating pheatmaps
library(RColorBrewer)
library(viridis)
library(pheatmap)
library(ggplot2)
# Set the seed 
set.seed(42)
random_string <- function(n) {
  substr(paste(sample(letters), collapse = ""), 1, n)
}

# Generate a random matrix
mat <- matrix(rgamma(1000, shape = 1) * 5, ncol = 50)

# Assign column names to matrix
colnames(mat) <- paste(
  rep(1:3, each = ncol(mat) / 3),
  replicate(ncol(mat), random_string(5)),
  sep = ""
)
rownames(mat) <- replicate(nrow(mat), random_string(3))

# Assign groups
col_groups <- substr(colnames(mat), 1, 1)
table(col_groups)
group2 <- c(rep("a", 10), rep("b", 10), rep("c", 10), rep("d", 10), rep("e", 10))
table(group2)

mat[,group2 == "a"] <- mat[,group2 == "a"] * 5
mat[,group2 == "d"] <- mat[,group2 == "d"] * 3


# Set the theme for all the following plots.
theme_set(theme_bw(base_size = 16))

dat <- data.frame(values = as.numeric(mat))
ggplot(dat, aes(values)) + geom_density(bw = "SJ")

# Data frame with column annotations.
mat_col <- data.frame(number = col_groups,
                      letter = group2)
rownames(mat_col) <- colnames(mat)

# List with colors for each annotation.
mat_colors <- list(number = brewer.pal(3, "Set1"),
                   letter = brewer.pal(5, "Set2"))
names(mat_colors$number) <- unique(col_groups)
names(mat_colors$letter) <- unique(group2)

# Generate Heatmap
pheatmap(
  mat               = mat,
  color             = inferno(10),
  border_color      = NA,
  show_colnames     = FALSE,
  show_rownames     = FALSE,
  annotation_col    = mat_col,
  annotation_colors = mat_colors,
  drop_levels       = TRUE,
  fontsize          = 14,
  main              = "Default Heatmap"
)
