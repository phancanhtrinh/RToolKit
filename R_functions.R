####Extract the data from venn list; see the venn list example below
# x <- list(
#   upRNA = upRNA$GeneName, 
#   upProt = upProt$GeneName, 
#   downRNA = downRNA$GeneName,
#   downProt = downProt$GeneName
#   )

Intersect <- function (x) {  
  # Multiple set version of intersect
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

Union <- function (x) {  
  # Multiple set version of union
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}

Setdiff <- function (x, y) {
  # Remove the union of the y's from the common x's. 
  # x and y are lists of characters.
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}

####Clean up theme from Michi
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(color = "black", lineend = "square", size =0.7),
                plot.margin = margin(0.5, 0.5, 0.5, 2, "cm"),
                axis.ticks = element_line(size = 0.7, lineend = "square", color = "black"),
                axis.ticks.length = unit(.1,"cm"),
                axis.title.x = element_text(color = "black", face = "bold", size = fsize, vjust = -2),
                axis.title.y = element_text(color = "black", face = "bold", size = fsize, vjust = 5),
                axis.text.x = element_text(color = "black", size = fsize*0.75, face = NULL, 
                                           hjust = NULL, vjust = NULL, angle = NULL,
                                           margin = margin(5,0,0,0,"pt")),
                axis.text.y = element_text(color = "black", size = fsize*0.75,
                                           margin = margin(0,5,0,0,"pt")),
                legend.background = element_blank(),
                legend.key = element_blank(),
                legend.title = element_text(color = "black", face = "bold", size = fsize),
                legend.text = element_text(color = "black", size = fsize*0.9),
                plot.title = element_text(size = fsize*1.25),
                legend.position = "right")