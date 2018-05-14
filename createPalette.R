# Palette picker
# From: https://dreamrs.github.io/shinyWidgets/articles/palette_picker.html#how-it-works

# List of palettes
colors_pal <- lapply(
  X = split(
    x = brewer.pal.info,
    f = factor(brewer.pal.info$category, labels = c("Diverging", "Qualitative", "Sequential"))
  ),
  FUN = rownames
)

get_brewer_name <- function(name) {
  pals <- brewer.pal.info[rownames(brewer.pal.info) %in% name, ]
  res <- lapply(
    X = seq_len(nrow(pals)),
    FUN = function(i) {
      brewer.pal(n = pals$maxcolors[i], name = rownames(pals)[i])
    }
  )
  unlist(res)
}

background_pals <- sapply(unlist(colors_pal, use.names = FALSE), get_brewer_name)
head(background_pals, 3)

# Calc linear gradient for CSS
linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}

background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))
head(background_pals, 3)

colortext_pals <- rep(c("white", "black", "black"), times = sapply(colors_pal, length))

palettes <- list()

# remove diverging
colors_pal$Diverging <- NULL

palettes$colors_pal <- colors_pal

palettes$colortext_pals <- colortext_pals[brewer.pal.info$category!="div"]
palettes$background_pals <- background_pals[brewer.pal.info$category!="div"]


saveRDS(object = palettes, file = "palettes.rds")

# pickerInput(
#   inputId = "col_palette", label = "Choose a palette :",
#   choices = colors_pal, selected = "Paired", width = "50%",
#   choicesOpt = list(
#     content = sprintf(
#       "<div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>",
#       unname(background_pals), colortext_pals, names(background_pals)
#     )
#   )
# )