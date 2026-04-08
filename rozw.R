require(stats); require(graphics)
library(dplyr)
savings <- LifeCycleSavings
summary(savings)
# hist(data$sr,breaks=10)

plot_numerical <- function() {
  par(mfrow = c(2, 3))
  for (col in names(savings)) {
    hist(savings[[col]], breaks = 15, main = paste("Histogram of", col), xlab = col)

  }
  par(mfrow = c(1, 1))
}

plot_numerical()

plot_scatters <- function() {
  par(mfrow = c(2, 2))
  for (col in names(savings %>% select(-sr)))
  {
    plot(savings[[col]], savings$sr, main = paste('Regression of sr in comp to', col), xlab = col)
    abline(lm(sr ~ savings[[col]], data = savings), col = 'red', lwd = 2)
  }
  par(mfrow = c(1, 1))
}
plot_scatters()

M <- cor(savings)
library(corrplot)
corrplot(M, method = "number", type = "upper", tl.col = "black", tl.srt = 45)


plot_pops_comp <- function() {
  plot(savings$pop75, savings$pop15, main = paste('Regression of pop15 in comp to pop75'), xlab = "% of people over 75", ylab = "% of people under 15")
  abline(lm(savings$pop15 ~ savings$pop75, data = savings), col = 'red', lwd = 2)
}

plot_pops_comp()

size <- LifeCycleSavings$sr/5
plot(LifeCycleSavings$pop15, LifeCycleSavings$dpi,
     ylab = 'real per-capita disposable income',
     xlab = '% of people under 15',
     main = 'Comparision beetween % of under 15 with RPDI ',
     pch = 19,
     col = rgb(0.7, 0.5, 0.2, alpha = 0.6),
     cex = size)






savings$is_young <- ifelse(savings$pop15>median(savings$pop15), "Young population", "Old population")
savings$is_rich <- ifelse(savings$dpi>median(savings$dpi),"Rich country", "Poor country")
savings$has_saving <- ifelse(savings$sr>median(savings$sr), "High savings","Low savings" )

mosaicplot(~+is_rich+has_saving+is_young,data = savings,
           main = "Comparision of age, saving and RPDI",
           color = c("steelblue","coral"),
           las=1)


boxplot(ddpi ~ has_saving, data = savings,
        main="Influence of growth in dpi to savings",
        xlab = "Has saving above median?",
        ylab = "Ddpi"

        )