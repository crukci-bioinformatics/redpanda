prismStripChart <- function(tab, errorBars = "std.dev") {
  
  lwd = 2
  stripchart(tab, xaxt = "n", vertical = TRUE, method = "jitter", jitter = 0.1, pch = 19, frame.plot = FALSE, las = 1, yaxs = "i")
  axis(side = 1, labels = FALSE, line = 0)
  text(x = 1:ncol(tab), y = -0.15 * diff(range(tab, na.rm = TRUE)), labels = colnames(tab), srt = 45, xpd = TRUE)
  
  meanPoints <- apply(tab, 2, mean, na.rm = TRUE)
  #sds <- apply(tab, 2, sd, na.rm = TRUE)
  errorLimits <- switch(errorBars,
                        std.dev = std.dev(t(tab)),
                        std.errors = std.errors(t(tab)),
                        conf.intervals = conf.intervals(t(tab)),
                        none = rep(0, ncol(tab)))
  
  #mean lines
  x0 <- (1:ncol(tab))-0.3
  x1 <- (1:ncol(tab))+0.3
  segments(x0 = x0, y0 = meanPoints, x1 = x1, lwd = lwd)
  
  if(errorBars != "none") {
    arrows(1:ncol(tab), meanPoints - errorLimits, 1:ncol(tab), meanPoints + errorLimits, angle = 90, code = 3, length = 0.2, lwd = lwd)
  }
}