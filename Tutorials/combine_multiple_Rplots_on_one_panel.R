# Combine multiple plots on one panel

# use par() to arrange multiple plots together. Here, it is 4 plots (2 rows and 2 columns)
par(mfrow = c(2, 2)) 
# add the plot itself
plot(rnorm(10), rnorm(10))
# add the Label on top left
mtext("A", 2, adj=1, line=2, las = 1, padj = -4, font = 2)
# Note : probably you have to play around with the padj parameter. You could e.g. first try -4 as here,
# and if you don't see the label, try 0 and then adjust.

# add the other 3 plots : 
plot(rnorm(10), rnorm(10))
mtext("B", 2, adj=1, line=2, las = 1, padj = -4, font = 2)
plot(rnorm(10), rnorm(10))
mtext("C", 2, adj=1, line=2, las = 1, padj = -4, font = 2)
plot(rnorm(10), rnorm(10))
mtext("D", 2, adj=1, line=2, las = 1, padj = -4, font = 2)


