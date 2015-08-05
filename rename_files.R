setwd("C://Users/ALBERT/Documents/R/data2/Negative")
startingDir <- "C://Users/ALBERT/Documents/R/data2/Negative"
for (i in 1:7) {
        ptn1 <- paste("comb_",i,sep="")
        ptn11 <- paste("Negative_", ptn1, sep="")
        ptn1
        ptn11
        filez <- list.files(startingDir, pattern=ptn1)
        head(filez)
        sapply(filez, FUN=function(eachPath) {
                file.rename(from = eachPath,
                            to = sub(pattern=ptn1,
                                     replacement=ptn11,
                                     eachPath))
        })
        filez2 <- list.files(startingDir, pattern=ptn1)
        head(filez2)
}
setwd("C://Users/ALBERT/Documents/R/data2/Positive")
startingDir <- "C://Users/ALBERT/Documents/R/data2/Positive"
for (i in 1:7) {
        ptn1 <- paste("comb_",i,sep="")
        ptn11 <- paste("Positive_", ptn1, sep="")
        ptn1
        ptn11
        filez <- list.files(startingDir, pattern=ptn1)
        head(filez)
        sapply(filez, FUN=function(eachPath) {
                file.rename(from = eachPath,
                            to = sub(pattern=ptn1,
                                     replacement=ptn11,
                                     eachPath))
        })
        filez2 <- list.files(startingDir, pattern=ptn1)
        head(filez2)
}