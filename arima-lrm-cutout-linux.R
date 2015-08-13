## Best fit while t1=0 and t2=0 ##
## 1-(12,0,4); 2-(16,0,6); 3-(12,0,4); 4-(14,0,6); 5-(16,0,4); 6-(12,0,2); 7-(16,0,2) ##
seekarima <- function (workDir = "/home/albert/simplifiedthesis/data", 
                       Num=4, K=3, AR=12, MA=4, t1=0, t2=0) {
        pattern <- paste("comb_", Num, sep = "")
        filez <- list.files(path = workDir, pattern = pattern, full.names = TRUE)
        Dat <- read.csv(filez[K])
        Dat <- Dat[which(Dat[,2] < 9.999 & Dat[,2] > -9.999), ]
        Dat <- ts(Dat[,2])
        totalN <- length(Dat)
        Dat <- Dat[(t1):(totalN - t2)]
        ## IMPORTANT NOTICE: DO NOT CHANGE THE FOLLOWING ONE LINE OF CODE 
        Dat.fit <- arima(diff(Dat), order =c(AR,0,MA), optim.method="Nelder-Mead")
        return(Dat.fit)
}

## if (with Positive) faultSTATUS == 1
faultstatus <- function ( workDir = "/home/albert/simplifiedthesis/data", Num = 4, K = 3) {
        pattern <- paste("comb_", Num, sep = "")
        filez <- list.files(path = workDir, pattern = pattern, full.names = TRUE)
        faultSTATUS <- length(grep("Positive", (filez[K]))) 
        return(faultSTATUS)
}

studyCutout <- function (workDir =  "/home/albert/simplifiedthesis/data",
                         AR = 12,
                         MA = 4,
                         Num = 1,
                         t1 = 0,
                         t2 = 0 ) 
{

startTime <- Sys.time()
Wt1 <- paste("Program Started @",format(Sys.time(),"%d%B%Y %H:%M:%S"))
print(Wt1)
        
## INITIALIZE DATA HOLDER OF MODEL PARAMETERS
f_para <- data.frame()
## CURRENT SAMPLE SIZE - NEGATIVE 22; POSITIVE 6 
TurbofanEngine <- data.frame()
for (i in 1:28) {
        Dat.fit <- seekarima(, Num, i, AR, MA, t1, t2)
        TurbofanEngine[i,1] <- as.numeric(faultstatus(, Num, i))
        print(paste("@ ",format(Sys.time(),"%H:%M:%S"),
                    " N=", Num, " data=", i,
                    " t1=", t1, " t2=", t2,
                    " arima(", AR,",0,",MA,") Processed!",sep=""))
for (k in 1:(AR + MA)) { TurbofanEngine[i, k + 1] <- as.numeric(Dat.fit$coef[k]) }
}
        
## CONSTRUCT COLUMN NAMES FOR 'TurbofanEngine'
term <- c("EngineStallFault",
          "AR01","AR02","AR03","AR04","AR05","AR06","AR07","AR08",
          "AR09","AR10","AR11","AR12","AR13","AR14","AR15","AR16",
          "AR17","AR18","AR19","AR20",
          "MA01","MA02","MA03","MA04","MA05","MA06")
colnames(TurbofanEngine) <- c(term[1],term[2:(AR+1)],term[22:(MA+21)])
        
## MAKE LRM MODEL FOR ALL EXPLANATORY VARIABLES
fm1 <- lm(EngineStallFault ~ ., data = TurbofanEngine, na.action = NULL)
x <- summary(fm1)$fstatistic
f_para <- data.frame()
        
## RETRIEVE MODELING PARAMETERS AS REQUIRED
f_para[1,1] <- Num
f_para[1,2] <- paste("arima(",AR,",0,",MA,")",sep="")
f_para[1,3] <- t1
f_para[1,4] <- t2
f_para[1,5] <- format(summary(fm1)$r.squared, digits=4)
f_para[1,6] <- format(summary(fm1)$adj.r.squared, digits=4)
f_para[1,7] <- format(summary(fm1)$sigma, digits=4)
## CALCULATE THE P-VALUE OF MODEL FROM F-STATISTICS
f_para[1,8] <- format(pf(x[1],x[2],x[3],lower.tail=FALSE), digits=4)
f_para[1,9] <- AR
f_para[1,10] <- MA
colnames(f_para) <- c("number_of_flights", "arima","t1", "t2", 
                      "r_sqared", "r_sqaured_adj","sigma^2","model_p_value",
                      "AR", "MA")

## print(f_para)
para_text <- (paste("t1 = ", f_para[1,3],
                    "; t2 = ", f_para[1,4],
                    "; R2 = ", f_para[1,5],
                    "; R2_adj = ", f_para[1,6],
                    "; Model_p-value = ", f_para[1,8], sep=""))
print(para_text)
y_hat <- data.frame()
for (y in 1:28) {
        if ( faultstatus(, Num, y) == 1) {
                y_hat[y,1] <- as.numeric(1) - fm1$residuals[y]   
        } else {
                y_hat[y,1] <- (-1)*fm1$residuals[y] 
        }
        y_hat[y,2] <- as.numeric(faultstatus(, Num, y))
}
colnames(y_hat) <- c("y_hat","response")
## print(y_hat)
y_hat <- y_hat[order(y_hat[2]),]
## construct ta and tb
ta <- ifelse(t1==0, "000", t1)
tb <- ifelse(t2==0, "000", t2)
## AUTOMATICALLY SAVE PLOT PER SCREEN DISPLAY
mypath <- paste("/home/albert/simplifiedthesis/img/lrm-arima(",
                AR,",0,",MA,")-", 
                Num, "Comb-",
                "t1(", ta, ")-t2(", tb, ")-",
                format(Sys.time(),"%d%b%Y-%H%M%S"),
                ".png", sep="")
par(mfrow=c(1,1))
par(mar=c(5.1, 5.1, 6.1, 2.1))
plot(y_hat[,1], 
     xlab=paste("Data Points (", Num, "-Combined-Flights)", SEP=""), 
     ylab="y[BLACK], y_hat[BLUE]",
     main=paste("Linear Regression Model Based On ", f_para[1,2], sep=""),
     xlim=c(0,30), 
     ylim=c(-0.2,1.2), 
     pch=16, 
     col="blue", 
     cex=1.5)
lines(y_hat[,1], 
      col="blue", 
      lty=2, 
      lwd=1)
points(y_hat[,2], 
       pch=15, 
       col="black", 
       cex=1.5)
lines(y_hat[,2], 
      col="black", 
      lty=1, 
      lwd=1)
text(12,0.6, labels=para_text, cex=0.95)
mtext(paste("Plotted ", format(Sys.time(),"%d%b%Y %H:%M:%S")), cex=1.05)
abline(h=c(-.2, 0, .2, .4, .6, .8, 1, 1.2), lwd=1, lty=3, col="grey")
abline(v=c(0, 5, 10, 15, 20, 25, 30), lwd=1, lty=3, col="grey")
legend("topleft", 
       legend=c("Orginal","Predicted"), 
       col=c("black","blue"), 
       pch=c(15, 16))
dev.copy(png, file=mypath, width=900, height=500)
dev.off()
        
Wt4 <- paste("Program Completed @",format(Sys.time(),"%d%B%Y %H:%M:%S"))
print(Wt4)
endTime <- Sys.time()
print((endTime - startTime))
return(f_para)
}