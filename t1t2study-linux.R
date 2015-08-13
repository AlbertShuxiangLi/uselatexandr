## Best fit while t1=0 and t2=0 ##
## 1-(12,0,4); 2-(16,0,6); 3-(12,0,4); 4-(14,0,6); 5-(16,0,4); 6-(12,0,2); 7-(16,0,2) ##
source("arima-lrm-cutout-linux.R")
f_ar <-  c(12, 12, 12, 12, 12, 12, 12, 12, 12)
f_ma <-  c(4,  4,  4,  4,  4,  4,  4,  4,  4)
f_num <- c(1,  1,  1,  1,  1,  1,  1,  1,  1)
f_t1 <-  c(0,  60, 60, 120,120,180,180,240,240)
f_t2 <-  c(0,  0,  60, 0,  60, 0,  60, 0,  60)

## studyCutout()
F_para <- data.frame()
##for (i in 1:2) {
for (i in 1:length(f_ar)) {
        AR1 <- f_ar[i]
        MA1 <- f_ma[i]
        Num1 <- f_num[i]
        t11 <- f_t1[i]
        t21 <- f_t2[i]
        f_para <- studyCutout(, AR1, MA1, Num1, t11, t21)
        F_para <- rbind(F_para, f_para)
}
write.table(F_para, file=paste("/home/albert/simplifiedthesis/img/arima-lrm-results-",
                               format(Sys.time(),"%d%b%Y-%H%M%S"),".csv",sep=""))