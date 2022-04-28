# testing for serial correlation in regression error
serialcor_test = function(regmodel, maxlag=12) {
library(lmtest)
  bg1 =  bgtest(regmodel, order = 1)
  bg2 =    bgtest(regmodel, order = 2)
  bg3 =    bgtest(regmodel, order = 3)
  bg4 =    bgtest(regmodel, order = 4)
  bg5 =    bgtest(regmodel, order = 5)
  bg6 =    bgtest(regmodel, order = 6)
  bg7 =    bgtest(regmodel, order = 7)
  bg8 =    bgtest(regmodel, order = 8)
  bg9 =    bgtest(regmodel, order = 9)
  bg10 =    bgtest(regmodel, order = 10)
  bg11 =    bgtest(regmodel, order = 11)
  bg12 =    bgtest(regmodel, order = 12)
  # Box-Ljung Q statistic p-values
  blt = rep(0,maxlag)
  for (i in 1:maxlag) {
    b = Box.test(regmodel$resid,lag = i , type = "Ljung-Box")
    blt[i] = b$p.value
  }
  blt
  bg_pvalue = c(bg1$p.value, bg2$p.value,bg3$p.value,bg4$p.value,bg5$p.value,bg6$p.value,bg7$p.value,bg8$p.value,bg9$p.value,bg10$p.value,bg11$p.value,bg12$p.value)
  list(bl_pvalue = blt, bg_pvalue = bg_pvalue)
}
