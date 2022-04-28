# dynr = unrestricted model, dynrr = restricted model
# usage: gctest(restricted_model, unrestricted_model)
gctest = function(dynrr, dynr){
  urss = sum(dynr$resid^2)
  rrss = sum(dynrr$resid^2)
  udf = dynr$df.residual
  rdf = dynrr$df.residual
  m = abs(rdf - udf)
  F = ((rrss - urss)/m)/(urss/udf)
  pval = 1 - pf(F,m,udf)
  list(F.stat = F, pvalue = pval)
}