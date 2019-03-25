#Question1 

N <- 100
pr <- c(0.25,0.25,0.25,0.25)

test.simu = 0 

for (i in 1:2000)
{
  y = rmultinom(1,N,pr)
  test.simu[i] <- chisq.test(y,p=pr)$statistic
}

hist(test.simu, freq=F, ylim=c(0, 0.25))

xx=seq(from=0, to=16, length.out=500)
dxx=dchisq(xx, df=3)
lines(xx, dxx)

#question2



#question3
#E[x]= integral(0 1) 12x^2(1-x)^2

true.value <- 0.4
N = 2000

u = runif(N,min=0,max=1)
h = u*dbeta(u,2,3)
iest = (1-0)*mean(h)
iest

estvar.iest = var(h)/N
estvar.iest

estse.iest = sqrt(estvar.iest)
#approximate 95% CI
CI1 =iest+ c(-1,1)*1.96*estse.iest

#question4

norm.probfunc <- function(mu,sigma,a,b,level,nobs)
{
  num = runif(nobs,min=a,max=b)
  fx = dnorm(num,mean=mu,sd=sigma)
  iest = (b-a)*mean(fx)
  var.iest = ((b-a)^2)/nobs*var(fx)
  se.iest = sqrt(var.iest)
  ci=c(0,0)
  ci=iest+c(-1,1)*qnorm((1+level)/2)*se.iest
  results = list(iest,ci,level)
  names(results)<-c("estimate", "conf.interval", "conf.level")
  return(results)
}

#(i)
res1 = norm.probfunc(0,2,-2,2,0.95,5000)
res1
pnorm(2,0,2)-pnorm(-2,0,2)
#(ii)
res2 = norm.probfunc(1,1.5,0,2,0.95,5000)
res2
pnorm(2,1,1.5)-pnorm(0,1,1.5)
#(iii)P(X3 > 1.96), where X3 ¡« N(¦Ì = 0, ¦Ò = 1).
#Here we write P(X3 > 1.96) = 0.5 ??? P(0 < X3 < 1.96) and use
#the function to estimate P(0 < X3 < 1.96).
res3 = norm.probfunc(0, 1, 0, 1.96, 0.95, 5000)
res3
p3 = 0.5-res3$estimate
ci3 =c(0.5- res3[[2]][2],0.5-res3[[2]][1])
0.5-(pnorm(1.96)-pnorm(0))







