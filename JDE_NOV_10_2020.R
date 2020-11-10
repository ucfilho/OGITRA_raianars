library('DEoptimR')


SeiCo= function(x)
  {
  Num=length(x)
	s=0
	for (i in 1:Num)
	  {
    		fun = 100*(x[i]-x[i-1]**2)**2 + (1-x[i-1])**2

	  }
  return(fun)
  }
# global minimum f(0)= 0 d=[-10,10] 

dim=10
RUNS=50
ITE=2000
NPAR=100
Bounds=10
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-Bounds, dim), rep(Bounds, dim), SeiCo ,
         tol = 1e-100,NP=NPAR, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('SeiCo JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'Bounds=',-Bounds,Bounds,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
