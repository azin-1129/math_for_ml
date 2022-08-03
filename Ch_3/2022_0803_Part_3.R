f<-function(x){
  print(x)
  return(x^2)
}

f<-function(x) (x^3+3*x^2-6*x-8)

curve(f, -5, 4, ylab='y=f(x)')

g<-function(x){}
body(g)<-D(body(f), 'x')
curve(g, -5,4 ,ylab='g(x)')

f2=function(x) (x^2+1)
integrate(f2, lower=0, upper=1)

integrate(g, lower=0, upper=10)

newton=function(f, tol=1e-7, x0=1, N=100){
  h=1e-7;
  i=1
  x1=x0;
  p=numeric(N)
  
  while(i<=N){
    df.dx=(f(x0+h)-f(x0))/h
    x1=(x0-(f(x0)/df.dx))
    
    p[i]=x1
    i=i+1
    
    if(abs(x1-x0)<tol) break
  }
  
  return(p[1:(i-1)])
}

f<-function(x)(x^2-2)
newton(f)
f<-function(x) (x^3+3*x^2-6*x-8)
newton(f)
