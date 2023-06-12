Q_H <-
function(simnum = 10000, TT = 50)
{
  signal = sing.acf = h = v = c()

  lb = TT*(TT+2)/(4*(TT-1)^2)
  
  signal = rnorm(TT,mean = 0,sd = 1)
  sing.acf = acf(signal,lag.max = (TT-1),plot = F)
  
  Q1 = vector(mode = "numeric", length = (TT))
  
  lb.pass = 0
  for (j in 2:(TT))
  {
    Q1[(j)] = Q1[(j-1)] + TT*(TT+2)*((sing.acf$acf[j])^2 / (TT-j+1))
    if(Q1[(j)]<lb)
    {
      lb.pass = max(lb.pass,j)
    }
  }
  plot(
    1:(TT-1),Q1[2:TT],type = "l",
    xlab="H", 
    ylab = expression("Q"["H"]),
    ylim =c(-10,(TT*2)),

    main = paste("T =",TT,sep = " ")
  )
  Q_C = 0
  for (k in 1:simnum)
  {
    signal = rnorm(TT,mean = 0,sd = 1)
    sing.acf = acf(signal,lag.max = (TT-1),plot = F)
    
    Q1 = vector(mode = "numeric", length = (TT))
    for (j in 2:(TT))
    {
      Q1[(j)] = Q1[(j-1)] + TT*(TT+2)*((sing.acf$acf[j])^2 / (TT-j+1))
      if(Q1[(j)]<lb)
      {
        lb.pass = max(lb.pass,j)
      }
    }
    Q_C = Q_C+ Q1/simnum
    lines(
      1:(TT-1),Q1[2:TT],type = "l"
    )
  }
  lines(
    1:(TT-1),Q_C[2:TT],type = "l",col="gray"
  )
  lb = TT*(TT+2)/(4*(TT-1)^2)
  abline(h=lb,v = lb.pass,col=c("blue","red"))
  abline(a=0,b = 1 ,col=c("blue"))
  text(x = lb.pass+TT*0.04, y = (2*TT-TT*0.05), labels = paste("H=",lb.pass))
  text(y = lb+TT*0.05, x = (TT-TT*0.05), labels = paste("LB=",lb))
 
  return(lower.b = lb)
}
