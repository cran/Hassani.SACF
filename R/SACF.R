SACF <-
function(x)
{
aa=acf(x)
names(aa)
aa$acf
L=length(aa$acf)
sacf = sum(aa$acf [-c(1)])
return(SACF = sacf)
}
