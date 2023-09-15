CalcRespProb=function(theta, ipar){
  a=ipar["a"]
  CB=ipar[paste0("cb",1:4)]
  NCAT=ipar["NCAT"]
  
  ps<-numeric(NCAT+1);
  ps[1]<-1;
  ps[NCAT+1]<-0;
  prob=sapply(theta,function(t){
    for (k in 1:(NCAT-1)) {
      ps[k+1]<-1/(1+exp(-1*a*(t-CB[k])));
    }
    prob<-numeric(NCAT);
    for (k in 1:NCAT) {
      prob[k]<-ps[k]-ps[k+1];
    }
    prob
  })
  return(prob)
}