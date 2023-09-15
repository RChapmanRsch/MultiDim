
thetaquad=round(seq(-3,3,0.1),1)

banksample=split(1:nrow(itempool),rep(1:10,nrow(itempool)/10))

items=lapply(banksample, function(banksample){
  #select item from item pool (1 million items) to suit the information shape
  bankinfoshapedensity=switch(as.character(bankinfoshape),
                          "flat"=rep(1/61, times=length(thetaquad)),
                          "leptokurtic"=dnorm(thetaquad,0,2)/sum(dnorm(thetaquad,0,2)),
                          "skewed"=(dbeta(thetaquad/4+2.5/4,2.5,4)+2.5/4)/(sum(dbeta(thetaquad/4+2.5/4,2.5,4)+2.5/4)))

  
  
  
  itemsused=c()

  bankinfo=rep(0,times=length(thetaquad))
  names(bankinfo)=thetaquad
  
  items=itempool[banksample,]
  iteminfo=apply(items,1,function(i){
    sapply(thetaquad, function(t){CalcInfo(t,i)})})

  #iterate over 'best items'
  
  for(item in 1:ni_dim){
      deviance=numeric(length=ncol(iteminfo))
      bankiteminfo=iteminfo
      for(candidateitem in 1:ncol(iteminfo)){
        bankiteminfo[,candidateitem]=iteminfo[,candidateitem]+bankinfo
        deviance[candidateitem]=
          sum(abs(bankinfoshapedensity-bankiteminfo[,candidateitem]/sum(bankiteminfo[,candidateitem])))
        }
      bestitem=which.min(deviance[!(1:ncol(iteminfo)%in%itemsused)])
      itemsused=c(itemsused,bestitem)
      bankinfo=bankiteminfo[,bestitem]
  }
  list("items"=items[itemsused,], "bankinfo"=bankinfo,"design"=bankinfoshapedensity, "deviance"=deviance)
})

items=items[order(sapply(items, function(x){x$deviance[length(x$deviance)]}),decreasing=FALSE)[1:numdimensions]]

####plotting for lab
png("out.png", 800,400)
par(mfrow = c(1,2))

plot(thetaquad, items$'1'$design, type="l", ylab="info density", main=paste(bankinfoshape, "Design Bank Info"))


plot(thetaquad,items$'1'$bankinfo, ylim=c(0,60), type="l", ylab="info", main=paste(bankinfoshape,"Simulated Bank Info"))
lines(thetaquad,items$'2'$bankinfo, lty=2)
lines(thetaquad,items$'3'$bankinfo, lty=3)
dev.off()

