
#12/13/2019
ad.mq=function(Y,Ped){
  res=qgmod.mq(Y,Ped,Model="AD",Cross="Two")
  return(res)
}

## checked Dec 13, 2019
ad4.mq=function(Y,Ped){
  res=qgmod.mq(Y,Ped,Model="AD",Cross="Four")
  return(res)
}

## checked Dec 13, 2019
adc.mq=function(Y,Ped){
  res=qgmod.mq(Y,Ped,Model="ADC",Cross="Two")
  return(res)
  
}
## checked Dec 13, 2019
adc4.mq=function(Y,Ped){
  res=qgmod.mq(Y,Ped,Model="ADC",Cross="Four")
  return(res)
  
}

## checked Dec 13, 2019
adm.mq=function(Y,Ped){
  res=qgmod.mq(Y,Ped,Model="ADM",Cross="Two")
  return(res)
  
}

## checked Dec 13, 2019
adaa.mq=function(Y,Ped){
  res=qgmod.mq(Y,Ped,Model="ADAA",Cross="Two")
  return(res)
  
}

## checked Dec 13, 2019
adrc.mq=function(Y,Ped,Row=NULL,Col=NULL){
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")  ## checked Dec 13, 2019
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)  ## checked
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=mq0(gdata)   ## checked
  
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  res$FixedEffect=res$FixEffect
  res$RandomFixedEffect=res$FixEffect
  return(res)
}

## Needed Dec 13, 2019

ad4rc.mq=function(Y,Ped,Row=NULL,Col=NULL){
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Four")
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=mq0(gdata)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X0=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X0=cbind(X0,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X0%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  return(res)
}

##checked Dec 13, 2019
ad4.mq.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.mq.jack(Y,Ped,Model="AD",Cross="Four",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adc4.mq.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.mq.jack(Y,Ped,Model="ADC",Cross="Four",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019

ad4.reml.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.reml.jack(Y,Ped,Model="AD",Cross="Four",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adc4.reml.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.reml.jack(Y,Ped,Model="ADC",Cross="Four",JacNum=JacNum,JacRep=JacRep)
  return(res)
}


## Needed Dec 13, 2019

adrc.mq.jack=function(Y,Ped,Row=NULL,Col=NULL,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  res$FixedEffect=res$FixEffect
  res$RandomFixedEffect=res$FixEffect
  return(res)
}

## Needed Dec 13, 2019

ad4rc.mq.jack=function(Y,Ped,Row=NULL,Col=NULL,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Four")
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  res$FixedEffect=res$FixEffect
  res$RandomFixedEffect=res$FixEffect
  return(res)
}

## Needed Dec 13, 2019
adrc.reml.jack=function(Y,Ped,Row=NULL,Col=NULL,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=genmod.reml.jack(gdata,JacNum=JacNum,JacRep=JacRep)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  res$FixedEffect=res$FixEffect
  res$RandomFixedEffect=res$FixEffect
  return(res)
}

## Needed Dec 13, 2019

ad4rc.reml.jack=function(Y,Ped,Row=NULL,Col=NULL,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=genmod.reml.jack(gdata,JacNum=JacNum,JacRep=JacRep)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  #mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  res$FixedEffect=res$FixEffect
  res$RandomFixedEffect=res$FixEffect
  return(res)
}



## Needed Dec 13, 2019
ad.mq.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.mq.jack(Y,Ped,Model="AD",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adc.mq.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.mq.jack(Y,Ped,Model="ADC",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adm.mq.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.mq.jack(Y,Ped,Model="ADM",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adaa.mq.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.mq.jack(Y,Ped,Model="ADAA",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}


## Needed Dec 13, 2019
qgmod.mq.jack=function(Y,Ped,Model,Cross,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  gdata=genmod.data(Y,Ped,Model,Cross)
  res=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  #mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  
  return(res)
}


## Needed Dec 13, 2019
qgmod.reml.jack=function(Y,Ped,Model,Cross,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  gdata=genmod.data(Y,Ped,Model,Cross)
  res=genmod.reml.jack(gdata,JacNum=JacNum,JacRep=JacRep)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  #mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  return(res)
  
}



## Needed Dec 13, 2019

ad.reml.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.reml.jack(Y,Ped,Model="AD",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adc.reml.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.reml.jack(Y,Ped,Model="ADC",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adm.reml.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.reml.jack(Y,Ped,Model="ADM",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
adaa.reml.jack=function(Y,Ped,JacNum=NULL,JacRep=NULL){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  res=qgmod.reml.jack(Y,Ped,Model="ADAA",Cross="Two",JacNum=JacNum,JacRep=JacRep)
  return(res)
}

## Needed Dec 13, 2019
ad.simudata=function(Y,Ped,v,b,SimuNum=NULL){
  if(is.null(SimuNum))SimuNum=200
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")
  gdata=SimuData(gdata,v=v,b=b,SimuNum=SimuNum)
  YS=gdata$Y
  return(YS)
}

## Needed Dec 13, 2019
adc.simudata=function(Y,Ped,v,b,SimuNum=NULL){
  if(is.null(SimuNum))SimuNum=200
  gdata=genmod.data(Y,Ped,Model="ADC",Cross="Two")
  gdata=SimuData(gdata,v=v,b=b,SimuNum=SimuNum)
  YS=gdata$Y
  return(YS)
}

## Needed Dec 13, 2019
adaa.simudata=function(Y,Ped,v,b,SimuNum=NULL){
  if(is.null(SimuNum))SimuNum=200
  gdata=genmod.data(Y,Ped,Model="ADAA",Cross="Two")
  gdata=SimuData(gdata,v=v,b=b,SimuNum=SimuNum)
  YS=gdata$Y
  return(YS)
}

## Needed Dec 13, 2019
adm.simudata=function(Y,Ped,v,b,SimuNum=NULL){
  if(is.null(SimuNum))SimuNum=200
  gdata=genmod.data(Y,Ped,Model="ADM",Cross="Two")
  gdata=SimuData(gdata,v=v,b=b,SimuNum=SimuNum)
  YS=gdata$Y
  return(YS)
}

## Needed Dec 13, 2019
ad.simu=function(Y,Ped,method=NULL,ALPHA=NULL){
  if(is.null(ALPHA))ALPHA=0.05
  if(is.null(method))method=c("minque")
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=mq0(gdata)
    if(method[i]=="reml")RES[[i]]=reml0(gdata)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,4]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}

## Needed Dec 13, 2019
adc.simu=function(Y,Ped,method=NULL,ALPHA=NULL){
  if(is.null(ALPHA))ALPHA=0.05
  if(is.null(method))method=c("minque")
  gdata=genmod.data(Y,Ped,Model="ADC",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=mq0(gdata)
    if(method[i]=="reml")RES[[i]]=reml0(gdata)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,4]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}

## Needed Dec 13, 2019
adaa.simu=function(Y,Ped,method=NULL,ALPHA=NULL){
  if(is.null(ALPHA))ALPHA=0.05
  if(is.null(method))method=c("minque")
  gdata=genmod.data(Y,Ped,Model="ADAA",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=mq0(gdata)
    if(method[i]=="reml")RES[[i]]=reml0(gdata)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,4]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}

## Needed Dec 13, 2019
adm.simu=function(Y,Ped,method=NULL,ALPHA=NULL){
  if(is.null(ALPHA))ALPHA=0.05
  if(is.null(method))method=c("minque")
  gdata=genmod.data(Y,Ped,Model="ADM",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=mq0(gdata)
    if(method[i]=="reml")RES[[i]]=reml0(gdata)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,4]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}


## Needed Dec 13, 2019

ad.simu.jack=function(Y,Ped,method=NULL,JacNum=NULL,JacRep=NULL,ALPHA=NULL){
  if(is.null(method))method=c("minque")
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  if(is.null(ALPHA))ALPHA=0.05
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  #gdata = mixed.data(formula)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
    if(method[i]=="reml")RES[[i]]=genmod.reml.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,3]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}

## Needed Dec 13, 2019
adc.simu.jack=function(Y,Ped,method=NULL,JacNum=NULL,JacRep=NULL,ALPHA=NULL){
  if(is.null(method))method=c("minque")
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  if(is.null(ALPHA))ALPHA=0.05
  gdata=genmod.data(Y,Ped,Model="ADC",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  #gdata = mixed.data(formula)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
    if(method[i]=="reml")RES[[i]]=genmod.reml.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,3]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}

## Needed Dec 13, 2019
adaa.simu.jack=function(Y,Ped,method=NULL,JacNum=NULL,JacRep=NULL,ALPHA=NULL){
  if(is.null(method))method=c("minque")
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  if(is.null(ALPHA))ALPHA=0.05
  gdata=genmod.data(Y,Ped,Model="ADAA",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  #gdata = mixed.data(formula)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
    if(method[i]=="reml")RES[[i]]=genmod.reml.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,3]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}

## Needed Dec 13, 2019
adm.simu.jack=function(Y,Ped,method=NULL,JacNum=NULL,JacRep=NULL,ALPHA=NULL){
  if(is.null(method))method=c("minque")
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  if(is.null(ALPHA))ALPHA=0.05
  gdata=genmod.data(Y,Ped,Model="ADM",Cross="Two")
  ml=length(gdata$U)+1
  gdata$VC=rep(1,ml)
  #gdata = mixed.data(formula)
  mn=length(method)
  RES=list()
  for(i in 1:mn){
    if(method[i]=="minque")RES[[i]]=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
    if(method[i]=="reml")RES[[i]]=genmod.reml.jack(gdata,JacNum=JacNum,JacRep=JacRep,ALPHA=ALPHA)
  }
  for(i in 1:mn){
    res=RES[[i]]
    V=NULL
    Var=res$Var
    SimuNum=length(Var)
    ml=length(Var[[1]][,1])
    P=numeric(ml)
    vm=numeric(ml)
    for(j in 1:SimuNum){
      vm=vm+Var[[j]][,1]
      V=cbind(V,Var[[j]][,1])
      id=which(Var[[j]][,3]<=ALPHA)
      P[id]=P[id]+1
    }
    SE=numeric(ml)
    for(j in 1:ml)SE[j]=sqrt(var(V[j,])/SimuNum)
    vm=vm/SimuNum
    P=P/SimuNum
    a=data.frame(vm,SE,P)
    colnames(a)=c("Estimate","SE","Power")
    rownames(a)=rownames(Var[[1]])
    RES[[i]]=a
  }
  names(RES)=method
  RES$ALPHA=ALPHA
  return(RES)
}

## Checked Dec 13, 2019
qgmod.mq=function(Y,Ped,Model,Cross){
  gdata=genmod.data(Y,Ped,Model,Cross)
  res=mq0(gdata)
  
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  #mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  
  return(res)
}

## needed Dec 13, 2019
ad4.reml=function(Y,Ped){
  res=qgmod.reml(Y,Ped,Model="AD",Cross="Four")
  return(res)
}

## needed Dec 13, 2019
adc4.reml=function(Y,Ped){
  res=qgmod.reml(Y,Ped,Model="ADC",Cross="Four")
  return(res)
  
}

## needed Dec 13, 2019
ad.reml=function(Y,Ped){
  res=qgmod.reml(Y,Ped,Model="AD",Cross="Two")
  return(res)
}

## needed Dec 13, 2019
adc.reml=function(Y,Ped){
  res=qgmod.reml(Y,Ped,Model="ADC",Cross="Two")
  return(res)
  
}

## needed Dec 13, 2019
adm.reml=function(Y,Ped){
  res=qgmod.reml(Y,Ped,Model="ADM",Cross="Two")
  return(res)
  
}

## needed Dec 13, 2019
adaa.reml=function(Y,Ped){
  res=qgmod.reml(Y,Ped,Model="ADAA",Cross="Two")
  return(res)
  
}

## needed Dec 13, 2019
adrc.reml=function(Y,Ped,Row=NULL,Col=NULL){
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Two")
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=reml0(gdata)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  #mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  res$FixedEffect=res$FixEffect
  res$RandomFixedEffect=res$FixEffect
  return(res)
}

## needed Dec 13, 2019
ad4rc.reml=function(Y,Ped,Row=NULL,Col=NULL){
  gdata=genmod.data(Y,Ped,Model="AD",Cross="Four")
  en=unique(Ped[,1])
  mat=cbind(Row,Col)
  if(length(en)>1){
    if(!is.null(Row))Row=paste(Ped[,1],Row,sep=":")
    if(!is.null(Col))Col=paste(Ped[,1],Col,sep=":")
  }
  if(!is.null(mat))URC=GenerateU(mat)
  gdata$U=c(gdata$U,URC)
  gdata$VC=c(gdata$VC,1,1)
  res=reml0(gdata)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  #mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  res$FixedEffect=res$FixEffect
  res$RandomFixedEffect=res$FixEffect
  return(res)
}

## Needed Dec 13, 2019
qgmod.reml=function(Y,Ped,Model,Cross){
  gdata=genmod.data(Y,Ped,Model,Cross)
  res=reml0(gdata)
  mk=length(gdata$U)
  Uk=numeric()
  BigU=NULL
  for(u in 1:mk){
    Uk[u]=ncol(gdata$U[[u]])
    BigU=cbind(BigU,gdata$U[[u]])
  }
  #mt=sum(Uk)
  xk=length(gdata$X)
  Xk=numeric()
  X=NULL
  for(i in 1:xk){
    Xk[i]=ncol(gdata$X[[i]])
    X=cbind(X,gdata$X[[i]])
  }
  TraitNum=ncol(gdata$Y)
  Residual=NULL
  Fitted=NULL
  for(j in 1:TraitNum){
    a=0
    re=res$RandomEffect[[j]][,1]
    fe=res$FixedEffect[[j]][,1]
    if(mk>0)a=a+BigU%*%re
    a=a+X%*%fe
    Fitted=cbind(Fitted,a)
    a=gdata$Y[,j]-a
    Residual=cbind(Residual,a)
  }
  colnames(Residual)=colnames(gdata$Y)
  colnames(Fitted)=colnames(gdata$Y)
  res$Residual=Residual
  res$Fitted=Fitted
  
  #res$FixedEffect=res$FixEffect
  return(res)
}


## Checked Dec 13, 2019

genmod.data=function(Y,Ped,Model,Cross,X=NULL,CC=NULL,...){
  
  nc=ncol(Ped)
  #for(i in 1:nc)Ped[,i]=as.vector(Ped[,i])
  
  #if(class(Y)=="numeric"){
  #  Y=data.frame(Y)
  #  colnames(Y)=paste(deparse(substitute(Y)))
  #}
  Y=data.frame(Y)
  n=nrow(Y)
  if(is.null(X)){
    #X=rep(1,n)
    X=matrix(1,n,1)
    colnames(X)="mu"
  }
  if(Cross=="Two"){
    nc=ncol(Ped)
    Env=as.vector(Ped[,1])
    F1=as.vector(Ped[,2])
    M1=as.vector(Ped[,3])
    Gen=as.vector(Ped[,4])
    if(nc==5){
      BlockID=1
      Block=Ped[,5]
    }
    else if(nc==4){
      BlockID=0
      Block=NULL
    }
    if(Model=="ADAA"){
      U=ADAA2Matrix(Env,F1,M1,Gen,Block,BlockID)
      mk=length(U)
      if(mk==3)VC=c(2,1,4,1)
      else if(mk==4)VC=c(2,1,4,1,1)
      else if(mk==7)VC=c(1,2,1,4,2,1,4,1)
      else if(mk==8)VC=c(1,2,1,4,2,1,4,1,1)
      
    }
    else if(Model=="AD"){
      U=AD2Matrix(Env,F1,M1,Gen,Block,BlockID)
      mk=length(U)
      if(mk==2)VC=c(2,1,1)
      else if(mk==3)VC=c(2,1,1,1)
      else if(mk==5)VC=c(1,2,1,2,1,1)
      else if(mk==6)VC=c(1,2,1,2,1,1,1)
      
    }
    else if(Model=="ADC"){
      U=ADC2Matrix(Env,F1,M1,Gen,Block,BlockID)
      mk=length(U)
      if(mk==3)VC=c(1,2,1,1)
      else if(mk==4)VC=c(1,2,1,1,1)
      else if(mk==7)VC=c(1,1,2,1,1,2,1,1)
      else if(mk==8)VC=c(1,1,2,1,1,2,1,1,1)
      
    }
    else if(Model=="ADM"){
      U=ADM2Matrix(Env,F1,M1,Gen,Block,BlockID)
      mk=length(U)
      if(mk==4)VC=c(2,1,2,1,1)
      else if(mk==5)VC=c(2,1,2,1,1,1)
      else if(mk==9)VC=c(1,2,1,2,1,2,1,2,1,1)
      else if(mk==10)VC=c(1,2,1,2,1,2,1,2,1,1,1)
      
      
    }
    #result=list(Y=Y,X=X,U=U,VC=VC,Model=Model)
    #result$call=match.call()
    #class(result)="genmod.data"
    #return(result)
  }
  if(Cross=="Four"){
    nc=ncol(Ped)
    Env=as.vector(Ped[,1])
    F1=as.vector(Ped[,2])
    M1=as.vector(Ped[,3])
    F2=as.vector(Ped[,4])
    M2=as.vector(Ped[,5])
    Gen=as.vector(Ped[,6])
    if(nc==7){
      BlockID=1
      Block=Ped[,7]
    }
    else if(nc==6){
      BlockID=0
      Block=NULL
    }
    if(Model=="ADAA"){
      #U=ADAA2Matrix(Env,F1,M1,Gen,Block,BlockID)
      #mk=length(U)
      #if(mk==3)VC=c(2,1,4,1)
      #else if(mk==4)VC=c(2,1,4,1,1)
      #else if(mk==7)VC=c(1,2,1,4,2,1,4,1)
      #else if(mk==8)VC=c(1,2,1,4,2,1,4,1,1)
      cat("No such a model available\n")
      return(0)
      
    }
    else if(Model=="AD"){
      if(is.null(CC))U=AD4Matrix(Env,F1,M1,F2,M2,Gen,Block,BlockID)
      else U=AD4Matrix(Env,F1,M1,F2,M2,Gen,Block,BlockID,CC)
      mk=length(U)
      if(mk==2)VC=c(2,1,1)
      else if(mk==3)VC=c(2,1,1,1)
      else if(mk==5)VC=c(1,2,1,2,1,1)
      else if(mk==6)VC=c(1,2,1,2,1,1,1)
    }
    else if(Model=="ADC"){ ##done
      U=ADC4Matrix(Env,F1,M1,F2,M2,Gen,Block,BlockID)
      mk=length(U)
      if(mk==3)VC=c(1,2,1,1)
      else if(mk==4)VC=c(1,2,1,1,1)
      else if(mk==7)VC=c(1,1,2,1,1,2,1,1)
      else if(mk==8)VC=c(1,1,2,1,1,2,1,1,1)
      
    }
    else if(Model=="ADM"){
      cat("No such a model available\n")
      return(0)
      #U=ADM2Matrix(Env,F1,M1,Gen,Block,BlockID)
      #mk=length(U)
      #if(mk==4)VC=c(2,1,2,1,1)
      #else if(mk==5)VC=c(2,1,2,1,1,1)
      #else if(mk==9)VC=c(1,2,1,2,1,2,1,2,1,1)
      #else if(mk==10)VC=c(1,2,1,2,1,2,1,2,1,1,1)
    }
  }
  X0=list()
  X0[[1]]=X
  result=list(Y=Y,X=X0,U=U,VC=VC,C=NULL)
  #result$call=match.call()
  #class(result)="genmod.data"
  return(result)
}



## Checked Dec 13, 2019

mq0=function(gdata){
  Y0=as.matrix(gdata$Y)
  #Y0=as.matrix(Y0)
  TraitNum=ncol(Y0)
  TraitNames=colnames(Y0)
  Var=list()
  RE=NULL
  FE=list()
  #i=1
  mk=length(gdata$U)
  for(i in 1:TraitNum){
    gdata$Y=as.matrix(Y0[,i])
    res=genmod.mq(gdata)  
    Var[[i]]=res$Var
    if(mk>0)RE[[i]]=res$RandomEffect
    #else RE[[i]]=NULL
    FE[[i]]=res$FixEffect
  }
  #res$FixEffect
  names(Var)=TraitNames
  if(!is.null(RE))names(RE)=TraitNames
  names(FE)=TraitNames
  res=list(Var=Var,FixedEffect=FE,RandomEffect=RE)
  gdata$Y=Y0
  return(res)
}

## Checked Dec 13, 2019

genmod.mq=function(gdata){
  res0=genmod(gdata)
  res=genmod(gdata,au=res0$Var[,1])
  ##Statistical tests for variance components
  h=diag(svdinv(res$HV))*2
  Chi_sq=(res0$Var[,1])^2/h
  P_value=(1-pchisq(Chi_sq,1))/2
  Var=data.frame(res0$Var,sqrt(h),Chi_sq,P_value)
  colnames(Var)=c("Est","SE","Chi_sq","P_value")
  res$Var=Var
  
  ##Statistical tests for fixed effects
  #res0$FixEffect[,1]
  h=diag(svdinv(res$HF[[1]]))
  h=sqrt(h)
  z=res0$FixEffect[,1]/h
  P_value=1-pchisq(z^2,1)
  attributes(res)
  FixEffect=data.frame(res0$FixEffect,h,z,P_value)
  colnames(FixEffect)=c("Est","SE","z_value","P_value")
  res$FixEffect=FixEffect
  
  ##Statistical tests for random effect  
  mk=length(gdata$U)
  if(mk>0){
    
    h=get_HR(gdata,res$Qa,res0$Var[,1])
    z=res0$RandomEffect[,1]/h
    res0$RandomEffect[,1][1]/h[1]
    
    P_value=1-pchisq(z^2,1)
    RandomEffect=data.frame(res0$RandomEffect,h,z,P_value)
    colnames(RandomEffect)=c("Pre","SE","z_value","P_value")
    res$RandomEffect=RandomEffect
  }
  
  return(res)
}

##This function is used without jackknife process

## Needed Dec 13, 2019
reml0=function(gdata,criterion=NULL){
  if(is.null(criterion))criterion=1e-3
  Y0=gdata$Y
  TraitNum=ncol(Y0)
  TraitNames=colnames(Y0)
  Var=list()
  RE=NULL
  FE=list()
  mk=length(gdata$U)
  for(i in 1:TraitNum){
    gdata$Y=as.matrix(Y0[,i])
    res=genmod.reml(gdata,criterion)
    Var[[i]]=res$Var
    if(mk>0)RE[[i]]=res$RandomEffect
    #else RE[[i]]=NULL
    FE[[i]]=res$FixEffect
  }
  
  names(Var)=TraitNames
  if(!is.null(RE))names(RE)=TraitNames
  names(FE)=TraitNames
  res=list(Var=Var,FixedEffect=FE,RandomEffect=RE)
  gdata$Y=Y0
  return(res)
}


## Needed Dec 13, 2019
genmod.reml=function(gdata,criterion=NULL){
  
  if(is.null(criterion))DIFF=1e-3
  else DIFF=criterion
  d=100
  res=genmod(gdata)
  mk=length(gdata$U)
  au0=res$Var[,1]
  id=which(au0<=0)
  au0[id]=0
  its=1
  while(d>DIFF&&its<10){
    
    res=genmod(gdata,au=au0)
    (v1=res$Var[,1])
    id=which(v1<=0)
    v1[id]=0
    #print(v1)
    #cat("\n")
    
    d=sum(au0-v1)^2
    d=sqrt(d)
    d=d/sum(au0)
    au0=v1
    #print(d)
    #cat("\n")
    its=its+1
  }
  ##Statistical tests for variance components
  h=diag(svdinv(res$HV))*2
  Chi_sq=(res$Var[,1])^2/h
  P_value=(1-pchisq(Chi_sq,1))/2
  Var=data.frame(res$Var,sqrt(h),Chi_sq,P_value)
  colnames(Var)=c("Est","SE","Chi_sq","P_value")
  res$Var=Var
  
  ##Statistical tests for fixed effects
  res$FixEffect[,1]
  h=diag(svdinv(res$HF[[1]]))
  h=sqrt(h)
  z=res$FixEffect[,1]/h
  P_value=1-pchisq(z^2,1)
  attributes(res)
  FixEffect=data.frame(res$FixEffect,h,z,P_value)
  colnames(FixEffect)=c("Est","SE","z_value","P_value")
  res$FixEffect=FixEffect
  
  ##Statistical tests for random effect  
  mk=length(gdata$U)
  if(mk>0){
    h=get_HR(gdata,res$Qa,res$Var[,1])
    z=res$RandomEffect[,1]/h
    res$RandomEffect[,1][1]/h[1]
    P_value=1-pchisq(z^2,1)
    RandomEffect=data.frame(res$RandomEffect,h,z,P_value)
    colnames(RandomEffect)=c("Pre","SE","z_value","P_value")
    res$RandomEffect=RandomEffect
  }
  
  return(res)
}

## Checked Dec 13, 2019
genmod=function(gdata,au=NULL,LUP=NULL,...){
  Y=gdata$Y
  U=gdata$U
  C=gdata$C
  ml=length(gdata$U)+1
  if(is.null(au))au=rep(1,ml)
  xk=length(gdata$X)
  if(xk==1)X=gdata$X[[1]]
  if(xk>1)X=BigX(gdata$X)  ## checked
  nx=ncol(X)
  #for(i in 1:nx)X[,i]=as.numeric(X[,i])
  VC=gdata$VC
  #Model=gdata$model
  #BigX=NULL
  #class(X[,3])
  #X=as.matrix(X)
  if(is.null(LUP))LUP=NULL
  #result=minque.default(X,U,Y,C,au)
  result=minque.default(X,U,Y,C,au,LUP)
  result$Var=result$Var*VC
  #result$call=match.call()
  class(result)="minque"
  return(result)
}


## Checked Dec 13, 2019

BigX=function(X,...){
  
  X0=NULL
  xk=length(X)
  xnames=NULL
  #i=2
  for(i in 1:xk)X0=cbind(X0,as.matrix(X[[i]]))
  colnames(X0)   
  return(X0)
}

## Needed Dec 13, 2019
minque.default=function(X,U,Y,C=NULL,au=NULL,LUP=NULL){
  mk=length(U)
  n=nrow(U[[1]])
  if(is.null(au))au=rep(1,(mk+1))
  if(is.null(X))X=matrix(1,nrow=n,ncol=1)
  if(is.null(C))C=NULL
  xnames=colnames(X)
  if(mk==0)vnames="V(e)"
  else if(mk>0){
    rnames=NULL
    vnames=c(paste("V(",names(U),")",sep=""),"V(e)")
    for(i in 1:mk)rnames=c(rnames,colnames(U[[i]]))
  }
  
  aa=GetQa(au,U,X,C)
  Qa=aa$Qa
  VI=aa$VI
  ML=MINQUE_L(Qa,U)
  #if(class(Y)=="numeric")
  Y=data.frame(Y)
  TraitNum=ncol(Y)
  TraitNames=colnames(Y)
  Var=matrix(0,(mk+1),TraitNum)
  B=matrix(0,ncol(X),TraitNum)
  hf=list()
  if(mk>0)Pre=matrix(0,length(rnames),TraitNum)
  if(is.null(LUP))VI=NULL
  for(i in 1:TraitNum){
    MR=MINQUE_R(Qa,U,Y[,i])
    a=MINQUEVarPre(Qa,X,U,ML,MR,Y[,i],C,VI)
    Var[,i]=a[[1]]
    B[,i]=a[[2]]
    if(mk>0)Pre[,i]=a[[3]]
    hf[[i]]=a$HF
  }
  
  colnames(Var)=TraitNames
  rownames(Var)=vnames
  colnames(B)=TraitNames
  rownames(B)=xnames
  names(hf)=TraitNames
  if(mk>0){
    colnames(Pre)=TraitNames
    rownames(Pre)=rnames
  }
  if(mk>0)result=list(Var=data.frame(Var),
                      FixEffect=data.frame(B),
                      RandomEffect=data.frame(Pre),
                      HV=ML,
                      Qa=Qa,
                      HF=hf)
  if(mk==0)result=list(Var=data.frame(Var),
                       FixEffect=data.frame(B),
                       RandomEffect=NULL,
                       HV=ML,
                       Qa=Qa,
                       HF=hf)
  result$call=match.call()
  return(result)
}




## Needed Dec 13, 2019
ginv=function (X, tol = sqrt(.Machine$double.eps)){ 

    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- svd(X)
    if (is.complex(X)) 
        Xsvd$u <- Conj(Xsvd$u)
    Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
    if (all(Positive)) 
        Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    else if (!any(Positive)) 
        array(0, dim(X)[2L:1L])
    else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
        t(Xsvd$u[, Positive, drop = FALSE]))
}


## Needed Dec 13, 2019
MINQUEVarPre=function(Qa,X,U,ML,MR,y,C=NULL,VI=NULL){
   svdinv=LargeInv
   #ML1=as.matrix(ML)
   #V0=ginv(ML)%*%MR ##Estimate variance components
   V0=svdinv(ML)%*%MR ##Estimate variance components
   v=as.vector(V0)
   mk=length(U)
   n=nrow(X)
   if(is.null(VI)){
      if(mk==0)VI=diag(1/v[1],n)
      else if(mk>0){
         id=which(v<=0)
         sumv=sum(v[id])
         smallvalue=sumv/10^3
         id=which(v<smallvalue)
         v[id]=smallvalue
         VI=Woodbury(v,U)
      }
   } 
   #X0=X
   if(ncol(X)==1){
       HF=t(X[,1])%*%VI%*%X[,1]
       bhat=svdinv(HF)%*%(t(X[,1])%*%VI%*%y)  ##Estimate fixed effects
   }
   if(is.null(C)){
       HF=t(X)%*%VI%*%X
       bhat=svdinv(HF)%*%(t(X)%*%VI%*%y)
   }
   else{
      HF=t(X)%*%VI%*%X+C%*%t(C)
      bhat=svdinv(HF)%*%(t(X)%*%VI%*%y)
   }
   #dim(X)
   #n=ncol(U[[1]])
   #mk=length(U)
   if(mk>0){
     k=numeric()
     for(i in 1:mk){
        nu=ncol(U[[i]])-1
        if(v[i]<=0){
          k[i]=0
          ##v[i]=0
        }
        else if(v[i]>0)k[i]=sqrt(nu*v[i]/MR[i])
     }
     PRE=numeric()
     vc=numeric()

     for(i in 1:mk){
        if(k[i]==0){
          es=rep(0,ncol(U[[i]]))
          PRE=c(PRE,es)
        }
        else if(k[i]>0){
          es<-k[i]*t(U[[i]])%*%Qa%*%y
          #es<-t(U[[i]])%*%Qa%*%y
          PRE=c(PRE,es)
        }
        #vc[i]=var(es)
     }
   }
   if(mk==0)result=list(v=v,b=bhat,Pre=NULL,HF=HF)
   else result=list(v=v,b=bhat,Pre=PRE,HF=HF)
   return(result)
}

MINQUEVarLUP=function(Qa,VI,X,U,ML,MR,y,C=NULL){
   svdinv=LargeInv
   V0=svdinv(ML)%*%MR ##Estimate variance components
   v=as.vector(V0)
   mk=length(U)
   if(ncol(X)==1) bhat=svdinv(t(X[,1])%*%VI%*%X[,1])%*%(t(X[,1])%*%VI%*%y)  ##Estimate fixed effects
   if(is.null(C)) bhat=svdinv(t(X)%*%VI%*%X)%*%(t(X)%*%VI%*%y)
   else bhat=svdinv(t(X)%*%VI%*%X+C%*%t(C))%*%(t(X)%*%VI%*%y)
   
   if(mk>0){
     k=numeric()
     for(i in 1:mk){
        nu=ncol(U[[i]])-1
        if(v[i]<=0){
          k[i]=0
          ##v[i]=0
        }
        else if(v[i]>0)k[i]=sqrt(nu*v[i]/MR[i])
     }
     PRE=numeric()
     vc=numeric()

     for(i in 1:mk){
        if(k[i]==0){
          es=rep(0,ncol(U[[i]]))
          PRE=c(PRE,es)
        }
        else if(k[i]>0){
          es<-k[i]*t(U[[i]])%*%Qa%*%y
          #es<-t(U[[i]])%*%Qa%*%y
          PRE=c(PRE,es)
        }
        #vc[i]=var(es)
     }
   }
   if(mk==0)result=list(v=v,b=bhat,Pre=NULL)
   else result=list(v=v,b=bhat,Pre=PRE)
   #result=list(v=v,b=bhat,Pre=NULL)
   return(result)
}



SVDInv=function (x, eps = 1e-06) 
{
    if (any(is.na(x))) 
        stop("NA(s) encountered in matrix, cannot invert.")
    if (length(x) > 1) {
        savesvd <- svd(x, LINPACK = TRUE)
        U.svd <- savesvd$u
        V.svd <- savesvd$v
        d.svd <- savesvd$d
        maxd <- max(d.svd)
        w <- ifelse((d.svd/maxd) < eps, rep(0, length(d.svd)), 
            1/d.svd)
        rank <- sum(d.svd/maxd >= eps)
        Ginv <- V.svd %*% diag(w) %*% t(U.svd)
    }
    else {
        Ginv <- ifelse(x < eps, 0, 1/x)
        rank <- ifelse(x < eps, 0, 1)
    }
    #list(Ginv = Ginv, rank = rank)
    return(Ginv)
}

GetQa=function(au,U,X,C=NULL){
  svdinv=LargeInv
  mk=length(U)
  n=nrow(X)
  if(mk>0)VI=Woodbury(au,U)

  else if(mk==0)VI=diag(1/au,n)
  if(ncol(X)==1)VX=VI%*%X[,1]
  else VX=VI%*%X
  if(is.null(C))Qa=VI-VX%*%ginv(t(X)%*%VX)%*%t(VX)
  else Qa=VI-VX%*%svdinv(t(X)%*%VX+C%*%t(C))%*%t(VX)
  res=list(Qa=Qa,VI=VI)
  return(res)

}

## Calculate the left side of MINQUE normal equations
## Needed Dec 13, 2019

MINQUE_L=function(Qa,U){
  mk=length(U)
  ML=matrix(0, nrow=mk+1,ncol=mk+1)
  if(mk>0){
  for(i in 1:mk){
     for(j in i:mk){
        a1=t(U[[i]])%*%Qa%*%U[[j]]
        a=a1%*%t(a1)
        b=sum(diag(a))
        ML[i,j]=ML[j,i]=b[1]
     }
     a1=t(U[[i]])%*%Qa
     a=a1%*%t(a1)
     b=sum(diag(a))
     ML[i,(mk+1)]=ML[(mk+1),i]=b[1]
  }
  }

  a1=Qa
  a=a1%*%t(a1)
  b=sum(diag(a))
  ML[(mk+1),(mk+1)]=b[1]
   
  return(ML)
}


## calculate the right side of MINQUE normal equations
## Needed Dec 13, 2019

MINQUE_R=function(Qa,U,y){
  mk=length(U)
  MR<-numeric()
  y=as.numeric(y)
  if(mk>0){
     for(i in 1:mk){
       #i=1
       t=t(y)%*%Qa%*%U[[i]]
       a=t%*%t(t)
       MR[i]=a[1,1]
     }
  }
  t=t(y)%*%Qa
  a=t%*%t(t)
  MR[mk+1]=a[1,1]
  return(MR)
}

##y=YD[,1]
GetVarPre=function(Qa,U,X,ML,y){
   svdinv=LargeInv
   MR=MINQUE_R(Qa,U,y)
   V0=svdinv(ML)%*%MR


   v=as.vector(V0)
   mk=length(U)
   n=nrow(X)
   if(mk==0)VI=diag(1,n)
   else VI=Woodbury(v,U) 
   X0=as.matrix(X)                      
   ##Estimate fixed effects by GLSE
   if(ncol(X)==1) bhat=ginv(t(X[,1])%*%VI%*%X[,1])%*%(t(X[,1])%*%VI%*%y)  ## I don't know why, but it works this way
   else if(ncol(X)>1)bhat=ginv(t(X0)%*%VI%*%X0)%*%(t(X0)%*%VI%*%y)
   
   #n=ncol(U[[1]])
   #mk=length(U)
   #v=as.vector(V0)
   if(mk>0){
     k=numeric()
     for(i in 1:mk){
       nu=ncol(U[[i]])-1
       if(v[i]<=0){
          k[i]=0
          ##v[i]=0
       }
       else if(v[i]>0)k[i]=sqrt(nu*v[i]/MR[i])
     }
     PRE=numeric()
     vc=numeric()

     for(i in 1:mk){
      if(k[i]==0){
          es=rep(0,ncol(U[[i]]))
          PRE=c(PRE,es)
      }
      else if(k[i]>0){
         es<-k[i]*t(U[[i]])%*%Qa%*%y
         #es<-t(U[[i]])%*%Qa%*%y
         PRE=c(PRE,es)
      }
      #vc[i]=var(es)
    }
   }
   if(mk==0)PRE=NULL
   result=list(v=v,b=bhat,Pre=PRE)
   return(result)
}

GetFit=function(U,X,b,Pre){
     fit=X%*%b
     #Pre=PRE0
     mk=length(U)
     BigU=U[[1]]
     for(i in 2:mk)BigU=cbind(BigU,U[[i]])
     fit=fit+BigU%*%Pre
     return (fit)
     #dim(BigU)
}

#######################################################################################
####### Function to Drop the columns of any matrix in which all elements are zero.#####
#######################################################################################

REML=function(U,X,y,ITMAX=NULL,...){
    #U=U1
    #y=yd
    if(is.null(ITMAX))ITMAX=5
    SMALLVALUE=0.001
    DIS=100
    it=1
    mk=length(U)
    v0=rep(1,mk+1)
    S=which(v0<=0)
    v0[S]=0

    while(DIS>SMALLVALUE&&it<=ITMAX){
        au=v0
        Qa=GetQa(au,U,X)$Qa
        ML=MINQUE_L(Qa,U)
        result=GetVarPre(Qa,U,ML,y)
        v1=result[[1]]
        S=which(v1<=0)
        v1[S]=0
        if(it==1)ITV=v1
        else if(it>1)ITV=cbind(ITV,v1)
        it=it+1
        d=v0-v1
        DIS=sqrt(mean(d^2)/sum(v0))
        v0=v1
    }
    Pre=result[[2]]
    res=list(v1,Pre)
    return(res)
}

## Needed Dec 13, 2019
Woodbury=function(au=NULL,U,...){
   svdinv=LargeInv
   mk=length(U)
   if(is.null(au))au=rep(1,mk+1)
   n=nrow(U[[1]])
   V=diag(1/(au[mk+1]),n)
   for(i in 1:mk){
     a=au[i]
     if(a>0){
        c=ncol(U[[i]])
        m1=V%*%U[[i]]
        a=au[i]
        d=diag(1,c)
        vi=d+a*t(U[[i]])%*%m1
        vi=svdinv(vi)
        VI=V-a*m1%*%vi%*%t(m1)
        V=VI
     }
   }
   VI=V
   return(VI)
}
#U=gdata$U


## Needed Dec 13, 2019

SimuData=function(gdata,v,b,SimuNum=NULL){
   if(is.null(SimuNum))SimuNum=200
   gdata=SimuData_Old(gdata,v,b,SimuNum)
   return(gdata)
}

## Needed Dec 13, 2019

SimuData_Old=function(gdata,v,b,SimuNum=NULL){
  if(is.null(SimuNum))SimuNum=200
  U=gdata$U
  nk=length(gdata$X)
  if(nk==1)X=gdata$X[[1]]
  else{
    for(i in 1:nk){
       if(i==1)X=gdata$X[[i]]
       else X=cbind(X,gdata$X[[i]])
    }
  }
  mk=length(U)
  n=nrow(U[[1]])
  c=numeric()
  for(i in 1:mk){
     c[i]=ncol(U[[i]])
     if(i==1)BigU=U[[i]]
     else if(i>1)BigU=cbind(BigU,U[[i]])
  }
  dim(X)
  mt=sum(c)
  #b=as.matrix(b)
  RE0=matrix(0,nrow=mt,ncol=SimuNum)
  YS=matrix(0,nrow=n,ncol=SimuNum)
  for(i in 1:SimuNum){
     #i=1
     for(j in 1:mk){
        #j=1
        if(v[j]>0)a=rnorm(c[j],0,sqrt(v[j]))
        if(v[j]<=0)a=rep(0,c[j])
        if(j==1)RE=a
        else if(j>1)RE=c(RE,a)
     }
     RE0[,i]=RE
     #RE=as.matrix(RE)
     YS[,i]=X%*%b+BigU%*%RE+rnorm(n,0,sqrt(v[mk+1]))
     
  }
  #res=list(SimuY=YS,SimuEffect=RE0)
  gdata$Y=YS
  gdata$SimuEffect=RE0
  gdata$VC=rep(1,mk+1)
  return(gdata)
}

## Needed Dec 13, 2019
svdinv=function(A){
   return(LargeInv(A))
}


##A function used for Monte Carlo simulations

## Needed Dec 13, 2019
genmod.simu=function(gdata,v,b,SimuNum,JacNum=NULL,JacRep=NULL,ALPHA=NULL,...){
   if(is.null(JacNum))JacNum=10
   if(is.null(JacRep))JacRep=1
   if(is.null(ALPHA))ALPHA=0.05
   result=genmod.simuold(gdata,v,b,SimuNum,JacNum,JacRep,ALPHA)
   return(result)
}


##A function used for Monte Carlo simulations

## Needed Dec 13, 2019

genmod.simuold=function(gdata,v,b,SimuNum=NULL,JacNum=NULL,JacRep=NULL,ALPHA=NULL,...){
   if(is.null(SimuNum))SimuNum=200
   if(is.null(ALPHA))ALPHA=0.05
   gdata=SimuData(gdata,v=v,b=b,SimuNum=SimuNum)
   if(is.null(JacNum))JacNum=10
   if(is.null(JacRep))JacRep=1
   jac=genmod.jack(gdata,JacNum=JacNum,JacRep=JacRep,LUP=1)
   ml=length(v)
   Var=jac$Var
   P=numeric(ml)
   vm=numeric(ml)
   for(i in 1:SimuNum){
      vm=vm+Var[[i]][,1]
      id=which(Var[[i]][,3]<=ALPHA)
      P[id]=P[id]+1
   }
   vm=vm/SimuNum
   P=P/SimuNum
   bias=vm-v
   a=data.frame(v,vm,bias,P)
   colnames(a)=c("True","Estimate","Bias","Power")
   rownames(a)=rownames(Var[[1]])
   res=list(P=a,ALPHA=ALPHA)
   return(res)
}



#gdata=mod
## Needed Dec 13, 2019

print.genmod.data=function(gdata,...){
   cat("\nCall:\n")
   print(gdata$call)

   cat("\nThe information data are very complicated. We only provide summarized information as follows:\n")
   
   cat("\nSample size=", nrow(gdata$Y),"\n")
   cat("\nNo of traits=", ncol(gdata$Y),"\n")
  
   cat("\nNo of information matrices X for fixed effects is: ", length(gdata$X), "\n")
   mk=length(gdata$U)
   if(mk==0) cat("\nNo matrix for ramdom effects available\n")
   else if (mk>0) cat("\nNo of information matrices for random effects is:", length(gdata$U), "\n")
   
}


## Needed Dec 13, 2019
minque.jack=function(Y,X,U,JacNum,JacRep,VC=NULL,C=NULL,VNames=NULL,Method=NULL,ALPHA=NULL,LUP=NULL,...){
   if(is.null(LUP))LUP=NULL
   #result=minque.jack1(Y,X,U,JacNum,JacRep,VC,C,VNames,Method,ALPHA)
   #else 
   result=minque.jack1(Y,X,U,JacNum,JacRep,VC,C,VNames,Method,ALPHA,LUP)
   return(result)
}

## Needed Dec 13, 2019
minque.jack1=function(Y,X,U,JacNum,JacRep,VC=NULL,C=NULL,VNames=NULL,Method=NULL,ALPHA=NULL,LUP=NULL,...){
   n=nrow(Y)
   mk=length(U)
   if(is.null(VC))VC=rep(1,mk+1)
   if(is.null(VNames)){
      if(mk>0)VNames=c(paste("Var(",names(U),")",sep=""),"Ve")
      else VNames="Ve"
   }
   if(is.null(C))C=NULL
   SIMUYES=NULL
   
   if(is.null(Method))Method=1
   TraitNum=ncol(Y)
   TraitNames=colnames(Y)
   nx=ncol(X)
   XNames=colnames(X)
   if(length(XNames)==0)XNames=paste("B<",(1:nx),">",sep="")
   JB=rep(1:JacNum,length=n)
   if(is.null(ALPHA))ALPHA=0.05
   a=1-(1-ALPHA)^(1/(JacNum-2))
   t=qt(1-a/2,JacNum-1)
   
   #VC=vc
   ml=mk+1
   mt=0
   if(mk>0){
     for(i in 1:mk)mt=mt+ncol(U[[i]])
     Str=NULL
     for(i in 1:mk)Str=c(Str,colnames(U[[i]]))
   }

   #RES0=NULL
   #VPower=numeric(ml)
   V0=matrix(0,TraitNum,ml)
   if(mk>0)P0=matrix(0,TraitNum,mt)
   B0=matrix(0,TraitNum,ncol(X))
   #dim(U[[5]])

   if(Method==1){
      au=rep(1,ml)
      aa=GetQa(au,U,X,C)
      Qa0=aa$Qa
      if(is.null(SIMUYES)==F)VI0=aa$VI
      ML0=MINQUE_L(Qa0,U)
      if(is.null(SIMUYES)){  ##LUP method
         for(i in 1:TraitNum){
            
            MR=MINQUE_R(Qa0,U,Y[,i])
            res=MINQUEVarPre(Qa0,X,U,ML0,MR,Y[,i],C)
         
            V0[i,]=res$v*VC
            B0[i,]=res$b
            if(mk>0)P0[i,]=res$Pre
         }
     }
     else{                   ##AUP method
         for(i in 1:TraitNum){
            MR=MINQUE_R(Qa0,U,Y[,i])
            res=MINQUEVarPre(Qa0,VI0,X,U,ML0,MR,Y[,i],C)
            V0[i,]=res$v*VC
            B0[i,]=res$b
            #if(mk>0)P0[i,]=res$Pre
         }
     }
     
   }
   else if(Method==0){
       for(i in 1:TraitNum){
          res=REML(U,X,Y[,i],5)
          V0[i,]=res[[1]]*VC
          if(mk>0)P0[i,]=res[[3]]
          B0[i,]=res[[2]]
       }
   }
  
   Vp=matrix(0,TraitNum,ml)
   Cv=matrix(0,TraitNum,ml)
   Cv1=matrix(0,TraitNum,ml)
   Cv2=matrix(0,TraitNum,ml)
   Vm=matrix(0,TraitNum,ml)

   VCp=matrix(0,TraitNum,ml)
   VCv=matrix(0,TraitNum,ml)
   VCv1=matrix(0,TraitNum,ml)
   VCv2=matrix(0,TraitNum,ml)
   VCm=matrix(0,TraitNum,ml)

   if(mk>0){
      Pp=matrix(0,TraitNum,mt)
      Cp=matrix(0,TraitNum,mt)
      Cp1=matrix(0,TraitNum,mt)
      Cp2=matrix(0,TraitNum,mt)
      Pm=matrix(0,TraitNum,mt)
   }

   Cb1=matrix(0,TraitNum,nx)
   Cb2=matrix(0,TraitNum,nx)
   Cb=matrix(0,TraitNum,nx)
   Bm=matrix(0,TraitNum,nx)
   Bp=matrix(0,TraitNum,nx)
 
   
   VJR=matrix(0,nrow=ml,ncol=JacRep)
   JacVar=matrix(0,nrow=JacNum,ncol=ml)
   if(mk>0)JacPre=matrix(0,nrow=JacNum,ncol=mt)
   JacB=matrix(0,nrow=JacNum,ncol=nx)

   
   JV=matrix(0,nrow=ml,ncol=JacNum)
   if(mk>0)JP=matrix(0,nrow=mt,ncol=JacNum)
   #JB=matrix(0,nrow=nx,ncol=JacNum)

   df=JacNum-1

   JACVAR=matrix(0,TraitNum,ml)
   if(mk>0)JACPRE=matrix(0,TraitNum,mt)
   JACB=matrix(0,TraitNum,nx)

   for(k in 1:JacRep){
      #k=1
      U1=NULL
      JacRes=NULL
      JAC1=NULL
      JAC2=NULL
      JAC3=NULL
      JB=sample(JB)
      for(i in 1:JacNum){
         #i=1
         index=which(JB==i)
         if(mk==0)U1=NULL
         else if (mk>0){
            for(j in 1:mk){
              m=U[[j]]
              m=m[-index,]
              U1[[j]]=m
            }
         }
         YD=Y[-index,]
         if(TraitNum==1)YD=matrix(YD,nrow=length(YD),ncol=1) ##or YD=data.frame(YD)
         #else if(TraitNum>1)YD=Y[-index,]
         if(nx==1)X1=as.matrix(X[-index,])
         else X1=X[-index,]
         if(Method==1){
             aa=GetQa(au,U1,X1,C)
             Qa1=aa$Qa
             if(is.null(SIMUYES)==F)VI1=aa$VI
             ML1=MINQUE_L(Qa1,U1)
             if(is.null(SIMUYES)){
                for(j in 1: TraitNum){
                    
                    MR1=MINQUE_R(Qa1,U1,YD[,j])
                    res=MINQUEVarPre(Qa1,X1,U1,ML1,MR1,YD[,j],C)
                    JACVAR[j,]=res$v*VC
                    if(mk>0)JACPRE[j,]=res$Pre
                    JACB[j,]=res$b
               }
             }
             else{
                for(j in 1: TraitNum){
                    MR1=MINQUE_R(Qa1,U1,YD[,j])
                    res=MINQUEVarPre(Qa1,VI1,X1,U1,ML1,MR1,YD[,j],C)
                    JACVAR[j,]=res$v*VC
                    #if(mk>0)JACPRE[j,]=res$Pre
                    JACB[j,]=res$b
               }
             }

         }
         else if(Method==0){
            for(j in 1: TraitNum){
                res=REML(U1,X1,YD[,j],5)
                JACVAR[j,]=res[[1]]*VC
                if(mk>0)JACPRE[j,]=res[[3]]
                JACB[j,]=res[[2]]
            }

         }
         #ncol(JACVAR)
         JAC1[[i]]=JACVAR
         if(is.null(SIMUYES))if(mk>0)JAC2[[i]]=JACPRE
         #if(nx==1)JACB
         JAC3[[i]]=JACB
         #e1=e2+1
      } ## end of loop i
   
      #for(i in 1: JacNum){
      #   JV[,i]=JacNum*v0-df*JacVar[,i]
      #   JP[,i]=JacNum*pre0-df*JacPre[,i]
      #}
      for(s in 1:TraitNum){
          #s=1
          for(j in 1:JacNum){
             #j=1
             
             if(mk==0&&TraitNum==1)JacVar[j,]=JAC1[[j]]
             else JacVar[j,]=JAC1[[j]][s,]
             if(is.null(SIMUYES)){if(mk>0)JacPre[j,]=JAC2[[j]][s,]}
             if(nx==1&&TraitNum==1)JacB[j,]=JAC3[[j]]
             else JacB[j,]=JAC3[[j]][s,]

          }
          for(j in 1:JacNum){
             index=which(JacVar[j,]<0)
             JacVar[j,index]=0
          }
          ####Calculate the variance components and their proportions from the jackknife estimates
          vt=as.vector(apply(JacVar,1,sum))
          for(i in 1:ml){
             m=mean(JacVar[,i])
             se=sqrt(var(JacVar[,i]))
             if(m<=0){
               m=0
               t1=0
               p=1.0
             }
             else if(m>0){
               t1=m/se
               p=pt(-abs(t1),df)
               p=1-(1-p)^(JacNum-2)
             }
             Vp[s,i]=Vp[s,i]+p
             Vm[s,i]=Vm[s,i]+m
             Cv[s,i]=Cv[s,i]+se
             m=mean(JacVar[,i]/vt)
             se=sqrt(var(JacVar[,i]/vt))
             if(m<=0){
               m=0
               t1=0
               p=1.0
             }
             else if(m>0){
               t1=m/se
               p=pt(-abs(t1),df)
               p=1-(1-p)^(JacNum-2)
             }
             VCp[s,i]=VCp[s,i]+p
             VCm[s,i]=VCm[s,i]+m
             VCv[s,i]=VCv[s,i]+se
             
          }
          if(is.null(SIMUYES)){ 
          if(mk>0){
             for(i in 1:mt){
               m=mean(JacPre[,i])
               se=sqrt(var(JacPre[,i]))
               t1=m/se
             
               p=pt(-abs(t1),df)
               if(se<=0.0000001)p=1.0
               #p=a$p.value
               p=1-(1-p)^(JacNum-2)
               Pp[s,i]=Pp[s,i]+p
               Pm[s,i]=Pm[s,i]+m
               Cp[s,i]=Cp[s,i]+se
             }
          }
          }
          for(i in 1:nx){
             m=mean(JacB[,i])
             se=sqrt(var(JacB[,i]))
             t1=m/se
             p=pt(-abs(t1),df)
             if(se<=0.0000001)p=1.0
             #p=a$p.value
             p=1-(1-p)^(JacNum-2)
             Bp[s,i]=Bp[s,i]+p
             Bm[s,i]=Bm[s,i]+m
             Cb[s,i]=Cb[s,i]+se
          }

      }
   }
   
   ### Variance components
   Vm=Vm/JacRep
   Vp=Vp/JacRep
   Cv=Cv/JacRep
   Cv1=Vm-t*Cv
   Cv2=Vm+t*Cv

   ### Proportional variance components
   VCm=VCm/JacRep
   VCp=VCp/JacRep
   VCv=VCv/JacRep
   VCv1=VCm-t*VCv
   VCv2=VCm+t*VCv
   if(is.null(SIMUYES)){
   if(mk>0){
     ##Random effects   
     Pm=Pm/JacRep
     Pp=Pp/JacRep
     Cp=Cp/JacRep
     Cp1=Pm-t*Cp
     Cp2=Pm+t*Cp
   }
   }
   ##Fixed effects
   Bm=Bm/JacRep
   Bp=Bp/JacRep
   Cb=Cb/JacRep
   Cb1=Bm-t*Cb
   Cb2=Bm+t*Cb
   
   ##

   VAR=NULL
   PRE=NULL
   VC=NULL
   B=NULL
   CNames=c("Estimate","SE","PValue","2.5%CL","97.5%CU")
   for(i in 1: TraitNum){
      VAR[[i]]=data.frame(Vm[i,],Cv[i,],Vp[i,],Cv1[i,],Cv2[i,])
      VC[[i]]=data.frame(VCm[i,],VCv[i,],VCp[i,],VCv1[i,],VCv2[i,])
      if(is.null(SIMUYES)){if(mk>0)PRE[[i]]=data.frame(Pm[i,],Cp[i,],Pp[i,],Cp1[i,],Cp2[i,])}
      B[[i]]=data.frame(Bm[i,],Cb[i,],Bp[i,],Cb1[i,],Cb2[i,])

      rownames(VAR[[i]])=VNames
      colnames(VAR[[i]])=CNames
      rownames(VC[[i]])=paste(VNames,"/VP",sep="")
      colnames(VC[[i]])=CNames
      if(is.null(SIMUYES)){
      if(mk>0){
         rownames(PRE[[i]])=Str
         colnames(PRE[[i]])=CNames
      }
      }
      colnames(B[[i]])=CNames
      rownames(B[[i]])=XNames

   }
   #VM=numeric(ml)
   #MSE=numeric(ml)
   #if(SimuID==1){
   #   for(i in 1:ml){
   #      VPower[i]=length(which(Vp[,i]<=0.05))/TraitNum
   #      VM[i]=mean(Vm[,i])
   #      MSE[i]=svar
   #   }
   #}
   names(VAR)=TraitNames
   names(VC)=TraitNames
   if(is.null(SIMUYES)){if(mk>0)names(PRE)=TraitNames}
   names(B)=TraitNames
   ##a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,Method=Method,JacNum=JacNum,JacRep=JacRep,VNames=VNames,Y=Y,X=X,U=U)
   if(is.null(SIMUYES))a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B)##,Method=Method,JacNum=JacNum,JacRep=JacRep,VNames=VNames,Y=Y,X=X,U=U)
   else a=list(Var=VAR,PVar=VC,FixedEffect=B)
   #class(a)="jackknife.result"
   a$call=match.call()
   a$Var
   return(a)
} 


## Needed Dec13, 2019
print.minque=function(mq,...){
   cat("Call: \n")
   print(mq$call)
   cat("\n")
   
   cat("\nMinque approach for variance components and for fixed and random effects\n")

   cat("\nVariance components:\n")
   print(mq$Var)
   cat("\n")

   cat("Fixed effects:\n")
   print(mq$FixEffect)
   cat("\n")
   
   cat("Random effects:\n")
   print(mq$RandomEffect)
   cat("\n")
}

## Needed Dec 13, 2019
print.minque.jack=function(mq,...){
   cat("Call: \n")
   print(mq$call)
   cat("\n")
   
   cat("\nJackknife technique with randomized grouping\n")

   cat("\nVariance components:\n")
   print(mq$Var)
   cat("\n")

   cat("Proportional variance components:\n")
   print(mq$PVar)
   cat("\n")
   
   cat("Fixed effects:\n")
   print(mq$FixedEffect)
   cat("\n")
   
   cat("Random effects:\n")
   print(mq$RandomEffect)
   cat("\n")
}

## Needed Dec 13, 2019
get_HR=function(gdata,Qa,v){
  mk=length(gdata$U)
  h=NULL
  dim(t(gdata$U[[1]]))
  n=ncol(Qa)  
  Q=Qa
  
  for(i in 1:mk){
    m=t(gdata$U[[i]])%*%Q%*%gdata$U[[i]]
    m=m*(v[i])^2
    #m=svdinv(m)
    if(i==1)h=diag(m)
    else h=c(h,diag(m))
  }
  summary(h)
  h=sqrt(h)
  return(h)
}



##This function is used for jackknife process

reml=function(gdata,criterion=NULL){
  if(is.null(criterion))criterion=1e-3
  Y0=as.matrix(gdata$Y)
  TraitNum=ncol(Y0)
  TraitNames=colnames(Y0)
  Var=NULL
  RE=NULL
  FE=NULL
  mk=length(gdata$U)
  for(i in 1:TraitNum){
    gdata$Y=as.matrix(Y0[,i])
    res=genmod.reml(gdata,criterion)
    Var=cbind(Var,res$Var[,1])
    if(mk>0)RE=cbind(RE,res$RandomEffect[,1])
    FE=cbind(FE,res$FixEffect[,1])
  }
  res$FixEffect
  Var=as.matrix(Var)
  colnames(Var)=TraitNames
  rownames(Var)=rownames(res$Var)
  if(mk>0){
    RE=as.matrix(RE)
    colnames(RE)=TraitNames
    rownames(RE)=rownames(res$RandomEffect)
  }
  FE=as.matrix(FE)
  colnames(FE)=TraitNames
  rownames(FE)=rownames(res$FixEffect)
  res=list(Var=Var,FixedEffect=FE,RandomEffect=RE)
  gdata$Y=Y0
  return(res)
}


##This MINQUE function is used without jackknife
mq=function(gdata){
  Y0=gdata$Y
  TraitNum=ncol(Y0)
  TraitNames=colnames(Y0)
  Var=NULL
  RE=NULL
  FE=NULL
  for(i in 1:TraitNum){
    gdata$Y=as.matrix(Y0[,i])
    res=genmod(gdata)
    Var=cbind(Var,res$Var[,1])
    RE=cbind(RE,res$RandomEffect[,1])
    FE=cbind(FE,res$FixEffect[,1])
  }
  res$FixEffect
  Var=as.matrix(Var)
  colnames(Var)=TraitNames
  rownames(Var)=rownames(res$Var)
  RE=as.matrix(RE)
  colnames(RE)=TraitNames
  rownames(RE)=rownames(res$RandomEffect)
  FE=as.matrix(FE)
  colnames(FE)=TraitNames
  rownames(FE)=rownames(res$FixEffect)
  res=list(Var=Var,FixedEffect=FE,RandomEffect=RE)
  gdata$Y=Y0
  return(res)
}

## Needed Dec 13, 2019

genmod.reml.jack=function(gdata,criterion=NULL,JacNum=NULL,JacRep=NULL,ALPHA=NULL,au=NULL,LUP=NULL){
  if(is.null(criterion))criterion=1e-3
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  if(is.null(ALPHA))ALPHA=0.05
  if(is.null(au))au=NULL
  if(is.null(LUP))LUP=NULL
  a=1-(1-ALPHA)^(1/(JacNum-2))
  t=qt(1-a/2,JacNum-1)
  Y=as.matrix(gdata$Y)
  TraitNum=ncol(Y)
  TraitNames=colnames(Y)
  n=nrow(Y)
  #colnames(Y)
  mk=length(gdata$U)
  xk=length(gdata$X)
  JB=rep(1:JacNum,length=n)
  gdata0=gdata
  ml=mk+1
  nx=0
  for(i in 1:xk)nx=nx+ncol(gdata0$X[[i]])
  res0=reml(gdata0,criterion)
  V0=res0$Var
  B0=res0$FixedEffect
  P0=res0$RandomEffect
  if(mk>0){
    mt=nrow(P0)
    Str=rownames(P0)
  }
  VNames=rownames(V0)
  XNames=rownames(B0)
  
  ##############################################################################  
  #RES0=NULL
  #VPower=numeric(ml)
  V0=matrix(0,TraitNum,ml)
  if(mk>0)P0=matrix(0,TraitNum,mt)
  #colnames(P0)
  Vp=matrix(0,TraitNum,ml)
  Cv=matrix(0,TraitNum,ml)
  Cv1=matrix(0,TraitNum,ml)
  Cv2=matrix(0,TraitNum,ml)
  Vm=matrix(0,TraitNum,ml)
  
  VCp=matrix(0,TraitNum,ml)
  VCv=matrix(0,TraitNum,ml)
  VCv1=matrix(0,TraitNum,ml)
  VCv2=matrix(0,TraitNum,ml)
  VCm=matrix(0,TraitNum,ml)
  
  if(mk>0){
    Pp=matrix(0,TraitNum,mt)
    Cp=matrix(0,TraitNum,mt)
    Cp1=matrix(0,TraitNum,mt)
    Cp2=matrix(0,TraitNum,mt)
    Pm=matrix(0,TraitNum,mt)
  }
  
  Cb1=matrix(0,TraitNum,nx)
  Cb2=matrix(0,TraitNum,nx)
  Cb=matrix(0,TraitNum,nx)
  Bm=matrix(0,TraitNum,nx)
  Bp=matrix(0,TraitNum,nx)
  
  
  VJR=matrix(0,nrow=ml,ncol=JacRep)
  JacVar=matrix(0,nrow=JacNum,ncol=ml)
  if(mk>0)JacPre=matrix(0,nrow=JacNum,ncol=mt)
  JacB=matrix(0,nrow=JacNum,ncol=nx)
  
  
  JV=matrix(0,nrow=ml,ncol=JacNum)
  if(mk>0)JP=matrix(0,nrow=mt,ncol=JacNum)
  #JB=matrix(0,nrow=nx,ncol=JacNum)
  
  df=JacNum-1
  
  JACVAR=matrix(0,TraitNum,ml)
  if(mk>0)JACPRE=matrix(0,TraitNum,mt)
  JACB=matrix(0,TraitNum,nx)
  
  JACKPRE=list()
  for(k in 1:JacRep){
    #k=1
    JB=sample(JB)
    U1=NULL
    X1=list()
    JacRes=NULL
    JAC1=NULL
    JAC2=NULL
    JAC3=NULL
    jackpre=list()
    
    for(i in 1:JacNum){
      #i=1
      index=which(JB==i)
      if(mk>0){
        for(j in 1:mk){
          m=gdata0$U[[j]]
          m=m[-index,]
          U1[[j]]=m
        }
      }
      gdata$U=U1
      names(gdata$U)=names(gdata0$U)
      for(j in 1:xk){
        m=gdata0$X[[j]]
        m=as.matrix(m[-index,])
        X1[[j]]=m
        colnames(X1[[j]])=colnames(gdata0$X[[j]])
        
      }
      gdata$X=X1
      #colnames(gdata$X[[1]])
      YD=as.matrix(Y[-index,])
      colnames(YD)=colnames(Y)
      gdata$Y=YD
      res=reml(gdata,criterion)
      JACVAR=t(res$Var)
      JACB=t(res$FixedEffect)
      if(mk>0){
        JACPRE=t(res$RandomEffect)
        jackpre[[i]]=JACPRE
      }
      JAC1[[i]]=as.matrix(JACVAR)
      if(mk>0)JAC2[[i]]=as.matrix(JACPRE)
      #if(nx==1)JACB
      JAC3[[i]]=as.matrix(JACB)
      
      
    }
    if(mk>0)JACKPRE[[k]]=jackpre
    for(s in 1:TraitNum){
      #s=1
      for(j in 1:JacNum){
        if(mk==0&&TraitNum==1)JacVar[j,]=JAC1[[j]]
        else JacVar[j,]=JAC1[[j]][s,]
        if(mk>0)JacPre[j,]=JAC2[[j]][s,]
        if(nx==1&&TraitNum==1)JacB[j,]=JAC3[[j]]
        else JacB[j,]=JAC3[[j]][s,]
        
      }
      for(j in 1:JacNum){
        index=which(JacVar[j,]<0)
        JacVar[j,index]=0
      }
      ####Calculate the variance components and their proportions from the jackknife estimates
      vt=as.vector(apply(JacVar,1,sum))
      for(i in 1:ml){
        m=mean(JacVar[,i])
        se=sqrt(var(JacVar[,i]))
        if(m<=0){
          m=0
          t1=0
          p=1.0
        }
        else if(m>0){
          t1=m/se
          p=pt(-abs(t1),df)
          p=1-(1-p)^(JacNum-2)
        }
        Vp[s,i]=Vp[s,i]+p
        Vm[s,i]=Vm[s,i]+m
        Cv[s,i]=Cv[s,i]+se
        m=mean(JacVar[,i]/vt)
        se=sqrt(var(JacVar[,i]/vt))
        if(m<=0){
          m=0
          t1=0
          p=1.0
        }
        else if(m>0){
          t1=m/se
          p=pt(-abs(t1),df)
          p=1-(1-p)^(JacNum-2)
        }
        VCp[s,i]=VCp[s,i]+p
        VCm[s,i]=VCm[s,i]+m
        VCv[s,i]=VCv[s,i]+se
        
      }
      #if(is.null(SIMUYES)){ 
      if(mk>0){
        for(i in 1:mt){
          m=mean(JacPre[,i])
          se=sqrt(var(JacPre[,i]))
          t1=m/se
          
          p=pt(-abs(t1),df)
          if(se<=0.0000001)p=1.0
          #p=a$p.value
          p=1-(1-p)^(JacNum-2)
          Pp[s,i]=Pp[s,i]+p
          Pm[s,i]=Pm[s,i]+m
          Cp[s,i]=Cp[s,i]+se
        }
      }
      #}
      for(i in 1:nx){
        m=mean(JacB[,i])
        se=sqrt(var(JacB[,i]))
        t1=m/se
        p=pt(-abs(t1),df)
        if(se<=0.0000001)p=1.0
        #p=a$p.value
        p=1-(1-p)^(JacNum-2)
        Bp[s,i]=Bp[s,i]+p
        Bm[s,i]=Bm[s,i]+m
        Cb[s,i]=Cb[s,i]+se
      }
      
    }
  }
  ### Variance components
  Vm=Vm/JacRep
  Vp=Vp/JacRep
  Cv=Cv/JacRep
  Cv1=Vm-t*Cv
  Cv2=Vm+t*Cv
  
  ### Proportional variance components
  VCm=VCm/JacRep
  VCp=VCp/JacRep
  VCv=VCv/JacRep
  VCv1=VCm-t*VCv
  VCv2=VCm+t*VCv
  #if(is.null(SIMUYES)){
  if(mk>0){
    ##Random effects   
    Pm=Pm/JacRep
    Pp=Pp/JacRep
    Cp=Cp/JacRep
    Cp1=Pm-t*Cp
    Cp2=Pm+t*Cp
  }
  #}
  ##Fixed effwaects
  Bm=Bm/JacRep
  Bp=Bp/JacRep
  Cb=Cb/JacRep
  Cb1=Bm-t*Cb
  Cb2=Bm+t*Cb
  
  ##
  
  VAR=NULL
  PRE=NULL
  VC=NULL
  B=NULL
  CL=c(ALPHA/2,1-ALPHA/2)*100
  CL2=c("%LL","%UL")
  CL=paste(CL,CL2,sep="")
  CNames=c("Estimate","SE","PValue",CL)
  
  #CNames=c("Estimate","SE","PValue","2.5%CL","97.5%CU")
  for(i in 1: TraitNum){
    VAR[[i]]=data.frame(Vm[i,],Cv[i,],Vp[i,],Cv1[i,],Cv2[i,])
    VC[[i]]=data.frame(VCm[i,],VCv[i,],VCp[i,],VCv1[i,],VCv2[i,])
    if(mk>0)PRE[[i]]=data.frame(Pm[i,],Cp[i,],Pp[i,],Cp1[i,],Cp2[i,])
    B[[i]]=data.frame(Bm[i,],Cb[i,],Bp[i,],Cb1[i,],Cb2[i,])
    
    rownames(VAR[[i]])=VNames
    colnames(VAR[[i]])=CNames
    rownames(VC[[i]])=paste(VNames,"/VP",sep="")
    colnames(VC[[i]])=CNames
    #if(is.null(SIMUYES)){
    if(mk>0){
      rownames(PRE[[i]])=Str
      Rnames=CNames
      Rnames[1]="Pre"
      colnames(PRE[[i]])=Rnames
    }
    #}
    colnames(B[[i]])=CNames
    rownames(B[[i]])=XNames
    
  }
  
  names(VAR)=TraitNames
  names(VC)=TraitNames
  if(mk>0)names(PRE)=TraitNames
  names(B)=TraitNames
  ##a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,Method=Method,JacNum=JacNum,JacRep=JacRep,VNames=VNames,Y=Y,X=X,U=U)
  #if(is.null(SIMUYES))a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,JackPre=JACKPRE)##,Method=Method,JacNum=JacNum,JacRep=JacRep,VNames=VNames,Y=Y,X=X,U=U)
  #else a=list(Var=VAR,PVar=VC,FixedEffect=B)
  if(is.null(LUP))Prediction="AUP"
  else Prediction="LUP"
  #a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,JackPre=JACKPRE,Prediction=Prediction)
  #a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,Prediction=Prediction)
  a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B)
  #class(a)="jackknife.result"
  #a$call=match.call()
  #a$Var
  return(a)
}




reml.perm=function(gdata,au=NULL,PermNum,LUP=NULL,...){
   if(is.null(LUP))LUP=NULL
   if(is.null(au))au=NULL
   result0=reml(gdata)
   tn=ncol(gdata$Y)
   n=nrow(gdata$Y)
   Y0=gdata$Y
   gc=length(gdata$VC)
   mc=nrow(result0$RandomEffect)
   fc=nrow(result0$FixedEffect)
   VP=matrix(0,gc,tn)
   VT=list()
   ET=list()
   FT=list()
   for(i in 1:tn){
      vp=numeric(gc)
      ep=numeric(mc)
      fp=numeric(fc)
      y=Y0[,i]
      Y=matrix(0,n,PermNum)
      for(j in 1:PermNum)Y[,j]=sample(y)
      colnames(Y)=paste("R",1:PermNum,sep="")
      gdata$Y=Y
      result=reml(gdata)
      V=result$Var
      v0=result0$Var[,i]
      e0=result0$RandomEffect[,i]
      f0=result0$FixedEffect[,i]
      PE=result$RandomEffect
      FE=result$FixedEffect
      for(j in 1:gc){
         id=which(V[j,]>v0[j])
         vp[j]=length(id)/PermNum
      }
      for(j in 1:mc){
         if(e0[j]<0){
             id=which(PE[j,]<e0[j])
             ep[j]=length(id)/PermNum
         }
         if(e0[j]>=0){
             id=which(PE[j,]>e0[j])
             ep[j]=length(id)/PermNum
         }
      }
      for(j in 1:fc){
         if(f0[j]<0){
             id=which(FE[j,]<f0[j])
             fp[j]=length(id)/PermNum
         }
         if(f0[j]>=0){
             id=which(FE[j,]>f0[j])
             fp[j]=length(id)/PermNum
         }
      }

      vp[gc]=1-vp[gc]
      VT[[i]]=cbind(v0,vp)
      ET[[i]]=cbind(e0,ep)
      FT[[i]]=cbind(f0,fp)
      colnames(VT[[i]])=c("Est","Pvalue")
      rownames(VT[[i]])=rownames(result0$Var)
      
      colnames(ET[[i]])=c("Pre","Pvalue")
      rownames(ET[[i]])=rownames(result0$RandomEffect)
      
      colnames(FT[[i]])=c("Est","Pvalue")
      rownames(FT[[i]])=rownames(result0$FixedEffect)

  }
  names(VT)=colnames(result0$Var)
  names(ET)=colnames(result0$Var)
  
  res=list(Var=VT,RandomEffect=ET,FixedEffect=FT)
  return(res)
}

reml1.perm=function(gdata,au=NULL,PermNum=NULL,LUP=NULL,...){
   if(is.null(LUP))LUP=NULL
   if(is.null(au))au=NULL
   if(is.null(PermNum))PermNum=100
   result0=reml(gdata)
   tn=ncol(gdata$Y)
   n=nrow(gdata$Y)
   Y0=gdata$Y
   gc=length(gdata$VC)
   mc=nrow(result0$RandomEffect)
   VP=matrix(0,gc,tn)
   VT=list()
   ET=list()
   for(i in 1:tn){
      vp=numeric(gc)
      ep=numeric(mc)
      y=Y0[,i]
      Y=matrix(0,n,PermNum)
      for(j in 1:PermNum)Y[,j]=sample(y)
      colnames(Y)=paste("R",1:PermNum,sep="")
      gdata$Y=Y
      result=reml(gdata)
      V=result$Var
      v0=result0$Var[,i]
      e0=result0$RandomEffect[,i]
      PE=result$RandomEffect
      for(j in 1:gc){
         id=which(V[j,]>v0[j])
         vp[j]=length(id)/PermNum
      }
      for(j in 1:mc){
         if(e0[j]<0){
             id=which(PE[j,]<e0[j])
             ep[j]=length(id)/PermNum
         }
         if(e0[j]>=0){
             id=which(PE[j,]>e0[j])
             ep[j]=length(id)/PermNum
         }
      }
      vp[gc]=1-vp[gc]
      VT[[i]]=cbind(v0,vp)
      ET[[i]]=cbind(e0,ep)
      colnames(VT[[i]])=c("Est","Pvalue")
      rownames(VT[[i]])=rownames(result0$Var)
      
      colnames(ET[[i]])=c("Pre","Pvalue")
      rownames(ET[[i]])=rownames(result0$RandomEffect)
  }
  names(VT)=colnames(result0$Var)
  names(ET)=colnames(result0$Var)
  
  res=list(Var=VT,Pre=ET)
  return(res)
}
#SIMUYES=1

## Checked Dec 13, 2019

genmod.jack=function(gdata,JacNum=NULL,JacRep=NULL,ALPHA=NULL,au=NULL,LUP=NULL,...){
  if(is.null(JacNum))JacNum=10
  if(is.null(JacRep))JacRep=1
  if(is.null(ALPHA))ALPHA=0.05
  if(is.null(au))au=NULL
  if(is.null(LUP))LUP=NULL
  #if(is.null(SIMUYES))result=genmod.jack1(gdata=gdata,JacNum=JacNum,JacRep=JacRep,au=au,ALPHA=ALPHA)
  result=genmod.jack1(gdata,JacNum,JacRep,ALPHA,au,LUP)
}


## Checked Dec 13, 2019
genmod0=function(gdata,au=NULL,LUP=NULL){
   gdata0=gdata
   if(is.null(au))au=NULL
   if(is.null(LUP))LUP=NULL
   #return(list(au=au,SIMUYES=SIMUYES))

   res0=genmod(gdata0,au,LUP)
}
#JacNum=10
#JacRep=1

## Checked Dec 13, 2019
genmod.jack1=function(gdata,JacNum,JacRep,ALPHA,au=NULL,LUP=NULL,...){
   if(is.null(au))au=NULL
   if(is.null(LUP))LUP=NULL
   a=1-(1-ALPHA)^(1/(JacNum-2))
   t=qt(1-a/2,JacNum-1)
   Y=as.matrix(gdata$Y)
   TraitNum=ncol(Y)
   TraitNames=colnames(Y)
   n=nrow(Y)
   #colnames(Y)
   mk=length(gdata$U)
   xk=length(gdata$X)
   JB=rep(1:JacNum,length=n)
   gdata0=gdata
   ml=mk+1
   nx=0
   for(i in 1:xk)nx=nx+ncol(gdata0$X[[i]])

   res0=genmod0(gdata0,au=au,LUP=LUP)
   V0=res0$Var
   B0=res0$FixEffect
   P0=res0$RandomEffect
   if(mk>0){
      mt=nrow(P0)
      Str=rownames(P0)
   }
   VNames=rownames(V0)
   XNames=rownames(B0)

   ##############################################################################  
   #RES0=NULL
   #VPower=numeric(ml)
   V0=matrix(0,TraitNum,ml)
   if(mk>0)P0=matrix(0,TraitNum,mt)
   #colnames(P0)
   Vp=matrix(0,TraitNum,ml)
   Cv=matrix(0,TraitNum,ml)
   Cv1=matrix(0,TraitNum,ml)
   Cv2=matrix(0,TraitNum,ml)
   Vm=matrix(0,TraitNum,ml)

   VCp=matrix(0,TraitNum,ml)
   VCv=matrix(0,TraitNum,ml)
   VCv1=matrix(0,TraitNum,ml)
   VCv2=matrix(0,TraitNum,ml)
   VCm=matrix(0,TraitNum,ml)
   
   if(mk>0){
      Pp=matrix(0,TraitNum,mt)
      Cp=matrix(0,TraitNum,mt)
      Cp1=matrix(0,TraitNum,mt)
      Cp2=matrix(0,TraitNum,mt)
      Pm=matrix(0,TraitNum,mt)
   }
   
   Cb1=matrix(0,TraitNum,nx)
   Cb2=matrix(0,TraitNum,nx)
   Cb=matrix(0,TraitNum,nx)
   Bm=matrix(0,TraitNum,nx)
   Bp=matrix(0,TraitNum,nx)
 
   
   VJR=matrix(0,nrow=ml,ncol=JacRep)
   JacVar=matrix(0,nrow=JacNum,ncol=ml)
   if(mk>0)JacPre=matrix(0,nrow=JacNum,ncol=mt)
   JacB=matrix(0,nrow=JacNum,ncol=nx)

   
   JV=matrix(0,nrow=ml,ncol=JacNum)
   if(mk>0)JP=matrix(0,nrow=mt,ncol=JacNum)
   #JB=matrix(0,nrow=nx,ncol=JacNum)

   df=JacNum-1

   JACVAR=matrix(0,TraitNum,ml)
   if(mk>0)JACPRE=matrix(0,TraitNum,mt)
   JACB=matrix(0,TraitNum,nx)

   JACKPRE=list()
   for(k in 1:JacRep){
      #k=1
      JB=sample(JB)
      U1=NULL
      X1=list()
      JacRes=NULL
      JAC1=NULL
      JAC2=NULL
      JAC3=NULL
      jackpre=list()

      for(i in 1:JacNum){
         #i=1
         index=which(JB==i)
         if(mk>0){
            for(j in 1:mk){
              m=gdata0$U[[j]]
              m=m[-index,]
              U1[[j]]=m
            }
         }
         gdata$U=U1
         names(gdata$U)=names(gdata0$U)
         for(j in 1:xk){
           m=gdata0$X[[j]]
           m=as.matrix(m[-index,])
           X1[[j]]=m
           colnames(X1[[j]])=colnames(gdata0$X[[j]])

         }
         gdata$X=X1
         #colnames(gdata$X[[1]])
         YD=as.matrix(Y[-index,])
         colnames(YD)=colnames(Y)
         gdata$Y=YD
         res=genmod(gdata,au=au,LUP=LUP)
         JACVAR=t(res$Var)
         JACB=t(res$FixEffect)
         if(mk>0){
            JACPRE=t(res$RandomEffect)
            jackpre[[i]]=JACPRE
         }
         JAC1[[i]]=as.matrix(JACVAR)
         if(mk>0)JAC2[[i]]=as.matrix(JACPRE)
         #if(nx==1)JACB
         JAC3[[i]]=as.matrix(JACB)
         

      }
      if(mk>0)JACKPRE[[k]]=jackpre
      for(s in 1:TraitNum){
          #s=1
          for(j in 1:JacNum){
             if(mk==0&&TraitNum==1)JacVar[j,]=JAC1[[j]]
             else JacVar[j,]=JAC1[[j]][s,]
             if(mk>0)JacPre[j,]=JAC2[[j]][s,]
             if(nx==1&&TraitNum==1)JacB[j,]=JAC3[[j]]
             else JacB[j,]=JAC3[[j]][s,]

          }
          for(j in 1:JacNum){
             index=which(JacVar[j,]<0)
             JacVar[j,index]=0
          }
          ####Calculate the variance components and their proportions from the jackknife estimates
          vt=as.vector(apply(JacVar,1,sum))
          for(i in 1:ml){
             m=mean(JacVar[,i])
             se=sqrt(var(JacVar[,i]))
             if(m<=0){
               m=0
               t1=0
               p=1.0
             }
             else if(m>0){
               t1=m/se
               p=pt(-abs(t1),df)
               p=1-(1-p)^(JacNum-2)
             }
             Vp[s,i]=Vp[s,i]+p
             Vm[s,i]=Vm[s,i]+m
             Cv[s,i]=Cv[s,i]+se
             m=mean(JacVar[,i]/vt)
             se=sqrt(var(JacVar[,i]/vt))
             if(m<=0){
               m=0
               t1=0
               p=1.0
             }
             else if(m>0){
               t1=m/se
               p=pt(-abs(t1),df)
               p=1-(1-p)^(JacNum-2)
             }
             VCp[s,i]=VCp[s,i]+p
             VCm[s,i]=VCm[s,i]+m
             VCv[s,i]=VCv[s,i]+se
             
          }
          #if(is.null(SIMUYES)){ 
          if(mk>0){
             for(i in 1:mt){
               m=mean(JacPre[,i])
               se=sqrt(var(JacPre[,i]))
               t1=m/se
             
               p=pt(-abs(t1),df)
               if(se<=0.0000001)p=1.0
               #p=a$p.value
               p=1-(1-p)^(JacNum-2)
               Pp[s,i]=Pp[s,i]+p
               Pm[s,i]=Pm[s,i]+m
               Cp[s,i]=Cp[s,i]+se
             }
          }
          #}
          for(i in 1:nx){
             m=mean(JacB[,i])
             se=sqrt(var(JacB[,i]))
             t1=m/se
             p=pt(-abs(t1),df)
             if(se<=0.0000001)p=1.0
             #p=a$p.value
             p=1-(1-p)^(JacNum-2)
             Bp[s,i]=Bp[s,i]+p
             Bm[s,i]=Bm[s,i]+m
             Cb[s,i]=Cb[s,i]+se
          }

      }
   }
   ### Variance components
   Vm=Vm/JacRep
   Vp=Vp/JacRep
   Cv=Cv/JacRep
   Cv1=Vm-t*Cv
   Cv2=Vm+t*Cv

   ### Proportional variance components
   VCm=VCm/JacRep
   VCp=VCp/JacRep
   VCv=VCv/JacRep
   VCv1=VCm-t*VCv
   VCv2=VCm+t*VCv
   #if(is.null(SIMUYES)){
   if(mk>0){
     ##Random effects   
     Pm=Pm/JacRep
     Pp=Pp/JacRep
     Cp=Cp/JacRep
     Cp1=Pm-t*Cp
     Cp2=Pm+t*Cp
   }
   #}
   ##Fixed effwaects
   Bm=Bm/JacRep
   Bp=Bp/JacRep
   Cb=Cb/JacRep
   Cb1=Bm-t*Cb
   Cb2=Bm+t*Cb
   
   ##

   VAR=NULL
   PRE=NULL
   VC=NULL
   B=NULL
   CL=c(ALPHA/2,1-ALPHA/2)*100
   CL2=c("%LL","%UL")
   CL=paste(CL,CL2,sep="")
   CNames=c("Estimate","SE","PValue",CL)

   #CNames=c("Estimate","SE","PValue","2.5%CL","97.5%CU")
   for(i in 1: TraitNum){
      VAR[[i]]=data.frame(Vm[i,],Cv[i,],Vp[i,],Cv1[i,],Cv2[i,])
      VC[[i]]=data.frame(VCm[i,],VCv[i,],VCp[i,],VCv1[i,],VCv2[i,])
      if(mk>0)PRE[[i]]=data.frame(Pm[i,],Cp[i,],Pp[i,],Cp1[i,],Cp2[i,])
      B[[i]]=data.frame(Bm[i,],Cb[i,],Bp[i,],Cb1[i,],Cb2[i,])

      rownames(VAR[[i]])=VNames
      colnames(VAR[[i]])=CNames
      rownames(VC[[i]])=paste(VNames,"/VP",sep="")
      colnames(VC[[i]])=CNames
      #if(is.null(SIMUYES)){
        if(mk>0){
           rownames(PRE[[i]])=Str
           Rnames=CNames
           Rnames[1]="Pre"
           colnames(PRE[[i]])=Rnames

        }
      #}
      colnames(B[[i]])=CNames
      rownames(B[[i]])=XNames

   }
   
   names(VAR)=TraitNames
   names(VC)=TraitNames
   if(mk>0)names(PRE)=TraitNames
   names(B)=TraitNames
   ##a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,Method=Method,JacNum=JacNum,JacRep=JacRep,VNames=VNames,Y=Y,X=X,U=U)
   #if(is.null(SIMUYES))a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,JackPre=JACKPRE)##,Method=Method,JacNum=JacNum,JacRep=JacRep,VNames=VNames,Y=Y,X=X,U=U)
   #else a=list(Var=VAR,PVar=VC,FixedEffect=B)
   if(is.null(LUP))Prediction="AUP"
   else Prediction="LUP"
   #a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,JackPre=JACKPRE,Prediction=Prediction)
   a=list(Var=VAR,PVar=VC,RandomEffect=PRE,FixedEffect=B,Prediction=Prediction)

   #class(a)="jackknife.result"
   #a$call=match.call()
   #a$Var
   return(a)
}



#library(MASS)
#library(lattice)
#library(Matrix)
#library(agridat)
#formula=mod
#mixed.data=function(Y,formula,data=list(),...)
#formula=Brate~Geno|Geno*POS+REP
#data=brate
##lm.data(cbind(y1,y2)~1|G*B)
#formula=y~X1+X2
#data=dat
#mixed_formula(formula)
#model_data(mod1,yv)

###A function to generate a list of information matrices #######

## Checked Dec 13, 2019

GenerateU=function(mat){
  names=colnames(mat)
  r=length(names)
  U=list(r)
  for(i in 1:r){
    U[[i]]=InfMat(mat[,i])
    colnames(U[[i]])=paste(names[i],"(",colnames(U[[i]]),")",sep="")
  }
  names(U)=names
  class(U)="Umatrix"
  return(U)
}


########################################################
## A very nice function to generate a indicator matrix##

## Checked Dec 13, 2019

InfMat <- function(v)
{  
  nr=length(v)
  nr=length(v)
  #if(nr>=2000) stop("Sample size is too large, please contact the author <qgtools@gmail.com> for additional assistance")
  lv=sort(unique(v))
  nc=length(lv)
  m=matrix(0,nr,nc)
  for(i in 1:nr){
    j=which(v[i]==lv)
    m[i,j]=1
  }
  colnames(m)=lv
  return(m)
}



#### Implementation of design matrices for 4-way AD model.

#######################################################################################
#######################################################################################
#######################################################################################

## Checked Dec 13, 2019

AD4Matrix=function(Env,F1,M1,F2,M2,Gen,Block,BlockID,CC=NULL,...){
  if(is.null(CC))CC=numeric(length(Env))
  Year=Env
  #length(F1)
  F1=as.vector(F1)
  M1=as.vector(M1)
  F2=as.vector(F2)
  M2=as.vector(F2)
  TP=unique(as.vector(F1))
  TP=c(TP,unique(as.vector(M1)))
  TP=c(TP,unique(as.vector(F2)))
  TP=c(TP,unique(as.vector(M2)))
  TP=unique(TP)
  #TP=sort(unique(cbind(F1,M1,F2,M2)))
  Ty=sort(unique(Year))
  Tb=unique(Block)
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  if(BlockID==1&&b0==1)BlockID=0
  
  ##########################################
  ## Additive effects#######################
  UA=matrix(0,nrow=n,ncol=p0)
  dim(UA)
  colnames(UA)=TP
  for(i in 1:n){
    if(Gen[i]==0){
      j=which(colnames(UA)==F1[i])
      UA[i,j]=2
    }
    else if(Gen[i]>0){
      if(CC[i]==0){
        j=which(colnames(UA)==F1[i])
        UA[i,j]=UA[i,j]+0.5
        j=which(colnames(UA)==M1[i])
        UA[i,j]=UA[i,j]+0.5
        j=which(colnames(UA)==F2[i])
        UA[i,j]=UA[i,j]+0.5
        j=which(colnames(UA)==M2[i])
        UA[i,j]=UA[i,j]+0.5
      }
      else{
        j=which(colnames(UA)==F1[i])
        UA[i,j]=UA[i,j]+0.25
        j=which(colnames(UA)==M1[i])
        UA[i,j]=UA[i,j]+0.25
        j=which(colnames(UA)==F2[i])
        UA[i,j]=UA[i,j]+0.5
        j=which(colnames(UA)==M2[i])
        UA[i,j]=UA[i,j]+1.0
        
      }
      
    }
  }
  colnames(UA)=paste("A(",TP,")",sep="")
  #head(UA)
  UA=DropColumns(UA)
  
  ##################################################
  ## Dominance effects  ############################
  TD=NULL
  
  TD=paste(TP,TP,sep="*")
  
  for(i in 1:(p0-1)){
    str=paste(TP[i],TP[(i+1):p0],sep="*")
    TD=c(TD,str)
  }
  
  UD=matrix(0,nrow=n,ncol=length(TD))
  
  colnames(UD)=TD
  
  for(i in 1:n){
    if(CC[i]==0){
      if(Gen[i]==0){
        j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
        UD[i,j]=1
      }
      
      else if(Gen[i]==1){
        j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25
        
        j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25
        
        j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25
        
        j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25
      }
      
      else if(Gen[i]==2){
        j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
        
        j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
        
        j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
        
        j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
        
        j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
        
        j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
        
        j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
        
        j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/2
      }
      
      else if(Gen[i]==3){
        j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
        UD[i,j]=UD[i,j]+3/16
        
        j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
        UD[i,j]=UD[i,j]+3/16
        
        j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+3/16
        
        j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+3/16
        
        j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/4
        
        j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/4
        
        j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/4
        
        j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25/4 
      }
    }
    else{
      if(Gen[i]==0){
        j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
        UD[i,j]=1
      }
      else if(Gen[i]==1){
        j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25
        
        j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25
        
        j= which(colnames(UD)==paste(F2[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.50
        
        #j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
        #UD[i,j]=UD[i,j]+0.25
      }
      
      else if(Gen[i]==2){
        j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
        UD[i,j]=UD[i,j]+1/16
        
        j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
        UD[i,j]=UD[i,j]+1/16
        
        j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/8
        
        j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/4
        
        j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/8
        
        j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/8
        
        j= which(colnames(UD)==paste(F2[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+0.25
        
      }
      
      else if(Gen[i]==3){
        j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
        UD[i,j]=UD[i,j]+1/16+1/32
        
        j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
        UD[i,j]=UD[i,j]+1/16+1/32
        
        j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/8+1/16
        
        j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/4+1/8
        
        j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/16
        
        j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/16
        
        j= which(colnames(UD)==paste(F2[i], M2[i], sep="*"))
        UD[i,j]=UD[i,j]+1/8
        
      }
    }
    
    
    
  }
  colnames(UD)=paste("D(",TD,")",sep="")
  UD= DropColumns(UD)
  
  
  ##############################################################
  ### Environmental effects:  random effect#####################
  if(y0>0){
    ### Environmental effects ###################################
    UY=matrix(0,nrow=n,ncol=length(Ty))
    colnames(UY)= Ty
    for(i in 1:n){
      j=which(colnames(UY)==Year[i])
      UY[i,j] = 1
    }
    colnames(UY)=paste("Y(",Ty,")",sep="")
    #UY
    #dim(UY)
    UY= DropColumns(UY)
    
    
    
    ##Additive*environment interaction effects
    
    ## comments: AE interaction
    TAE=NULL
    for(i in 1:y0){
      str=paste(Ty[i],TP[1:p0],sep="*")
      TAE=c(TAE,str)
    }
    
    UAE=matrix(0,nrow=n,ncol=length(TAE))
    #UAE
    colnames(UAE)=TAE
    #head(UAE)
    
    for(i in 1:n){
      if(CC[i]==0){
        j= which(colnames(UAE)==paste(Year[i], F1[i], sep="*"))
        UAE[i, j] = UAE[i,j]+0.5
        
        j= which(colnames(UAE)==paste(Year[i], M1[i], sep="*"))
        UAE[i, j] = UAE[i,j]+0.5
        
        j= which(colnames(UAE)==paste(Year[i], F2[i], sep="*"))
        UAE[i, j] = UAE[i,j]+0.5
        
        j= which(colnames(UAE)==paste(Year[i], M2[i], sep="*"))
        UAE[i, j] = UAE[i,j]+0.5
      }
      else{
        j= which(colnames(UAE)==paste(Year[i], F1[i], sep="*"))
        UAE[i, j] = UAE[i,j]+0.25
        
        j= which(colnames(UAE)==paste(Year[i], M1[i], sep="*"))
        UAE[i, j] = UAE[i,j]+0.25
        
        j= which(colnames(UAE)==paste(Year[i], F2[i], sep="*"))
        UAE[i, j] = UAE[i,j]+0.5
        
        j= which(colnames(UAE)==paste(Year[i], M2[i], sep="*"))
        UAE[i, j] = UAE[i,j]+1.0
      }
      
    }
    colnames(UAE)=paste("AE(",TAE,")",sep="")
    #dim(UAE)
    
    UAE= DropColumns(UAE)
    #dim(UAE)
    
    ###########################################################
    ## DE interaction effects #################################
    
    TDE=NULL
    
    for(i in 1:y0){
      for(j in 1:length(TD)){
        TDE=c(TDE,paste(Ty[i],TD[j],sep="*"))
      }
    }
    
    UDE=matrix(0,nrow=n,ncol=length(TDE))
    colnames(UDE)=TDE
    
    for(i in 1:n){
      if(Gen[i]==0){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=1
        
      }
      if(CC[i]==0){
        if(Gen[i]==1){
          j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25
          
          j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25
        }
        
        else if(Gen[i]==2){
          j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
          j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
          j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25/2
          
        }
        
        else if(Gen[i]==3){
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+3/16
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+3/16
          
          
          j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+3/16
          
          
          j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+3/16
          
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
        }  
      }
      else{
        if(Gen[i]==1){
          j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.25
          
          j= which(colnames(UDE)==paste(Year[i],F2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+0.50
          
        }
        
        else if(Gen[i]==2){
          j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
          
          j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/8
          
          j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/4
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/8
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/8
          
          j= which(colnames(UDE)==paste(Year[i],F2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/4
          
          
        }
        
        else if(Gen[i]==3){
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16+1/32
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16+1/32
          
          j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/8+1/16
          
          j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/4+1/8
          
          j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
          
          j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/16
          
          j= which(colnames(UDE)==paste(Year[i],F2[i], M2[i], sep="*"))
          UDE[i,j]=UDE[i,j]+1/8
        }  
      }
    }
    colnames(UDE)=paste("DE(",TDE,")",sep="")
    UDE= DropColumns(UDE)
  }
  
  if(BlockID==1){
    if(y0==1&&b0>1){
      UB=matrix(0,nrow=n,ncol=b0)
      colnames(UB)=Tb
      for(i in 1:n){
        j=which(colnames(UB)==Block[i])
        UB[i,j]=1
      }
      colnames(UB)=paste("B(",Tb,")",sep="")
      #dim(UB)
      
      UB= DropColumns(UB)
      #dim(UB)
    }
    else if(y0>1&&b0>1){
      TB=NULL
      
      for(i in 1:y0){
        str=paste(Ty[i],Tb[1:b0],sep="*")
        TB=c(TB,str)
        
      }
      
      UB=matrix(0,nrow=n,ncol=length(TB))
      UB
      colnames(UB)=TB
      
      for(i in 1:n){
        
        j= which(colnames(UB)==paste(Year[i],Block[i],sep="*"))
        
        UB[i, j] = 1
      }
      colnames(UB)=paste("B(",TB,")",sep="")
      
      UB= DropColumns(UB)
      
    }
  }
  if(y0==1&&BlockID==0){
    U=list(UA,UD)
    names(U)=c("A","D")
  }
  else if(y0==1&&BlockID==1){
    U=list(UA,UD,UB)
    names(U)=c("A","D","B")
  }
  else if(y0>1&&BlockID==0){
    U=list(UY,UA,UD,UAE,UDE)
    names(U)=c("Y","A","D","AE","DE")
  }
  else if(y0>1&&BlockID==1){
    U=list(UY,UA,UD,UAE,UDE,UB)
    names(U)=c("Y","A","D","AE","DE","B")
  }
  class(U)="Umatrix"
  return(U)
  
}


#### Implementation of design matrices for 4-way ADC model.

#######################################################################################
#######################################################################################
#######################################################################################

ADC4Matrix=function(Env,F1,M1,F2,M2,Gen,Block,BlockID){
  Year=Env
  TP=sort(unique(c(F1,M1,F2,M2)))
  Ty=sort(unique(Year))
  Tb=sort(unique(Block))
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  if(BlockID==1&&b0==1)BlockID=0
  
  ##########################################
  ## Cytoplasmic effects#######################
  UC=matrix(0,nrow=n,ncol=p0)
  colnames(UC)=TP
  for(i in 1:n){
    j=which(colnames(UC)==F1[i])
    UC[i,j]=1
  }
  
  colnames(UC)=paste("C",TP,sep="")
  UC=DropColumns(UC)
  
  ##########################################
  ## Additive effects#######################
  UA=matrix(0,nrow=n,ncol=p0)
  dim(UA)
  colnames(UA)=TP
  for(i in 1:n){
    if(Gen[i]==0){
      j=which(colnames(UA)==F1[i])
      UA[i,j]=2
    }
    else if(Gen[i]>0){
      j=which(colnames(UA)==F1[i])
      UA[i,j]=UA[i,j]+0.5
      j=which(colnames(UA)==M1[i])
      UA[i,j]=UA[i,j]+0.5
      j=which(colnames(UA)==F2[i])
      UA[i,j]=UA[i,j]+0.5
      j=which(colnames(UA)==M2[i])
      UA[i,j]=UA[i,j]+0.5
    }
  }
  colnames(UA)=paste("A(",TP,")",sep="")
  UA=DropColumns(UA)
  
  ##################################################
  ## Dominance effects  ############################
  TD=NULL
  
  TD=paste(TP,TP,sep="*")
  
  for(i in 1:(p0-1)){
    str=paste(TP[i],TP[(i+1):p0],sep="*")
    TD=c(TD,str)
  }
  
  UD=matrix(0,nrow=n,ncol=length(TD))
  
  colnames(UD)=TD
  
  for(i in 1:n){
    if(Gen[i]==0){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=1
    }
    
    else if(Gen[i]==1){
      j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
    }
    
    else if(Gen[i]==2){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
    }
    
    else if(Gen[i]==3){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/4
      
      j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/4
      
      j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/4
      
      j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/4 
    }
  }
  colnames(UD)=paste("D(",TD,")",sep="")
  UD= DropColumns(UD)
  
  
  ##############################################################
  ### Environmental effects:  random effect#####################
  if(y0>0){
    ### Environmental effects ###################################
    UY=InfMat(Env)
    colnames(UY)=paste("E(",colnames(UY),")",sep="")
    UY=matrix(0,nrow=n,ncol=length(Ty))
    
    ##Cytoplasm*environment interaction effects
    UCE=UCE2Matrix(Env,F1)
    
    #dim(UAE)
    
    ##Additive*environment interaction effects
    
    ## comments: AE interaction
    TAE=NULL
    for(i in 1:y0){
      str=paste(Ty[i],TP[1:p0],sep="*")
      TAE=c(TAE,str)
    }
    
    UAE=matrix(0,nrow=n,ncol=length(TAE))
    #UAE
    colnames(UAE)=TAE
    #head(UAE)
    for(i in 1:n){
      j= which(colnames(UAE)==paste(Year[i], F1[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
      
      j= which(colnames(UAE)==paste(Year[i], M1[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
      
      j= which(colnames(UAE)==paste(Year[i], F2[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
      
      j= which(colnames(UAE)==paste(Year[i], M2[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
    }
    colnames(UAE)=paste("AE(",TAE,")",sep="")
    #dim(UAE)
    
    UAE= DropColumns(UAE)
    #dim(UAE)
    
    ###########################################################
    ## DE interaction effects #################################
    
    TDE=NULL
    
    for(i in 1:y0){
      for(j in 1:length(TD)){
        TDE=c(TDE,paste(Ty[i],TD[j],sep="*"))
      }
    }
    
    UDE=matrix(0,nrow=n,ncol=length(TDE))
    colnames(UDE)=TDE
    
    for(i in 1:n){
      if(Gen[i]==0){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=1
        
      }
      
      else if(Gen[i]==1){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
      }
      
      else if(Gen[i]==2){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
      }
      
      else if(Gen[i]==3){
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        
        j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        
        j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
      }  
    }
    colnames(UDE)=paste("DE(",TDE,")",sep="")
    UDE= DropColumns(UDE)
  }
  
  if(BlockID==1){
    if(y0==1)UB=InfMat(Block)
    else if(y0>1){
      B=paste(Env,Block,sep="*")
      UB=InfMat(Block)
    }
    colnames(UB)=paste("B(",colnames(UB),")",sep="")
  }
  
  if(y0==1&&BlockID==0){
    U=list(UC,UA,UD)
    names(U)=c("C","A","D")
  }
  else if(y0==1&&BlockID==1){
    U=list(UC,UA,UD,UB)
    names(U)=c("C","A","D","B")
  }
  else if(y0>1&&BlockID==0){
    U=list(UY,UC,UA,UD,UCE,UAE,UDE)
    names(U)=c("E","C","A","D","CE","AE","DE")
  }
  else if(y0>1&&BlockID==1){
    U=list(UY,UC,UA,UD,UC,UAE,UDE,UB)
    names(U)=c("Y","C","A","D","CE","AE","DE","B")
  }
  class(U)="Umatrix"
  return(U)
  
}




#### Implementation of design matrices for 4-way Marker AD model.

#######################################################################################
#######################################################################################
#######################################################################################

MarkerAD4Matrix=function(Env,MP,F1,M1,F2,M2,Gen,Block,BlockID=NULL){
  Year=Env
  TP=sort(unique(c(F1,M1,F2,M2)))
  Ty=sort(unique(Year))
  Tb=sort(unique(Block))
  Tm=sort(unique(MP))
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  if(is.null(BlockID))BlockID=0
  if(BlockID==1&&b0==1)BlockID=0
  
  ##########################################
  ## Marker Additive effects#######################
  UAm=matrix(0,nrow=n,ncol=length(Tm))
  #dim(UA)
  colnames(UAm)=Tm
  for(i in 1:n){
    j=which(colnames(UAm)==MP[F1[i]])
    UAm[i,j]=UAm[i,j]+0.5
    j=which(colnames(UAm)==MP[M1[i]])
    UAm[i,j]=UAm[i,j]+0.5
    j=which(colnames(UAm)==MP[F2[i]])
    UAm[i,j]=UAm[i,j]+0.5
    j=which(colnames(UAm)==MP[M2[i]])
    UAm[i,j]=UAm[i,j]+0.5
  }
  colnames(UAm)=paste("Am(",Tm,")",sep="")
  UAm=DropColumns(UAm)
  
  
  ##################################################
  ## Dominance effects  ############################
  TDm=NULL
  
  TDm=paste(Tm,Tm,sep="*")
  
  for(i in 1:(length(Tm)-1)){
    str=paste(Tm[i],Tm[(i+1):length(Tm)],sep="*")
    TDm=c(TDm,str)
  }
  
  UDm=matrix(0,nrow=n,ncol=length(TDm))
  
  colnames(UDm)=TDm
  
  for(i in 1:n){
    if(Gen[i]==0){
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[F1[i]], sep="*"))
      UDm[i,j]=1
    }
    
    else if(Gen[i]==1){
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25
      
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25
    }
    
    else if(Gen[i]==2){
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[F1[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[M1[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
      
      j= which(colnames(UDm)==paste(MP[F2[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
      
      j= which(colnames(UDm)==paste(MP[M2[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
      
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
      
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+0.25/2
    }
    
    else if(Gen[i]==3){
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[F1[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+3/16
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[M1[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+3/16
      
      j= which(colnames(UDm)==paste(MP[F2[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+3/16
      
      j= which(colnames(UDm)==paste(MP[M2[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+3/16
      
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+1/16
      
      j= which(colnames(UDm)==paste(MP[F1[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+1/16
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[F2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+1/16
      
      j= which(colnames(UDm)==paste(MP[M1[i]], MP[M2[i]], sep="*"))
      UDm[i,j]=UDm[i,j]+1/16
    }
  }
  colnames(UDm)=paste("Dm(",TDm,")",sep="")
  UDm= DropColumns(UDm)
  
  
  #P=c(1:10)
  #Y=c(1,2)
  #TYP=NULL
  #for(i in 1:2){
  #  TYP=c(TYP,paste(Y[i],P,sep="*"))
  #}
  
  ##########################################
  ## Additive effects#######################
  UA=matrix(0,nrow=n,ncol=p0)
  dim(UA)
  colnames(UA)=TP
  for(i in 1:n){
    if(Gen[i]==0){
      j=which(colnames(UA)==F1[i])
      UA[i,j]=2
    }
    else if(Gen[i]>0){
      j=which(colnames(UA)==F1[i])
      UA[i,j]=UA[i,j]+0.5
      j=which(colnames(UA)==M1[i])
      UA[i,j]=UA[i,j]+0.5
      j=which(colnames(UA)==F2[i])
      UA[i,j]=UA[i,j]+0.5
      j=which(colnames(UA)==M2[i])
      UA[i,j]=UA[i,j]+0.5
    }
  }
  colnames(UA)=paste("A(",TP,")",sep="")
  UA=DropColumns(UA)
  
  ##################################################
  ## Dominance effects  ############################
  TD=NULL
  
  TD=paste(TP,TP,sep="*")
  
  for(i in 1:(p0-1)){
    str=paste(TP[i],TP[(i+1):p0],sep="*")
    TD=c(TD,str)
  }
  
  UD=matrix(0,nrow=n,ncol=length(TD))
  
  colnames(UD)=TD
  
  for(i in 1:n){
    if(Gen[i]==0){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=1
    }
    
    else if(Gen[i]==1){
      j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
    }
    
    else if(Gen[i]==2){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
      
      j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25/2
    }
    
    else if(Gen[i]==3){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(F2[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(M2[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+3/16
      
      j= which(colnames(UD)==paste(F1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+1/16
      
      j= which(colnames(UD)==paste(F1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+1/16
      
      j= which(colnames(UD)==paste(M1[i], F2[i], sep="*"))
      UD[i,j]=UD[i,j]+1/16
      
      j= which(colnames(UD)==paste(M1[i], M2[i], sep="*"))
      UD[i,j]=UD[i,j]+1/16 
    }
  }
  colnames(UD)=paste("D(",TD,")",sep="")
  UD= DropColumns(UD)
  
  
  ##############################################################
  ### Environmental effects:  random effect#####################
  if(y0>0){
    ### Environmental effects ###################################
    UY=matrix(0,nrow=n,ncol=length(Ty))
    colnames(UY)= Ty
    for(i in 1:n){
      j=which(colnames(UY)==Year[i])
      UY[i,j] = 1
    }
    colnames(UY)=paste("E(",Ty,")",sep="")
    #UY
    #dim(UY)
    UY= DropColumns(UY)
    
    ##Marker additive*environment interaction effects
    
    ## comments: AmE interaction
    TAmE=NULL
    for(i in 1:y0){
      str=paste(Ty[i],Tm,sep="*")
      TAmE=c(TAmE,str)
    }
    
    UAmE=matrix(0,nrow=n,ncol=length(TAmE))
    #UAE
    colnames(UAmE)=TAmE
    #head(UAE)
    for(i in 1:n){
      j= which(colnames(UAmE)==paste(Year[i], MP[F1[i]], sep="*"))
      UAmE[i, j] = UAmE[i,j]+0.5
      
      j= which(colnames(UAmE)==paste(Year[i], MP[M1[i]], sep="*"))
      UAmE[i, j] = UAmE[i,j]+0.5
      
      j= which(colnames(UAmE)==paste(Year[i], MP[F2[i]], sep="*"))
      UAmE[i, j] = UAmE[i,j]+0.5
      
      j= which(colnames(UAmE)==paste(Year[i], MP[M2[i]], sep="*"))
      UAmE[i, j] = UAmE[i,j]+0.5
    }
    colnames(UmAE)=paste("AmE(",TAmE,")",sep="")
    #dim(UAE)
    
    UAmE= DropColumns(UAmE)
    #dim(UAE)
    
    ###########################################################
    ## DE interaction effects #################################
    
    TDmE=NULL
    
    for(i in 1:y0){
      for(j in 1:length(TDm)){
        TDmE=c(TDmE,paste(Ty[i],TDm[j],sep="*"))
      }
    }
    
    UDmE=matrix(0,nrow=n,ncol=length(TDmE))
    colnames(UDmE)=TDmE
    
    for(i in 1:n){
      if(Gen[i]==0){
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[F1[i]], sep="*"))
        UDmE[i,j]=1
        
      }
      
      else if(Gen[i]==1){
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[M2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M1[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M2[i]], MP[M2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25
      }
      
      else if(Gen[i]==2){
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[F1[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25/2
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M1[i]], MP[M1[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25/2
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F2[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25/2
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M2[i]], MP[M2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25/2
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25/2
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[M2[i]], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M1[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25/2
        
        j= which(colnames(UDmE)==paste(Year[i],M1[i], M2[i], sep="*"))
        UDmE[i,j]=UDmE[i,j]+0.25/2
        
      }
      
      else if(Gen[i]==3){
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[F1[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+3/16
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M1[i]], MP[M1[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+3/16
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F2[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+3/16
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M2[i]], MP[M2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+3/16
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+1/16
        
        j= which(colnames(UDmE)==paste(Year[i],MP[F1[i]], MP[M2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+1/16
        
        j= which(colnames(UDmE)==paste(Year[i],MP[M1[i]], MP[F2[i]], sep="*"))
        UDmE[i,j]=UDmE[i,j]+1/16
        
        j= which(colnames(UDmE)==paste(Year[i],M1[i], M2[i], sep="*"))
        UDmE[i,j]=UDmE[i,j]+1/16
        
        
      }  
    }
    colnames(UDmE)=paste("DmE(",TDmE,")",sep="")
    UDmE= DropColumns(UDmE)
    
    
    
    ##Additive*environment interaction effects
    
    ## comments: AE interaction
    TAE=NULL
    for(i in 1:y0){
      str=paste(Ty[i],TP[1:p0],sep="*")
      TAE=c(TAE,str)
    }
    
    UAE=matrix(0,nrow=n,ncol=length(TAE))
    #UAE
    colnames(UAE)=TAE
    #head(UAE)
    for(i in 1:n){
      j= which(colnames(UAE)==paste(Year[i], F1[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
      
      j= which(colnames(UAE)==paste(Year[i], M1[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
      
      j= which(colnames(UAE)==paste(Year[i], F2[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
      
      j= which(colnames(UAE)==paste(Year[i], M2[i], sep="*"))
      UAE[i, j] = UAE[i,j]+0.5
    }
    colnames(UAE)=paste("AE(",TAE,")",sep="")
    #dim(UAE)
    
    UAE= DropColumns(UAE)
    #dim(UAE)
    
    ###########################################################
    ## DE interaction effects #################################
    
    TDE=NULL
    
    for(i in 1:y0){
      for(j in 1:length(TD)){
        TDE=c(TDE,paste(Ty[i],TD[j],sep="*"))
      }
    }
    
    UDE=matrix(0,nrow=n,ncol=length(TDE))
    colnames(UDE)=TDE
    
    for(i in 1:n){
      if(Gen[i]==0){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=1
        
      }
      
      else if(Gen[i]==1){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
      }
      
      else if(Gen[i]==2){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25/2
        
      }
      
      else if(Gen[i]==3){
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        
        j= which(colnames(UDE)==paste(Year[i],F2[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        
        j= which(colnames(UDE)==paste(Year[i],M2[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/16
        
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], F2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M2[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/16
      }  
    }
    colnames(UDE)=paste("DE(",TDE,")",sep="")
    UDE= DropColumns(UDE)
  }
  
  if(BlockID==1){
    if(y0==1&&b0>1){
      UB=matrix(0,nrow=n,ncol=b0)
      colnames(UB)=Tb
      for(i in 1:n){
        j=which(colnames(UB)==Block[i])
        UB[i,j]=1
      }
      colnames(UB)=paste("B(",Tb,")",sep="")
      #dim(UB)
      
      UB= DropColumns(UB)
      #dim(UB)
    }
    else if(y0>1&&b0>1){
      TB=NULL
      
      for(i in 1:y0){
        str=paste(Ty[i],Tb[1:b0],sep="*")
        TB=c(TB,str)
        
      }
      
      UB=matrix(0,nrow=n,ncol=length(TB))
      #UB
      colnames(UB)=TB
      
      for(i in 1:n){
        
        j= which(colnames(UB)==paste(Year[i],Block[i],sep="*"))
        
        UB[i, j] = 1
      }
      colnames(UB)=paste("B(",TB,")",sep="")
      
      UB= DropColumns(UB)
      
    }
  }
  if(y0==1&&BlockID==0){
    U=list(UAm,UDm,UA,UD)
    names(U)=c("Am","Dm","A","D")
  }
  else if(y0==1&&BlockID==1){
    U=list(UAm,UDm,UA,UD,UB)
    names(U)=c("Am","Dm","A","D","B")
  }
  else if(y0>1&&BlockID==0){
    U=list(UY,UAm,UDm,UA,UD,UAmE,UDmE,UAE,UDE)
    names(U)=c("E","Am","Dm","A","D","AmE","DmE","AE","DE")
  }
  
  else if(y0>1&&BlockID==1){
    U=list(UY,UAm,UDm,UA,UD,UAmE,UDmE,UAE,UDE,UB)
    names(U)=c("E","Am","Dm","A","D","AmE","DmE","AE","DE","B")
  }
  class(U)="Umatrix"
  return(U)
}



#### Implementation of design matrices for 2-way ADM model.

#######################################################################################
#######################################################################################
#######################################################################################
ADM2Matrix=function(Env,F1,M1,Gen,Block,BlockID=NULL){
  Year=Env
  TP=sort(unique(c(F1,M1)))
  Ty=sort(unique(Year))
  Tb=sort(unique(Block))
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  if(is.null(Block))BlockID=0
  if(BlockID==1&&b0==1)BlockID=0
  GenM=Gen
  id=which(GenM>0)
  GenM[id]=GenM[id]-1
  M2=M1
  id=which(GenM==0)
  M2[id]=F1[id]
  UAm=UA2Matrix(F1,M2)
  colnames(UAm)=gsub("A","Am",colnames(UAm))
  UA=UA2Matrix(F1,M1)
  UDm=UD2Matrix(F1,M2,GenM)
  colnames(UDm)=gsub("D","Dm",colnames(UDm))
  UD=UD2Matrix(F1,M1,Gen)
  if(y0==1){
    if(BlockID==1)UB=InfMat(Block)
    colnames(UB)=paste("B(",colnames(UB),")",sep="")
    
  }
  if(y0>1){
    UY=InfMat(Env)
    colnames(UY)=paste("E(",colnames(UY),")",sep="")
    UAmE=UAE2Matrix(Env,F1,M2)
    colnames(UAmE)=gsub("A","Am",colnames(UAmE))
    UDmE=UDE2Matrix(Env,F1,M2,GenM)
    colnames(UDmE)=gsub("D","Dm",colnames(UDmE))
    UAE=UAE2Matrix(Env,F1,M1)
    UDE=UDE2Matrix(Env,F1,M1,Gen)
    if(BlockID==1){
      B=paste(Env,Block,sep="*")
      UB=InfMat(B)
      colnames(UB)=paste("B(",colnames(UB),")",sep="")
    }
  }
  
  
  if(y0==1&&BlockID==0){
    U=list(UAm,UDm,UA,UD)
    names(U)=c("Am","Dm","A","D")
  }
  else if(y0==1&&BlockID==1){
    U=list(UAm,UDm,UA,UD,UB)
    names(U)=c("Am","Dm","A","D","B")
  }
  
  else if(y0>1&&BlockID==0){
    U=list(UY,UAm,UDm,UA,UD,UAmE,UDmE,UAE,UDE)
    names(U)=c("E","Am","Dm","A","D","AmE","DmE","AE","DE")
  }
  
  else if(y0>1&&BlockID==1){
    U=list(UY,UAm,UDm,UA,UD,UAmE,UDmE,UAE,UDE,UB)
    names(U)=c("E","Am","Dm","A","D","AmE","DmE","AE","DE","B")
  }
  class(U)="Umatrix"
  
  return(U)
  
}

#### Implementation of design matrices for 2-way AD model.

#######################################################################################
#######################################################################################
#######################################################################################
#Env=dat$Loc
#F1=dat$Female
#M1=dat$Male
#Block=dat$Blk
#BlockID=1
#Gen=dat$Gen

## Checked Dec 13, 2019
AD2Matrix=function(Env,F1,M1,Gen,Block=NULL,BlockID=NULL){
  Year=Env
  F1=as.vector(F1)
  M1=as.vector(M1)
  TP=sort(unique(c(F1,M1)))
  Ty=sort(unique(Year))
  if(is.null(Block))Block=1
  Tb=sort(unique(Block))
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  if(is.null(BlockID))BlockID=0
  if(BlockID==1&&b0==1)BlockID=0
  
  UA=UA2Matrix(F1,M1)
  UD=UD2Matrix(F1,M1,Gen)
  #UAA=UAA2Matrix(F1,M1)
  if(y0==1){
    if(BlockID==1){
      UB=InfMat(Block)
      colnames(UB)=paste("B(",colnames(UB),")",sep="")
    }
    
  }
  if(y0>1){
    UY=InfMat(Env)
    colnames(UY)=paste("E(",colnames(UY),")",sep="")
    UAE=UAE2Matrix(Env,F1,M1)
    UDE=UDE2Matrix(Env,F1,M1,Gen)
    #UAAE=UAAE2Matrix(Env,F1,M1)
    if(BlockID==1){
      B=paste(Env,Block,sep="*")
      UB=InfMat(B)
      colnames(UB)=paste("B(",colnames(UB),")",sep="")
    }
  }
  
  if(y0==1&&BlockID==0){
    U=list(UA,UD)
    names(U)=c("A","D")
  }
  else if(y0==1&&BlockID==1){
    U=list(UA,UD,UB)
    names(U)=c("A","D","B")
  }
  
  else if(y0>1&&BlockID==0){
    U=list(UY,UA,UD,UAE,UDE)
    names(U)=c("E","A","D","AE","DE")
  }
  else if(y0>1&&BlockID==1){
    U=list(UY,UA,UD,UAE,UDE,UB)
    names(U)=c("E","A","D","AE","DE","B")
  }
  class(U)="Umatrix"
  return(U)
  
}

#### Implementation of design matrices for 2-way RCAD model.

#######################################################################################
#######################################################################################
#######################################################################################

RCAD2Matrix=function(Env,R,C,F1,M1,Gen,Block,BlockID){
  Year=Env
  TP=sort(unique(c(F1,M1)))
  Ty=sort(unique(Year))
  Tb=sort(unique(Block))
  Tr=sort(unique(R))
  Tc=sort(unique(C))
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  c0=length(Tc)
  r0=length(Tr)
  
  if(BlockID==1&&b0==1)BlockID=0
  
  ##########################################
  ## Additive effects#######################
  UA=matrix(0,nrow=n,ncol=p0)
  dim(UA)
  colnames(UA)=TP
  for(i in 1:n){
    if(Gen[i]==0){
      j=which(colnames(UA)==F1[i])
      UA[i,j]=2
    }
    else if(Gen[i]>0){
      j=which(colnames(UA)==F1[i])
      UA[i,j]=UA[i,j]+1
      j=which(colnames(UA)==M1[i])
      UA[i,j]=UA[i,j]+1
      
    }
  }
  colnames(UA)=paste("A(",TP,")",sep="")
  UA=DropColumns(UA)
  
  ##################################################
  ## Dominance effects  ############################
  TD=NULL
  
  TD=paste(TP,TP,sep="*")
  
  for(i in 1:(p0-1)){
    str=paste(TP[i],TP[(i+1):p0],sep="*")
    TD=c(TD,str)
  }
  
  UD=matrix(0,nrow=n,ncol=length(TD))
  
  colnames(UD)=TD
  
  for(i in 1:n){
    if(Gen[i]==0){
      j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      UD[i,j]=1
    }
    
    else if(Gen[i]==1){
      j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+1
    }
    
    else if(Gen[i]==2){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.5
      
    }
    
    else if(Gen[i]==3){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/8
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/8
      
      j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
    }
  }
  colnames(UD)=paste("D(",TD,")",sep="")
  UD= DropColumns(UD)
  
  
  ##############################################################
  ### Environmental effects:  random effect#####################
  if(y0>0){
    ### Environmental effects ###################################
    UY=matrix(0,nrow=n,ncol=length(Ty))
    colnames(UY)= Ty
    for(i in 1:n){
      j=which(colnames(UY)==Year[i])
      UY[i,j] = 1
    }
    colnames(UY)=paste("E(",Ty,")",sep="")
    #UY
    #dim(UY)
    UY= DropColumns(UY)
    
    
    ##Additive*environment interaction effects
    
    ## comments: AE interaction
    TAE=NULL
    for(i in 1:y0){
      str=paste(Ty[i],TP[1:p0],sep="*")
      TAE=c(TAE,str)
    }
    
    UAE=matrix(0,nrow=n,ncol=length(TAE))
    #UAE
    colnames(UAE)=TAE
    #head(UAE)
    for(i in 1:n){
      j= which(colnames(UAE)==paste(Year[i], F1[i], sep="*"))
      UAE[i, j] = UAE[i,j]+1
      
      j= which(colnames(UAE)==paste(Year[i], M1[i], sep="*"))
      UAE[i, j] = UAE[i,j]+1
      
    }
    colnames(UAE)=paste("AE(",TAE,")",sep="")
    #dim(UAE)
    
    UAE= DropColumns(UAE)
    #dim(UAE)
    
    ###########################################################
    ## DE interaction effects #################################
    
    TDE=NULL
    
    for(i in 1:y0){
      for(j in 1:length(TD)){
        TDE=c(TDE,paste(Ty[i],TD[j],sep="*"))
      }
    }
    
    UDE=matrix(0,nrow=n,ncol=length(TDE))
    colnames(UDE)=TDE
    
    for(i in 1:n){
      if(Gen[i]==0){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=1
        
      }
      
      else if(Gen[i]==1){
        j= which(colnames(UDE)==paste(Year[i],F1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1
      }
      
      else if(Gen[i]==2){
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.25
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+0.5
      }
      
      else if(Gen[i]==3){
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/8
        
        j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+3/8
        
        
        j= which(colnames(UDE)==paste(Year[i],F1[i], M1[i], sep="*"))
        UDE[i,j]=UDE[i,j]+1/4
      }  
    }
    colnames(UDE)=paste("DE(",TDE,")",sep="")
    UDE= DropColumns(UDE)
  }
  
  if(BlockID==1){
    if(y0==1&&b0>1){
      UB=matrix(0,nrow=n,ncol=b0)
      colnames(UB)=Tb
      for(i in 1:n){
        j=which(colnames(UB)==Block[i])
        UB[i,j]=1
      }
      colnames(UB)=paste("B(",Tb,")",sep="")
      #dim(UB)
      
      UB= DropColumns(UB)
      #dim(UB)
    }
    else if(y0>1&&b0>1){
      TB=NULL
      
      for(i in 1:y0){
        str=paste(Ty[i],Tb[1:b0],sep="*")
        TB=c(TB,str)
        
      }
      
      UB=matrix(0,nrow=n,ncol=length(TB))
      #UB
      colnames(UB)=TB
      
      for(i in 1:n){
        
        j= which(colnames(UB)==paste(Year[i],Block[i],sep="*"))
        
        UB[i, j] = 1
      }
      colnames(UB)=paste("B(",TB,")",sep="")
      
      UB= DropColumns(UB)
      
    }
  }
  
  
  if(y0==1){
    UR=matrix(0,nrow=n,ncol=r0)
    colnames(UR)=Tr
    UC=matrix(0,nrow=n,ncol=c0)
    colnames(UC)=Tc
    for(i in 1:n){
      j=which(colnames(UR)==R[i])
      UR[i,j]=1
      j=which(colnames(UC)==C[i])
      UC[i,j]=1
    }
    colnames(UR)=paste("Row(",Tr,")",sep="")
    colnames(UC)=paste("Col(",Tc,")",sep="")
    #dim(UB)
    
    UC=DropColumns(UC)
    UR=DropColumns(UR)
    
    #dim(UB)
  }
  
  else if(y0>1){
    TR=NULL
    TC=NULL
    
    for(i in 1:y0){
      TR=c(TR,paste(Ty[i],Tr,sep="*"))
      TC=c(TC,paste(Ty[i],Tc,sep="*"))
    }
    
    UR=matrix(0,nrow=n,ncol=length(TR))
    UC=matrix(0,nrow=n,ncol=length(TC))
    colnames(UR)=TR
    colnames(UC)=TC
    
    for(i in 1:n){
      j=which(colnames(UR)==paste(Year[i],R[i],sep="*"))
      UR[i,j]=1
      j=which(colnames(UC)==paste(Year[i],C[i],sep="*"))
      UC[i,j]=1
    }
    
    colnames(UR)=paste("Row(",TR,")",sep="")
    colnames(UC)=paste("Col(",TR,")",sep="")
    
    UC=DropColumns(UC)
    UR=DropColumns(UR)
  }
  
  if(y0==1&&BlockID==0){
    U=list(UR,UC,UA,UD)
    names(U)=c("R","C","A","D")
  }
  else if(y0==1&&BlockID==1){
    U=list(UR,UC,UA,UD,UB)
    names(U)=c("R","C","A","D","B")
  }
  else if(y0>1&&BlockID==0){
    U=list(UY,UR,UC,UA,UD,UAE,UDE)
    names(U)=c("E","R","C","A","D","AE","DE")
  }
  
  else if(y0>1&&BlockID==1){
    U=list(UY,UR,UC,UA,UD,UAE,UDE,UB)
    names(U)=c("E","R","C","A","D","AE","DE","B")
  }
  class(U)="Umatrix"
  return(U)
  
}


#### Implementation of design matrices for 2-way ADC model.

#######################################################################################
#######################################################################################
#######################################################################################

ADC2Matrix=function(Env,F1,M1,Gen,Block,BlockID){
  
  Year=Env
  TP=sort(unique(c(F1,M1)))
  Ty=sort(unique(Year))
  Tb=sort(unique(Block))
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  if(BlockID==1&&b0==1)BlockID=0
  UC=UC2Matrix(F1)
  UA=UA2Matrix(F1,M1)
  UD=UD2Matrix(F1,M1,Gen)
  #UAA=UAA2Matrix(F1,M1)
  if(y0==1){
    if(BlockID==1)UB=InfMat(Block)
    colnames(UB)=paste("B(",colnames(UB),")",sep="")
    
  }
  if(y0>1){
    UY=InfMat(Env)
    colnames(UY)=paste("E(",colnames(UY),")",sep="")
    UCE=UCE2Matrix(Env,F1)
    UAE=UAE2Matrix(Env,F1,M1)
    UDE=UDE2Matrix(Env,F1,M1,Gen)
    #UAAE=UAAE2Matrix(Env,F1,M1)
    if(BlockID==1){
      B=paste(Env,Block,sep="*")
      UB=InfMat(B)
      colnames(UB)=paste("B(",colnames(UB),")",sep="")
    }
  }
  if(y0==1&&BlockID==0){
    U=list(UC,UA,UD)
    names(U)=c("C","A","D")
  }
  else if(y0==1&&BlockID==1){
    U=list(UC,UA,UD,UB)
    names(U)=c("C","A","D","B")
  }
  else if(y0>1&&BlockID==0){
    U=list(UY,UC,UA,UD,UCE,UAE,UDE)
    names(U)=c("E","C","A","D","CE","AE","DE")
  }
  else if(y0>1&&BlockID==1){
    U=list(UY,UC,UA,UD,UCE,UAE,UDE,UB)
    names(U)=c("E","C","A","D","CE","AE","DE","B")
  }
  class(U)="Umatrix"      
  return(U)
  
}


#### Implementation of design matrices for 2-way ADAA model.

#######################################################################################
#######################################################################################
#######################################################################################

## Checked Dec 13, 2019
ADAA2Matrix=function(Env,F1,M1,Gen,Block,BlockID){
  Year=Env
  #F=as.vector(F1)
  TP=c(F1,M1)
  F1=as.vector(F1)
  M1=as.vector(M1)
  
  TP=unique(c(F1,M1))
  Ty=sort(unique(Year))
  Tb=sort(unique(Block))
  
  n=length(F1)
  
  y0=length(Ty)
  p0=length(TP)
  b0=length(Tb)
  if(BlockID==1&&b0==1)BlockID=0
  UA=UA2Matrix(F1,M1)
  UD=UD2Matrix(F1,M1,Gen)
  UAA=UAA2Matrix(F1,M1)
  if(y0==1){
    if(BlockID==1)UB=InfMat(Block)
    colnames(UB)=paste("B(",colnames(UB),")",sep="")
    
  }
  if(y0>1){
    UY=InfMat(Env)
    colnames(UY)=paste("E(",colnames(UY),")",sep="")
    UAE=UAE2Matrix(Env,F1,M1)
    UDE=UDE2Matrix(Env,F1,M1,Gen)
    UAAE=UAAE2Matrix(Env,F1,M1)
    if(BlockID==1){
      B=paste(Env,Block,sep="*")
      UB=InfMat(B)
      colnames(UB)=paste("B(",colnames(UB),")",sep="")
    }
  }
  if(y0==1&&BlockID==0){
    U=list(UA,UD,UAA)
    names(U)=c("A","D","AA")
  }
  else if(y0==1&&BlockID==1){
    U=list(UA,UD,UAA,UB)
    names(U)=c("A","D","AA","B")
  }
  else if(y0>1&&BlockID==0){
    U=list(UY,UA,UD,UAA,UAE,UDE,UAAE)
    names(U)=c("E","A","D","AA","AE","DE","AAE")
  }
  else if(y0>1&&BlockID==1){
    U=list(UY,UA,UD,UAA,UAE,UDE,UAAE,UB)
    names(U)=c("E","A","D","AA","AE","DE","AAE","B")
  }
  class(U)="Umatrix"
  return(U)
}





#P=c(as.vector(unique(F1)),as.vector(unique(M1)))
#unique(P)

## Checked Dec 13, 2019

UA2Matrix=function(F1,M1){
  TP=unique(c(F1,M1))
  n=length(F1)
  p0=length(TP)
  TA=TP
  UA=matrix(0,nrow=n,ncol=length(TA))
  colnames(UA)=TA
  #head(UAE)
  for(i in 1:n){
    j= which(colnames(UA)==F1[i])
    UA[i, j] = UA[i,j]+1
    
    j= which(colnames(UA)==M1[i])
    UA[i, j] = UA[i,j]+1
    
  }
  colnames(UA)=paste("A(",TA,")",sep="")
  #dim(UAE)
  UA= DropColumns(UA)
  return(UA)
}

## Checked Dec 13, 2019

UD2Matrix=function(F1,M1,Gen){
  TP=sort(unique(c(F1,M1)))
  n=length(F1)
  F=M=numeric(n)
  for(i in 1:n){
    if(F1[i]<=M1[i]){
      F[i]=F1[i]
      M[i]=M1[i]
    }
    else{
      F[i]=M1[i]
      M[i]=F1[i]
    }
  }
  F1=F
  M1=M
  p0=length(TP)
  #TD=NULL
  TD=paste(TP,TP,sep="*")
  str=unique(paste(F1,M1,sep="*"))
  #for(i in 1:(p0-1)){
  #    str=paste(TP[i],TP[(i+1):p0],sep="*")
  #    TD=c(TD,str)
  #}
  TD=c(TD,str)
  TD=unique(TD)
  UD=matrix(0,nrow=n,ncol=length(TD))
  colnames(UD)=TD
  
  for(i in 1:n){
    if(Gen[i]==0){
      j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      UD[i,j]=1
    }
    
    else if(Gen[i]==1){
      if(F1[i]<M1[i])j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      else j= which(colnames(UD)==paste(M1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+1
      
    }
    
    else if(Gen[i]==2){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      if(F1[i]<M1[i])j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      else j= which(colnames(UD)==paste(M1[i], F1[i], sep="*"))
      #j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      #UD[i,j]=UD[i,j]+0.5
      #j= which(colnames(UD)==paste(M1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.5
      
      
    }
    
    else if(Gen[i]==3){
      j= which(colnames(UD)==paste(F1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/8
      
      j= which(colnames(UD)==paste(M1[i], M1[i], sep="*"))
      UD[i,j]=UD[i,j]+3/8
      
      if(F1[i]<M1[i])j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      else j= which(colnames(UD)==paste(M1[i], F1[i], sep="*"))
      #j= which(colnames(UD)==paste(F1[i], M1[i], sep="*"))
      #UD[i,j]=UD[i,j]+0.25
      #j= which(colnames(UD)==paste(M1[i], F1[i], sep="*"))
      UD[i,j]=UD[i,j]+0.25
      
      
    }
  }
  colnames(UD)=paste("D(",TD,")",sep="")
  UD= DropColumns(UD)
  return(UD)
}

##data.frame(F1,M1)
UAA2Matrix=function(F1,M1){
  TP=sort(unique(c(F1,M1)))
  n=length(F1)
  p0=length(TP)
  F=M=numeric(n)
  for(i in 1:n){
    if(F1[i]<=M1[i]){
      F[i]=F1[i]
      M[i]=M1[i]
    }
    else{
      F[i]=M1[i]
      M[i]=F1[i]
    }
  }
  F1=F
  M1=M
  p0=length(TP)
  #TD=NULL
  TD=paste(TP,TP,sep="*")
  str=unique(paste(F1,M1,sep="*"))
  #TD=paste(TP,TP,sep="*")
  #for(i in 1:(p0-1)){
  #    str=paste(TP[i],TP[(i+1):p0],sep="*")
  #    TD=c(TD,str)
  #}
  TD=c(TD,str)
  TD=unique(TD)
  UAA=matrix(0,nrow=n,ncol=length(TD))
  colnames(UAA)=TD
  
  for(i in 1:n){
    #i=11
    j= which(colnames(UAA)==paste(F1[i], F1[i], sep="*"))
    UAA[i,j]=1
    j= which(colnames(UAA)==paste(M1[i], M1[i], sep="*"))
    UAA[i,j]=UAA[i,j]+1
    if(F1[i]<M1[i])j= which(colnames(UAA)==paste(F1[i], M1[i], sep="*"))
    else j= which(colnames(UAA)==paste(M1[i], F1[i], sep="*"))
    #j= which(colnames(UAA)==paste(F1[i], M1[i], sep="*"))
    UAA[i,j]=UAA[i,j]+2
  }
  #sum(UAA[,11])
  colnames(UAA)=paste("AA(",TD,")",sep="")
  UAA= DropColumns(UAA)
  return(UAA)
}



UAE2Matrix=function(Env,F1,M1){
  Year=Env
  TP=sort(unique(c(F1,M1)))
  Ty=sort(unique(Year))
  n=length(F1)
  y0=length(Ty)
  p0=length(TP)
  TAE=NULL
  for(i in 1:y0){
    str=paste(Ty[i],TP[1:p0],sep="*")
    TAE=c(TAE,str)
  }
  UAE=matrix(0,nrow=n,ncol=length(TAE))
  #UAE
  colnames(UAE)=TAE
  #head(UAE)
  for(i in 1:n){
    j= which(colnames(UAE)==paste(Year[i], F1[i], sep="*"))
    UAE[i, j] = UAE[i,j]+1
    
    j= which(colnames(UAE)==paste(Year[i], M1[i], sep="*"))
    UAE[i, j] = UAE[i,j]+1
    
  }
  colnames(UAE)=paste("AE(",TAE,")",sep="")
  #dim(UAE)
  UAE= DropColumns(UAE)
  return(UAE)
}


UDE2Matrix=function(Env,F1,M1,Gen){
  Year=Env
  TP=sort(unique(c(F1,M1)))
  Ty=sort(unique(Year))
  n=length(F1)
  y0=length(Ty)
  p0=length(TP)
  F=M=numeric(n)
  for(i in 1:n){
    if(F1[i]<=M1[i]){
      F[i]=F1[i]
      M[i]=M1[i]
    }
    else{
      F[i]=M1[i]
      M[i]=F1[i]
    }
  }
  F1=F
  M1=M
  p0=length(TP)
  TD=paste(TP,TP,sep="*")
  str=unique(paste(F1,M1,sep="*"))
  #TD=paste(TP,TP,sep="*")
  #for(i in 1:(p0-1)){
  #    str=paste(TP[i],TP[(i+1):p0],sep="*")
  #    TD=c(TD,str)
  #}
  TD=c(TD,str)
  TD=unique(TD)
  
  #TD=paste(TP,TP,sep="*")
  #for(i in 1:(p0-1)){
  #    str=paste(TP[i],TP[(i+1):p0],sep="*")
  #    TD=c(TD,str)
  #}
  
  TDE=NULL
  for(i in 1:y0){
    for(j in 1:length(TD)){
      TDE=c(TDE,paste(Ty[i],TD[j],sep="*"))
    }
  }
  
  UDE=matrix(0,nrow=n,ncol=length(TDE))
  colnames(UDE)=TDE
  
  for(i in 1:n){
    if(Gen[i]==0){
      j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
      UDE[i,j]=1
      
    }
    
    else if(Gen[i]==1){
      if(F1[i]<M1[i])j= which(colnames(UDE)==paste(Year[i],F1[i], M1[i], sep="*"))
      else j= which(colnames(UDE)==paste(Year[i],M1[i], F1[i], sep="*"))
      #j= which(colnames(UDE)==paste(Year[i],F1[i], M1[i], sep="*"))
      UDE[i,j]=UDE[i,j]+1
    }
    
    else if(Gen[i]==2){
      j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
      UDE[i,j]=UDE[i,j]+0.25
      
      j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
      UDE[i,j]=UDE[i,j]+0.25
      
      if(F1[i]<M1[i])j= which(colnames(UDE)==paste(Year[i],F1[i], M1[i], sep="*"))
      else j= which(colnames(UDE)==paste(Year[i],M1[i], F1[i], sep="*"))
      
      UDE[i,j]=UDE[i,j]+0.5
    }
    
    else if(Gen[i]==3){
      
      j= which(colnames(UDE)==paste(Year[i],F1[i], F1[i], sep="*"))
      UDE[i,j]=UDE[i,j]+3/8
      
      j= which(colnames(UDE)==paste(Year[i],M1[i], M1[i], sep="*"))
      UDE[i,j]=UDE[i,j]+3/8
      
      
      if(F1[i]<M1[i])j= which(colnames(UDE)==paste(Year[i],F1[i], M1[i], sep="*"))
      else j= which(colnames(UDE)==paste(Year[i],M1[i], F1[i], sep="*"))
      
      UDE[i,j]=UDE[i,j]+1/4
    }  
  }
  colnames(UDE)=paste("DE(",TDE,")",sep="")
  UDE= DropColumns(UDE)
  return(UDE)
}


UAAE2Matrix=function(Env,F1,M1){
  Year=Env
  TP=sort(unique(c(F1,M1)))
  Ty=sort(unique(Year))
  n=length(F1)
  y0=length(Ty)
  p0=length(TP)
  F=M=numeric(n)
  for(i in 1:n){
    if(F1[i]<=M1[i]){
      F[i]=F1[i]
      M[i]=M1[i]
    }
    else{
      F[i]=M1[i]
      M[i]=F1[i]
    }
  }
  F1=F
  M1=M
  p0=length(TP)
  #TD=NULL
  TAA=paste(TP,TP,sep="*")
  str=unique(paste(F1,M1,sep="*"))
  #TD=paste(TP,TP,sep="*")
  #for(i in 1:(p0-1)){
  #    str=paste(TP[i],TP[(i+1):p0],sep="*")
  #    TD=c(TD,str)
  #}
  TAA=c(TAA,str)
  TAA=unique(TAA)
  
  
  #TAA=paste(TP,TP,sep="*")
  #for(i in 1:(p0-1)){
  #    str=paste(TP[i],TP[(i+1):p0],sep="*")
  #    TAA=c(TAA,str)
  #}
  
  TAAE=NULL
  for(i in 1:y0){
    for(j in 1:length(TAA)){
      TAAE=c(TAAE,paste(Ty[i],TAA[j],sep="*"))
    }
  }
  
  UAAE=matrix(0,nrow=n,ncol=length(TAAE))
  colnames(UAAE)=TAAE
  
  for(i in 1:n){
    j= which(colnames(UAAE)==paste(Year[i],F1[i], F1[i], sep="*"))
    UAAE[i,j]=UAAE[i,j]+1
    
    j= which(colnames(UAAE)==paste(Year[i],M1[i], M1[i], sep="*"))
    UAAE[i,j]=UAAE[i,j]+1
    if(F1[i]<M1[i])j= which(colnames(UAAE)==paste(Year[i],F1[i], M1[i], sep="*"))
    else j= which(colnames(UAAE)==paste(Year[i],M1[i], F1[i], sep="*"))
    
    #j= which(colnames(UAAE)==paste(Year[i],F1[i], M1[i], sep="*"))
    UAAE[i,j]=UAAE[i,j]+2
  }
  colnames(UAAE)=paste("AAE(",TAAE,")",sep="")
  UAAE= DropColumns(UAAE)
  return(UAAE)
}

UC2Matrix=function(F1){
  UC=InfMat(F1)
  colnames(UC)=paste("C(",colnames(UC),")",sep="")
  return(UC)
}

UCE2Matrix=function(Env, F1){
  UCE=InfMat(paste(Env,F1,sep="*"))
  colnames(UCE)=paste("CE(",colnames(UCE),")",sep="")
  return(UCE)
}



## Checked Dec 13, 2019
DropColumns=function(mat){
  ti=apply(mat,2,sum)
  id=which(ti==0)
  if(length(id)>0)mat=mat[,-id]
  return(mat)
}



#isSymmetric(V)
LargeInv=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  m=ncol(A)
  if(m<=20)return(ginv(A))
  else if(m<=50)return(ginv(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  else if(m<=4000)return(subinv2000(A))
  else if(m<=8000)return(subinv4000(A))
  else if(m<=16000)return(subinv8000(A))
  else if(m<=32000)return(subinv16000(A))
  else if(m<=64000)return(subinv32000(A))
  else if(m<=128000)return(subinv64000(A))
  
  #for(i in 1:(m-1)){
  #    for(j in (i+1):m){
  #       if(A[i,j]!=A[j,i]){
  #          sys=0
  #          break
  #       }
  #    }
  #    if(sys==0)break
  #}
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000&&p<=2000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  else if(p>2000&&p<=4000){
    S10=subinv2000(S)
    P1=subinv2000(P-Q%*%S10%*%R)
  }
  else if(p>4000&&p<=8000){
    S10=subinv4000(S)
    P1=subinv4000(P-Q%*%S10%*%R)
  }
  else if (p>8000&&p<=16000){
    S10=subinv8000(S)
    P1=subinv8000(P-Q%*%S10%*%R)
  }
  else if (p>16000&&p<=32000){
    S10=subinv16000(S)
    P1=subinv16000(P-Q%*%S10%*%R)
  }
  else if (p>32000&&p<=64000){
    S10=subinv32000(S)
    P1=subinv32000(P-Q%*%S10%*%R)
  }
  else if (p>64000&&p<=128000){
    S10=subinv64000(S)
    P1=subinv64000(P-Q%*%S10%*%R)
  }
  else{
    S10=subinv128000(S)
    P1=subinv128000(P-Q%*%S10%*%R)
  }
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
  
}

subinv128000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  else if(m<=4000)return(subinv2000(A))
  else if(m<=8000)return(subinv4000(A))
  else if(m<=16000)return(subinv8000(A))
  else if(m<=32000)return(subinv16000(A))
  else if(m<=64000)return(subinv32000(A))
  else if(m<=128000)return(subinv64000(A))
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000&&p<=2000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  else if(p>2000&&p<=4000){
    S10=subinv2000(S)
    P1=subinv2000(P-Q%*%S10%*%R)
  }
  else if(p>4000&&p<=8000){
    S10=subinv4000(S)
    P1=subinv4000(P-Q%*%S10%*%R)
  }
  else if(p>8000&&p<=16000){
    S10=subinv8000(S)
    P1=subinv8000(P-Q%*%S10%*%R)
  }
  else if(p>16000&&p<=32000){
    S10=subinv16000(S)
    P1=subinv16000(P-Q%*%S10%*%R)
  }
  else if(p>32000&&p<=64000){
    S10=subinv32000(S)
    P1=subinv32000(P-Q%*%S10%*%R)
  }
  else if(p>64000){
    S10=subinv64000(S)
    P1=subinv64000(P-Q%*%S10%*%R)
  }
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}



subinv64000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  else if(m<=4000)return(subinv2000(A))
  else if(m<=8000)return(subinv4000(A))
  else if(m<=16000)return(subinv8000(A))
  else if(m<=32000)return(subinv16000(A))
  else if(m<=64000)return(subinv32000(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000&&p<=2000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  else if(p>2000&&p<=4000){
    S10=subinv2000(S)
    P1=subinv2000(P-Q%*%S10%*%R)
  }
  else if(p>4000&&p<=8000){
    S10=subinv4000(S)
    P1=subinv4000(P-Q%*%S10%*%R)
  }
  else if(p>8000&&p<=16000){
    S10=subinv8000(S)
    P1=subinv8000(P-Q%*%S10%*%R)
  }
  else if(p>16000&&p<=32000){
    S10=subinv16000(S)
    P1=subinv16000(P-Q%*%S10%*%R)
  }
  else if(p>32000){
    S10=subinv32000(S)
    P1=subinv32000(P-Q%*%S10%*%R)
  }
  
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}




subinv32000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  else if(m<=4000)return(subinv2000(A))
  else if(m<=8000)return(subinv4000(A))
  else if(m<=16000)return(subinv8000(A))
  else if(m<=32000)return(subinv16000(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000&&p<=2000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  else if(p>2000&&p<=4000){
    S10=subinv2000(S)
    P1=subinv2000(P-Q%*%S10%*%R)
  }
  else if(p>4000&&p<=8000){
    S10=subinv4000(S)
    P1=subinv4000(P-Q%*%S10%*%R)
  }
  else if(p>8000&&p<=16000){
    S10=subinv8000(S)
    P1=subinv8000(P-Q%*%S10%*%R)
  }
  else if(p>16000){
    S10=subinv16000(S)
    P1=subinv16000(P-Q%*%S10%*%R)
  }
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}




subinv16000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  else if(m<=4000)return(subinv2000(A))
  else if(m<=8000)return(subinv4000(A))
  else if(m<=16000)return(subinv8000(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000&&p<=2000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  else if(p>2000&&p<=4000){
    S10=subinv2000(S)
    P1=subinv2000(P-Q%*%S10%*%R)
  }
  else if(p>4000&&p<=8000){
    S10=subinv4000(S)
    P1=subinv4000(P-Q%*%S10%*%R)
  }
  else if(p>8000){
    S10=subinv8000(S)
    P1=subinv8000(P-Q%*%S10%*%R)
  }
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}

subinv8000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  else if(m<=4000)return(subinv2000(A))
  else if(m<=8000)return(subinv8000(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000&&p<=2000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  else if(p>2000&&p<=4000){
    S10=subinv2000(S)
    P1=subinv2000(P-Q%*%S10%*%R)
  }
  else if(p>4000){
    S10=subinv4000(S)
    P1=subinv4000(P-Q%*%S10%*%R)
  }
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}

subinv4000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  else if(m<=4000)return(subinv2000(A))
  
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000&&p<=2000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  else if(p>2000){
    S10=subinv2000(S)
    P1=subinv2000(P-Q%*%S10%*%R)
  }
  
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}




subinv2000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  else if(m<=2000)return(subinv1000(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500&&p<=1000){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  else if(p>1000){
    S10=subinv1000(S)
    P1=subinv1000(P-Q%*%S10%*%R)
  }
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}

subinv1000=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  else if(m<=1000)return(subinv500(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200&&p<=500){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  else if(p>500){
    S10=subinv500(S)
    P1=subinv500(P-Q%*%S10%*%R)
  }
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}



subinv500=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  else if(m<=500)return(subinv200(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100&&p<=200){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  else if(p>200){
    S10=subinv200(S)
    P1=subinv200(P-Q%*%S10%*%R)
  }
  
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}

subinv200=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  else if(m<=200)return(subinv100(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else if(p>50&&p<=100){
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  else if(p>100){
    S10=subinv100(S)
    P1=subinv100(P-Q%*%S10%*%R)
  }
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
  
}

#A=vi
subinv100=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  else if(m<=100)return(subinv50(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=20){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else if(p>20&&p<=50){
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  else{
    S10=subinv50(S)
    P1=subinv50(P-Q%*%S10%*%R)
  }
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
  
}


subinv50=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  else if(m<=50)return(subinv20(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  if(p<=4){
    S10=solve(S)
    P1=solve(P-Q%*%S10%*%R)
  }
  else{
    S10=subinv20(S)
    P1=subinv20(P-Q%*%S10%*%R)
  }
  
  
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
  
}

subinv20=function(A){
  if(isSymmetric(A)==TRUE)sys=1
  else sys=0
  
  m=ncol(A)
  if(m<=20)return(solve(A))
  
  p=as.integer(m/2)
  s=m-p
  P=A[1:p,1:p]
  R=A[(p+1):m,1:p]
  Q=A[1:p,(p+1):m]
  S=A[(p+1):m,(p+1):m]
  
  S10=solve(S)
  P1=solve(P-Q%*%S10%*%R)
  Q1=-P1%*%Q%*%S10
  if(sys==0)R1=-(S10%*%R)%*%P1
  S1=S10+(S10%*%R)%*%P1%*%Q%*%S10
  A1=cbind(P1,Q1)
  if(sys==0)A1=rbind(cbind(P1,Q1),cbind(R1,S1))
  else A1=rbind(cbind(P1,Q1),cbind(t(Q1),S1))
  return(A1)
}

