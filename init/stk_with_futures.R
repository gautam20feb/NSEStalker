listSTK<-function()
{
  db<-read.csv("fo31JAN2011bhav.csv")
  t<-seq(800)-seq(800)
  for(i in 1:800)
    if ((db[i,2]==db[i+1,2])&&(db[i+1,2]==db[i+2,2])&&(db[i+2,2]!=db[i+3,2])&&(db[i,1]=="FUTSTK"))
    t[i]=1
  sum<-sum(t)
  list<-seq(sum)-seq(sum)
  k<-1
  i<-1
  while(k<(sum+1)||i<800)
  {
    if(t[i]==1)
    {
      list[k]=as.character(db[i,2])
      k=k+1
    }
    i=i+1
  }
  length(list)
  return(paste(list,collapse = ","))
  write.csv(list,"../data/stk_with_fut.csv")
}
