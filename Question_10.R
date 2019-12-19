x<-c("str1","str2","str3","str4","str4","str3","str2","str1")
x<-as.numeric(as.factor(x))
print(x)
hash<-function(var){
  return ((6*var+1)%%23)
}

toBits<-function(var){
  l=as.numeric(intToBits(var))
  index<-match(1,l)-1
  return (index)
}
H<-c()
B<-c()
for(i in 1:length(x)){
  H[i]<-hash(x[i])
  B[i]<-toBits(H[i])
}

B[is.na(B)]<-0
print(2^max(B))
