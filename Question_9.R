n=10
m=100
nh=4
l = rep(0,m)
l
user=c('user1','user2','user3','user4','user5','user6')
print(user)
notuser=c('user7','user8','user9','user10','user11','user12')
print(notuser)

get_hash = function(item,seed){
  hex_str=digest(object=item,
                 algo="murmur32",
                 serialize=F,
                 seed=seed)
  hex=paste('0x',hex_str,sep="")
  return(as.numeric(hex) %% m)
}

add = function(item){
  for (i in 1:nh){
    hash_digest=get_hash(item,i)
    hash_digest=hash_digest+1
    l[hash_digest]<<-1
  }
}

check = function(item){
  for(i in 1:nh){
    hash_digest=get_hash(item,i)
    hash_digest=hash_digest+1
    if(l[hash_digest] == 0)
      return(FALSE)
  }
  return(TRUE)
}

for(i in 1:n){
  add(user[i])
}

print(l)
test_set=c(user[1:4],notuser[1:4])
print(test_set)

for(i in 1:length(test_set)){
  if(check(test_set[i]))
  {
    if(test_set[i] %in% notuser)
      cat(test_set[i]," this is false positive ","\n")
    else
      cat(test_set[i]," this is probably a user ","\n")
  }
  else
    cat(test_set[i]," this is not a user !","\n")
}