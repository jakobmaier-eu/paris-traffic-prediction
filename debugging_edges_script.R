i = 13

str = '"'
for (elem in edges[[i]]){
  str = paste0(str, elem,'" OR "')
}
writeClipboard(paste0(str, 'asdfasdf"'))
print(names(edges)[i])
print(i)
i = i+1