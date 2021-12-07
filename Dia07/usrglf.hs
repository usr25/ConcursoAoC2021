p=foldr a[[]]
a c(y:x)|c==','=[]:y:x|0<1=(c:y):x
t n=div(n*n+n)2
c f l p=sum$map(f.abs.(p-))l
s l f=minimum$map(c f l)[0..maximum l]
main=do u<-getContents;print$s(read<$>p u)<$>[id,t]
