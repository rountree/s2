# scales vector x from a to b.
blr_scale <- function(x, s){
	a<-s[1]
	b<-s[2]
	x<-as.vector(x)
	x1<-x-min(x)
	Q<-max(x1)/(b-a)
	x2<-x1/Q
	x3<-x2s+a
	return(x3)
}
	
blr_scale2 <- function(x_vector, y_vector, x, y, scale){
	return (matrix( c( (scale * (x_vector - min(x_vector))) + x, (scale * (y_vector - min(y_vector))) + y), nrow=2, byrow=T)) ;
}

