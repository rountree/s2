#Assumes blr_nas.R has already run and ep and mg data tables are created.
source("scale.R")
vx=0.2 #for boxplot

c30="turquoise"
c65="springgreen1"
c80="pink"
c95="red"

C30="dodgerblue"
C65="springgreen4"
C80="darkorange"
C95="firebrick4"

L<-length(ep$ns30) # all columns are the same length.
Wcount<-5
W<-order( mg$ns95, decreasing=F )[ (L-(Wcount-1)):L ] # bottom 24 nodes


cl_A<-c(rep(c30,L),rep(c65,L),rep(c80,L),rep(c95,L),rep(C30,L),rep(C65,L),rep(C80,L),rep(C95,L)) 
cl_B<-c(           rep(c65,L),rep(c80,L),rep(c95,L),           rep(C65,L),rep(C80,L),rep(C95,L)) 
cl_C<-c(                      rep(c80,L),rep(c95,L),                      rep(C80,L),rep(C95,L))
cl_D<-c(                                 rep(c95,L),                                 rep(C95,L))

cl_AW<-c(rep(c30,Wcount),rep(c65,Wcount),rep(c80,Wcount),rep(c95,Wcount),rep(C30,Wcount),rep(C65,Wcount),rep(C80,Wcount),rep(C95,Wcount)) 
cl_BW<-c(                rep(c65,Wcount),rep(c80,Wcount),rep(c95,Wcount),                rep(C65,Wcount),rep(C80,Wcount),rep(C95,Wcount)) 
cl_CW<-c(                                rep(c80,Wcount),rep(c95,Wcount),                                rep(C80,Wcount),rep(C95,Wcount))
cl_DW<-c(                                                rep(c95,Wcount),                                                rep(C95,Wcount))

ns_A<-c(   ep$ns30,   ep$ns65,   ep$ns80,   ep$ns95,   mg$ns30,   mg$ns65,   mg$ns80,  mg$ns95 )
ns_B<-c(              ep$ns65,   ep$ns80,   ep$ns95,              mg$ns65,   mg$ns80,  mg$ns95 )
ns_C<-c(                         ep$ns80,   ep$ns95,                         mg$ns80,  mg$ns95 )
ns_D<-c(                                    ep$ns95,                                   mg$ns95 )

ns_AW<-c(   ep$ns30[W],   mg$ns30[W] )
ns_BW<-c(   ep$ns65[W],   mg$ns65[W] )
ns_CW<-c(   ep$ns80[W],   mg$ns80[W] )
ns_DW<-c(   ep$ns95[W],   mg$ns95[W] )

# Ok, that didn't work....
#ep$fa30 <- ep$fa30 * 2.6
#ep$fa65 <- ep$fa65 * 2.6
#ep$fa80 <- ep$fa80 * 2.6
#ep$fa95 <- ep$fa95 * 2.6
#mg$fa30 <- mg$fa30 * 2.6
#mg$fa65 <- mg$fa65 * 2.6
#mg$fa80 <- mg$fa80 * 2.6
#mg$fa95 <- mg$fa95 * 2.6


fa_A<-c(   ep$fa30,   ep$fa65,   ep$fa80,   ep$fa95,   mg$fa30,   mg$fa65,   mg$fa80,  mg$fa95 ) 
fa_B<-c(              ep$fa65,   ep$fa80,   ep$fa95,              mg$fa65,   mg$fa80,  mg$fa95 ) 
fa_C<-c(                         ep$fa80,   ep$fa95,                         mg$fa80,  mg$fa95 ) 
fa_D<-c(                                    ep$fa95,                                   mg$fa95 ) 

fa_AW<-c(   ep$fa30[W],   mg$fa30[W] ) 
fa_BW<-c(   ep$fa65[W],   mg$fa65[W] ) 
fa_CW<-c(   ep$fa80[W],   mg$fa80[W] ) 
fa_DW<-c(   ep$fa95[W],   mg$fa95[W] ) 

pdf(file="s2.pdf", height=10, width=7 )
plot(x=0, y=0, xlim=c(0,7), ylim=c(0,10), pch="", xaxt="n", yaxt="n", bty="n", xlab="Normalized Slowdown", ylab="Effective Frequency", main="Processor Variation Across Several Power Bounds", sub="LLNL Cab DAT, 19 June 2014, 2386 processors"  )
legend( x=3.75, y=3, horiz=F, legend=c("ep-30W", "ep-65W", "ep-80W", "ep-95W"), col=c(c30, c65, c80, c95), pch=19, cex=1.0, pt.cex=1.2, bty="n")
legend( x=5.25, y=3, horiz=F, legend=c("mg-30W", "mg-65W", "mg-80W", "mg-95W"), col=c(C30, C65, C80, C95), pch=19, cex=1.0, pt.cex=1.2, bty="n")
legend( x=3.79, y=1.8, horiz=T, legend=c("Slowest processors, mg-95W"), pch=1, cex=0.8, pt.cex=1.2, bty="n")

pAx <- 0
pAy <- 9.1
pAs <- 2
pA <- blr_scale2( ns_A, fa_A, pAx, pAy, pAs )
p_ep_30 <- blr_scale2( c( ep$ns30, ns_A ), c( ep$fa30, fa_A ), pAx, pAy, pAs )
p_mg_30 <- blr_scale2( c( mg$ns30, ns_A ), c( mg$fa30, fa_A ), pAx, pAy, pAs )
points( pA[1,], pA[2,], pch=".", col=cl_A )
pApoly <- blr_scale2(
	c( min(ns_B), max(ns_B), max(ns_B), min(ns_B), min(ns_B), ns_A ),
	c( min(fa_B), min(fa_B), max(fa_B), max(fa_B), min(fa_B), fa_A ),
	pAx, pAy, pAs )
polygon(x=pApoly[1,1:5], y=pApoly[2,1:5], border="gray")
axis(side=2, cex.axis=0.6, 
	at=c( min(pA[2,]), max(pA[2,]) ), 
	label=c( format(min(fa_A)*2.6, digits=4), format(max(fa_A)*2.6, digits=4)))
axis(side=1, cex.axis=0.6, line=-34.4, padj=-2, 
	at=c( min(pA[1,]), max(pA[1,]) ), 
	label=c( format(min(ns_A), digits=4), format(max(ns_A), digits=4)) )
boxplot( p_ep_30[1,1:L], range=0, horizontal=T, at=8.9, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c30, frame=F)
text( x=min(p_ep_30[1,1:L]) - 0.00, y=8.9 - 0.1, labels=c(format(min(ep$ns30), digits=3) ), cex=0.5 ) 
text( x=max(p_ep_30[1,1:L]) + 0.00, y=8.9 - 0.1, labels=c(format(max(ep$ns30), digits=3) ), cex=0.5 ) 

boxplot( p_ep_30[2,1:L], range=0, horizontal=F, at=2.3, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c30, frame=F)
text( y=min(p_ep_30[2,1:L]) - 0.00, x=2.3 - 0.25, labels=c(format(min(ep$fa30)*2.6, digits=3) ), cex=0.5 ) 
text( y=max(p_ep_30[2,1:L]) - 0.00, x=2.3 - 0.25, labels=c(format(max(ep$fa30)*2.6, digits=3) ), cex=0.5 ) 

boxplot( p_mg_30[1,1:L], range=0, horizontal=T, at=9.7, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C30, frame=F)
text( x=min(p_mg_30[1,1:L]) - 0.00, y=9.7 + 0.1, labels=c(format(min(mg$ns30), digits=3) ), cex=0.5 ) 
text( x=max(p_mg_30[1,1:L]) + 0.00, y=9.7 + 0.1, labels=c(format(max(mg$ns30), digits=3) ), cex=0.5 ) 

boxplot( p_mg_30[2,1:L], range=0, horizontal=F, at=7.0, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C30, frame=F)
text( y=min(p_mg_30[2,1:L]) - 0.10, x=7.0 - 0.20, labels=c(format(min(mg$fa30)*2.6, digits=3) ), cex=0.5 ) 
text( y=max(p_mg_30[2,1:L]) - 0.00, x=7.0 - 0.20, labels=c(format(max(mg$fa30)*2.6, digits=3) ), cex=0.5 ) 

text( x=mean(p_mg_30[1,1:L]), y=mean(p_mg_30[2,1:L])+0.15, label="mg", cex=0.7 )
text( x=mean(p_ep_30[1,1:L]), y=mean(p_ep_30[2,1:L])+0.15, label="ep", cex=0.7 )


pAW <- blr_scale2( c(ns_AW, ns_A), c(fa_AW, fa_A), pAx, pAy, pAs )
points( pAW[1,1:(Wcount*2)], pAW[2,1:(Wcount*2)], pch=1, col="black", cex=0.8 )


pBx <- 0
pBy <- 4.4
pBs <- 28
pB <- blr_scale2( ns_B, fa_B, pBx, pBy, pBs )
p_ep_65 <- blr_scale2( c( ep$ns65, ns_B ), c( ep$fa65, fa_B ), pBx, pBy, pBs )
p_mg_65 <- blr_scale2( c( mg$ns65, ns_B ), c( mg$fa65, fa_B ), pBx, pBy, pBs )
p_ep_80 <- blr_scale2( c( ep$ns80, ns_B ), c( ep$fa80, fa_B ), pBx, pBy, pBs )
p_mg_80 <- blr_scale2( c( mg$ns80, ns_B ), c( mg$fa80, fa_B ), pBx, pBy, pBs )
points( pB[1,], pB[2,], pch=".", col=cl_B )
pBpoly <- blr_scale2(
	c( min(ns_D), max(ns_D), max(ns_D), min(ns_D), min(ns_D), ns_B ),
	c( min(fa_D), min(fa_D), max(fa_D), max(fa_D), min(fa_D), fa_B ),
	pBx, pBy, pBs )
polygon(x=pBpoly[1,1:5], y=pBpoly[2,1:5], border="gray")
axis(side=2, cex.axis=0.6, 
	at=c( min(pB[2,]), max(pB[2,]) ), 
	label=c( format(min(fa_B)*2.6, digits=4), format(max(fa_B)*2.6, digits=4)))
axis(side=1, cex.axis=0.6, line=-17.8, padj=-2, 
	at=c( min(pB[1,]), max(pB[1,]) ), 
	label=c( format(min(ns_B), digits=4), format(max(ns_B), digits=4)) )

### 65W

boxplot( p_ep_65[1,1:L], range=0, horizontal=T, at=6.4, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c65, frame=F)
text( x=min(p_ep_65[1,1:L]) - 0.00, y=6.4 + 0.1, labels=c(format(min(ep$ns65), digits=3) ), cex=0.5 ) 
text( x=max(p_ep_65[1,1:L]) + 0.00, y=6.4 + 0.1, labels=c(format(max(ep$ns65), digits=3) ), cex=0.5 ) 

boxplot( p_ep_65[2,1:L], range=0, horizontal=F, at=6.45, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c65, frame=F)
text( y=min(p_ep_65[2,1:L]) - 0.00, x=6.4 + 0.25, labels=c(format(min(ep$fa65)*2.6, digits=3) ), cex=0.5 ) 
text( y=max(p_ep_65[2,1:L]) - 0.00, x=6.4 + 0.25, labels=c(format(max(ep$fa65)*2.6, digits=3) ), cex=0.5 ) 

boxplot( p_mg_65[1,1:L], range=0, horizontal=T, at=4.7, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C65, frame=F)
text( x=min(p_mg_65[1,1:L]) - 0.00, y=4.7 - 0.1, labels=c(format(min(mg$ns65), digits=3) ), cex=0.5 ) 
text( x=max(p_mg_65[1,1:L]) + 0.00, y=4.7 - 0.1, labels=c(format(max(mg$ns65), digits=3) ), cex=0.5 ) 

boxplot( p_mg_65[2,1:L], range=0, horizontal=F, at=2.2, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C65, frame=F)
text( y=min(p_mg_65[2,1:L]) - 0.00, x=2.2 - 0.15, labels=c(format(min(mg$fa65)*2.6, digits=3) ), cex=0.5 ) 
text( y=max(p_mg_65[2,1:L]) - 0.00, x=2.2 - 0.15, labels=c(format(max(mg$fa65)*2.6, digits=3) ), cex=0.5 ) 

text( x=mean(p_mg_65[1,1:L]), y=mean(p_mg_65[2,1:L])+0.0, label="mg", cex=0.7 )
text( x=mean(p_ep_65[1,1:L]), y=mean(p_ep_65[2,1:L])+0.0, label="ep", cex=0.7 )

### 80W

boxplot( p_ep_80[1,1:L], range=0, horizontal=T, at=5.4, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c80, frame=F)
text( x=min(p_ep_80[1,1:L]) - 0.00, y=5.4 + 0.1, labels=c(format(min(ep$ns80), digits=3) ), cex=0.5 ) 
text( x=max(p_ep_80[1,1:L]) + 0.10, y=5.4 + 0.1, labels=c(format(max(ep$ns80), digits=3) ), cex=0.5 ) 

boxplot( p_ep_80[2,1:L], range=0, horizontal=F, at=-0.1, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c80, frame=F)
text( y=min(p_ep_80[2,1:L]) - 0.00, x=0.1 - 0.05, labels=c(format(min(ep$fa80)*2.6, digits=3) ), cex=0.5 ) 
text( y=max(p_ep_80[2,1:L]) - 0.00, x=0.1 - 0.05, labels=c(format(max(ep$fa80)*2.6, digits=3) ), cex=0.5 ) 

boxplot( p_mg_80[1,1:L], range=0, horizontal=T, at=7.6, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C80, frame=F)
text( x=min(p_mg_80[1,1:L]) - 0.00, y=7.6 + 0.1, labels=c(format(min(mg$ns80), digits=3) ), cex=0.5 ) 
text( x=max(p_mg_80[1,1:L]) + 0.00, y=7.6 + 0.1, labels=c(format(max(mg$ns80), digits=3) ), cex=0.5 ) 

boxplot( p_mg_80[2,1:L], range=0, horizontal=F, at=1.9, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C80, frame=F)
text( y=min(p_mg_80[2,1:L]) - 0.10, x=1.9 - 0.00, labels=c(format(min(mg$fa80)*2.6, digits=3) ), cex=0.5 ) 
text( y=max(p_mg_80[2,1:L]) - 0.00, x=1.9 + 0.20, labels=c(format(max(mg$fa80)*2.6, digits=3) ), cex=0.5 ) 

text( x=mean(p_mg_80[1,1:L]), y=mean(p_mg_80[2,1:L])+0.0, label="mg", cex=0.7 )
text( x=mean(p_ep_80[1,1:L]), y=mean(p_ep_80[2,1:L])+0.0, label="ep", cex=0.7 )

pBW <- blr_scale2( c(ns_BW, ns_B), c(fa_BW, fa_B), pBx, pBy, pBs )
points( pBW[1,1:(Wcount*2)], pBW[2,1:(Wcount*2)], pch=1, col="black", cex=0.8 )

pCW <- blr_scale2( c(ns_CW, ns_B), c(fa_CW, fa_B), pBx, pBy, pBs )
points( pCW[1,1:(Wcount*2)], pCW[2,1:(Wcount*2)], pch=1, col="black", cex=0.8 )

pDx <- 0
pDy <- 0.0
pDs <- 80
pD <- blr_scale2( ns_D, fa_D, pDx, pDy, pDs )
p_ep_95 <- blr_scale2( c( ep$ns95, ns_D ), c( ep$fa95, fa_D ), pDx, pDy, pDs )
p_mg_95 <- blr_scale2( c( mg$ns95, ns_D ), c( mg$fa95, fa_D ), pDx, pDy, pDs )
points( pD[1,], pD[2,], pch=".", col=cl_D )
axis(side=2, cex.axis=0.6, 
	at=c( min(pD[2,]), max(pD[2,]) ), 
	label=c( format(min(fa_D)*2.6, digits=4), format(max(fa_D)*2.6, digits=4)))
axis(side=1, cex.axis=0.6, line=0.0, padj=-2, 
	at=c( min(pD[1,]), max(pD[1,]) ), 
	label=c( format(min(ns_D), digits=4), format(max(ns_D), digits=4)) )
boxplot( p_ep_95[1,1:L], range=0, horizontal=T, at=0.2, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c95, frame=F)
text( x=min(p_ep_95[1,1:L]) - 0.00, y=0.2 - 0.15, labels=c(format(min(ep$ns95), digits=3) ), cex=0.5 ) 
text( x=max(p_ep_95[1,1:L]) - 0.00, y=0.2 - 0.15, labels=c(format(max(ep$ns95), digits=3) ), cex=0.5 ) 

boxplot( p_ep_95[2,1:L], range=0, horizontal=F, at=-0.1, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=c95, frame=F)
text( y=min(p_ep_95[2,1:L]) - 0.10, x=-0.1 - 0.05, labels=c(format(min(ep$fa95)*2.6, digits=4) ), cex=0.5 ) 
text( y=max(p_ep_95[2,1:L]) + 0.10, x=-0.1 - 0.05, labels=c(format(max(ep$fa95)*2.6, digits=4) ), cex=0.5 ) 

boxplot( p_mg_95[1,1:L], range=0, horizontal=T, at=-0.20, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C95, frame=F)
text( x=min(p_mg_95[1,1:L]) - 0.00, y=0.0 - 0.1, labels=c(format(min(mg$ns95), digits=4) ), cex=0.5 ) 
text( x=max(p_mg_95[1,1:L]) + 0.00, y=0.0 - 0.1, labels=c(format(max(mg$ns95), digits=4) ), cex=0.5 ) 

boxplot( p_mg_95[2,1:L], range=0, horizontal=F, at=2.9, boxwex=vx, add=T, xaxt="n", yaxt="n", bty="n", col=C95, frame=F)
text( y=min(p_mg_95[2,1:L]) - 0.00, x=2.9 + 0.20, labels=c(format(min(mg$fa95)*2.6, digits=3) ), cex=0.5 ) 
text( y=max(p_mg_95[2,1:L]) - 0.00, x=2.9 + 0.20, labels=c(format(max(mg$fa95)*2.6, digits=3) ), cex=0.5 ) 

text( x=mean(p_mg_95[1,1:L]), y=mean(p_mg_95[2,1:L])+0.8, label="mg", cex=0.7 )
text( x=mean(p_ep_95[1,1:L])+0.2, y=mean(p_ep_95[2,1:L])+0.0, label="ep", cex=0.7 )

pDW <- blr_scale2( c(ns_DW, ns_D), c(fa_DW, fa_D), pDx, pDy, pDs )
points( pDW[1,1:(Wcount*2)], pDW[2,1:(Wcount*2)], pch=1, col="black", cex=0.8 )


text( x=6.4, y=9.55, label="30W", cex=0.9 )
text( x=6.0, y=6.10, label="65W", cex=0.9 )
text( x=1.6, y=7.35, label="80W", cex=0.9 )
text( x=2.4, y=3.20, label="95W", cex=0.9 )


# Dashed lines
#x=c( blr_scale( c( max(ns_B), min(ns_A), max(ns_A) ), xscale   )[1], 7.00)
#y=c( blr_scale( c( max(fa_B), min(fa_A), max(fa_A) ), yscale_A )[1], 7.50)
#lines( x=x, y=y, lty=2, col="gray" )
#x=c( blr_scale( c( max(ns_C), min(ns_B), max(ns_B) ), xscale   )[1], 7.00)
#y=c( blr_scale( c( max(fa_C), min(fa_B), max(fa_B) ), yscale_B )[1], 5.00)
#lines( x=x, y=y, lty=2, col="gray" )
#x=c( blr_scale( c( max(ns_D), min(ns_C), max(ns_C) ), xscale   )[1], 7.00)
#y=c( blr_scale( c( max(fa_D), min(fa_C), max(fa_C) ), yscale_C )[1], 2.50)
#lines( x=x, y=y, lty=2, col="gray" )

#text( x=6.90, y=0.20, format(    min( mg$fa95 ), digits=4 ), cex=0.4 ) 

dev.off()

