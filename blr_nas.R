d<-read.table("blr_nas.dat", header=T)

# ts values: 0   60  120  180  240   30   90  150  210  270  300  330  360  390  420  450  480  510  540  570  600  630  660  690  720  750  780  810  840  870  900  930  960  990 1020
# pkg power limits:  115  95  80  65  50  30  40, lots of experiments missing at 40W limit.

d$ts<-floor(d$TimeStamp) # round to the nearest second.
ts = 30 # both ep and mg get to at least this point. 210 works
ts_max = 210
ts_list = seq(from=30, to=ts_max, by=30)

ep<-NA
mg<-NA
colnames<-"dummy"
pkg_limits <- unique(d$pkg0_lim)
whitelist <- d$hostname!="cab500" & d$hostname!="cab37"

print("QQA\n")
for( pkglim in pkg_limits[pkg_limits != 40] ){

	#set the mask for this package limit
	print("QQA1\n")
	ep_m <- d$bmark=="EP" & d$ts==ts & ( (d$config=="s1" & d$pkg0_lim==pkglim) | (d$config=="s2" & d$pkg1_lim==pkglim) ) & whitelist
	mg_m <- d$bmark=="MG" & d$ts==ts & ( (d$config=="s1" & d$pkg0_lim==pkglim) | (d$config=="s2" & d$pkg1_lim==pkglim) ) & whitelist

	#hostname
	print("QQA2\n")
	ep_h <- c(as.vector(d$hostname[ep_m & d$config=="s1"]), as.vector(d$hostname[ep_m & d$config=="s2"]) )
	mg_h <- c(as.vector(d$hostname[mg_m & d$config=="s1"]), as.vector(d$hostname[mg_m & d$config=="s2"]) )

	#zeroth core temperature
	print("QQA3\n")
	ep_t <- c( d$coreT0[ ep_m & d$config=="s1"] , d$coreT8[ ep_m & d$config=="s2" ] ) 
	mg_t <- c( d$coreT0[ mg_m & d$config=="s1"] , d$coreT8[ mg_m & d$config=="s2" ] ) 

	#execution time in seconds	
	print("QQA4\n")
	ep_s <- c(d$s[ep_m & d$config=="s1"], d$s[ep_m & d$config=="s2"] )
	mg_s <- c(d$s[mg_m & d$config=="s1"], d$s[mg_m & d$config=="s2"] )

	# Sanity check
	print("QQA5\n")
	cat( "bmark pkglim ts m h t s \n" )
	cat( paste( "ep", pkglim, ts, sum(ep_m), length(ep_h), length(ep_t), length(ep_s), "\n" ) )
	cat( paste( "mg", pkglim, ts, sum(mg_m), length(mg_h), length(mg_t), length(mg_s), "\n" ) )

	#fold into data tables
	print("QQA6\n")
	colnames <- c( colnames, paste("h",pkglim,sep=""), paste("t",pkglim,sep=""), paste("s",pkglim,sep="") )
	print("QQA7\n")
	print(length(ep_h))
	ep<-cbind(as.data.frame(ep),ep_h,ep_t,ep_s)
	print("QQA8\n")
	mg<-cbind(as.data.frame(mg),mg_h,mg_t,mg_s)
	print("QQA9\n")
	names(ep)<-colnames
	names(mg)<-colnames
}

print("QQB\n")
for( pkglim in pkg_limits[pkg_limits != 40 ] ){
	for( ts in ts_list ){
		# set the mask for this package limit
		print("QQB1\n")
		ep_m <- d$bmark=="EP" & d$ts==ts & ( (d$config=="s1" & d$pkg0_lim==pkglim) | (d$config=="s2" & d$pkg1_lim==pkglim) ) & whitelist
		mg_m <- d$bmark=="MG" & d$ts==ts & ( (d$config=="s1" & d$pkg0_lim==pkglim) | (d$config=="s2" & d$pkg1_lim==pkglim) ) & whitelist

		# grab mperf and aperf
		print("QQB2\n")
		ep_aperf <- c( d$APERF0[ep_m & d$config=="s1"], d$APERF8[ep_m & d$config=="s2"])
		mg_aperf <- c( d$APERF0[mg_m & d$config=="s1"], d$APERF8[mg_m & d$config=="s2"])

		print("QQB3\n")
		ep_mperf <- c( d$MPERF0[ep_m & d$config=="s1"], d$MPERF8[ep_m & d$config=="s2"])
		mg_mperf <- c( d$MPERF0[mg_m & d$config=="s1"], d$MPERF8[mg_m & d$config=="s2"])

		# effective frequency for a single 30-second period
		print("QQB4\n")
		ep_f <- ep_aperf/ep_mperf
		mg_f <- mg_aperf/mg_mperf

		# colnames
		print("QQB5\n")
		ep<-cbind(as.data.frame(ep), ep_aperf, ep_mperf, ep_f)
		mg<-cbind(as.data.frame(mg), mg_aperf, mg_mperf, mg_f)
		print("QQB6\n")
		colnames <- c( colnames, paste("aperf",pkglim,"_",ts,sep=""), paste("mperf",pkglim,"_",ts,sep=""), paste("f",pkglim,"_",ts,sep="") )
		names(ep)<-colnames
		names(mg)<-colnames
	}
}	
print("QQC\n")
ep$fa30 <- (ep$f30_30 + ep$f30_60 + ep$f30_90 + ep$f30_120 + ep$f30_150 + ep$f30_180 + ep$f30_210) / 7
ep$fa50 <- (ep$f50_30 + ep$f50_60 + ep$f50_90 + ep$f50_120 + ep$f50_150 + ep$f50_180 + ep$f50_210) / 7
ep$fa65 <- (ep$f65_30 + ep$f65_60 + ep$f65_90 + ep$f65_120 + ep$f65_150 + ep$f65_180 + ep$f65_210) / 7
ep$fa80 <- (ep$f80_30 + ep$f80_60 + ep$f80_90 + ep$f80_120 + ep$f80_150 + ep$f80_180 + ep$f80_210) / 7
ep$fa95 <- (ep$f95_30 + ep$f95_60 + ep$f95_90 + ep$f95_120 + ep$f95_150 + ep$f95_180 + ep$f95_210) / 7
ep$fa115<- (ep$f115_30 + ep$f115_60 + ep$f115_90 + ep$f115_120 + ep$f115_150 + ep$f115_180 + ep$f115_210) / 7

print("QQD\n")
mg$fa30 <- (mg$f30_30 + mg$f30_60 + mg$f30_90 + mg$f30_120 + mg$f30_150 + mg$f30_180 + mg$f30_210) / 7
mg$fa50 <- (mg$f50_30 + mg$f50_60 + mg$f50_90 + mg$f50_120 + mg$f50_150 + mg$f50_180 + mg$f50_210) / 7
mg$fa65 <- (mg$f65_30 + mg$f65_60 + mg$f65_90 + mg$f65_120 + mg$f65_150 + mg$f65_180 + mg$f65_210) / 7
mg$fa80 <- (mg$f80_30 + mg$f80_60 + mg$f80_90 + mg$f80_120 + mg$f80_150 + mg$f80_180 + mg$f80_210) / 7
mg$fa95 <- (mg$f95_30 + mg$f95_60 + mg$f95_90 + mg$f95_120 + mg$f95_150 + mg$f95_180 + mg$f95_210) / 7
mg$fa115<- (mg$f115_30 + mg$f115_60 + mg$f115_90 + mg$f115_120 + mg$f115_150 + mg$f115_180 + mg$f115_210) / 7

print("QQE\n")
min_ep_s<- min(ep$s115)
min_mg_s<- min(mg$s115)

print("QQF\n")
ep$ns30 <- ep$s30 / min_ep_s
ep$ns50 <- ep$s50 / min_ep_s
ep$ns65 <- ep$s65 / min_ep_s
ep$ns80 <- ep$s80 / min_ep_s
ep$ns95 <- ep$s95 / min_ep_s
ep$ns115<- ep$s115/ min_ep_s

print("QQG\n")
mg$ns30 <- mg$s30 / min_mg_s
mg$ns50 <- mg$s50 / min_mg_s
mg$ns65 <- mg$s65 / min_mg_s
mg$ns80 <- mg$s80 / min_mg_s
mg$ns95 <- mg$s95 / min_mg_s
mg$ns115<- mg$s115/ min_mg_s

