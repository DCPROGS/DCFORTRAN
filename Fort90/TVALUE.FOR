	subroutine TVALUE(ndf,tval)
c Returns P=0.95 value of Student's t, with ndf degrees of freedom
	real ttable(30)
c
	ttable(1)=12.706
	ttable(2)=4.303
	ttable(3)=3.1824
	ttable(4)=2.776
	ttable(5)=2.571
	ttable(6)=2.447
	ttable(7)=2.365
	ttable(8)=2.306
	ttable(9)=2.262
	ttable(10)=2.228
	ttable(11)=2.201
	ttable(12)=2.179
	ttable(13)=2.160
	ttable(14)=2.145
	ttable(15)=2.131
	ttable(16)=2.120
	ttable(17)=2.210
	ttable(18)=2.101
	ttable(19)=2.093
	ttable(20)=2.086
	ttable(21)=2.080
	ttable(22)=2.074
	ttable(23)=2.069
	ttable(24)=2.064
	ttable(25)=2.060
	ttable(26)=2.056
	ttable(27)=2.052
	ttable(28)=2.048
	ttable(29)=2.045
	ttable(30)=2.042
c
	if(ndf.ge.1.and.ndf.le.30) then
	   tval=ttable(ndf)
	else if(ndf.ge.31.and.ndf.le.40) then
	   frac=float(ndf-30)/10.	!=0.1 to 1
	   tval=2.042 - frac*(2.042-2.021)
	else if(ndf.ge.41.and.ndf.le.60) then
	   frac=float(ndf-40)/20.	!=0.05 to 1
	   tval=2.021 - frac*(2.021-2.000)
	else if(ndf.ge.61.and.ndf.le.120) then
	   frac=float(ndf-60)/60.	!=1/60 to 1
	   tval=2.000 - frac*(2.000-1.980)
	else if(ndf.gt.120) then
	   tval=1.96
	else if(ndf.le.0) then
	   call BELL(2)
	   print *,' ERROR IN TVALUE.FOR'
	endif
	RETURN
	end

