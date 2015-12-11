	subroutine PDrOUTs(ptitle,LI,LJ,area,amean,ncomp,am,sd,
     & kdim,pon,fprint,discprt)
c To print distribution of geometric distributions (number of ops/bst etc)
c with single prec inputs area,amean as provided by
c subroutines PDOPbst,PDOPclst etc
c Analogue of PDFOUTs() for printing exponential dists
c Prints f(2,3), f(2) or just f(t) for condional dists. Also has overall title
c (up to 70 char) in ptitle.
c To print pdfs, mean and SD and return mean and SD (real*8)
C	L1=i LJ=j prints P(r;i,j)
C	L1=i LJ=-1 prints P(r;i)
C	L1=-1 LJ=-1 prints P(r)
C	L1=-2 LJ=-1 prints P(inf)
C
c FPRINT=true  -types/prints everything
c FPRINT=false  -types everything, but prints only mean and sd (if pon())
	real amean(kdim),area(kdim)
	logical pon,fprint,discprt
c	character*70 ptitle
	character*(*) ptitle
c
C Type LHS
c need to rmove leading blanks form ptitle?
c	do 51 i=1,70
c	i1=i
c	ip=ichar(ptitle(i:i))
c51	if(ip.ne.32.and.ip.ne.0) goto 52
c52	i1=i1-1
c	ns=nblank(ptitle)
c	print 50,ptitle(i1:ns)
c	print 103
c103	format(/)
	nl=NBLANK(ptitle)
	if(nl.gt.0) print *,CHARNB(ptitle)
	IF(LI.EQ.-1.AND.LJ.EQ.-1) print 1
	IF(LI.GE.0.AND.LJ.LT.0) print 2 ,LI
	IF(LI.GE.0.AND.LJ.GE.0) print 3 ,LI,LJ
	IF(LI.EQ.-2.AND.LJ.EQ.-1) print 4
	if(pon) then
	   if(nl.gt.0) write(7,*) ptitle
	   IF(LI.EQ.-1.AND.LJ.EQ.-1) write(7,1)
	   IF(LI.GE.0.AND.LJ.LT.0) write(7,2)LI
	   IF(LI.GE.0.AND.LJ.GE.0) write(7,3)LI,LJ
	   IF(LI.EQ.-2.AND.LJ.EQ.-1) write(7,4)
	endif
	if(discprt) then
	   if(nl.gt.0) write(8,*) ptitle
         if(LI.EQ.-2.AND.LJ.EQ.-1) write(8,4)
         if(LI.EQ.-1.AND.LJ.EQ.-1) write(8,1)
         if(LI.GE.0.AND.LJ.LT.0) write(8,2)LI
         if(LI.GE.0.AND.LJ.GE.0) write(8,3)LI,LJ
	endif
1	FORMAT( 1X,'P(r) = ')
2	FORMAT( 1X,'P(r;',I2,') =    ')
3	FORMAT( 1X,'P(r;',I2,',',I2,') = ')
4	FORMAT( 1X,'P(inf) = ')
50	format(//,1x,a70)
c
C TEST INDETERMINACY- in old version w(m) were set to zero in this case;
c here test for area=zero (but setting a=0.0 in indeterminate cases not
c yet done in any calling progs)
	DO 10 M=1,ncomp
10	if(area(m).gt.1.e-20) goto 11	!out of loop
	print 12
	if(pon) write(7,12)
	if(discprt) write(8,12)
12	FORMAT(' INDETERMINATE: all areas are zero in PDrOUTs')
	RETURN
11	continue
c
c
	atot=0.0
	am=0.0
	sum2=0.0
	print 13
	if(pon) write(7,13)
	if(discprt) write(8,13)
13	format(
     &' term      w         lambda           area (%)       Norm mean')
cc     & '  m      W(m)         A(m)       Lambda(m)     Norm mean')
c
	am=0.0			!for mean
	sum2=0.0
	DO 41 m=1,ncomp
	  x=1.0/amean(m)			!=1-eig
	  rho=1.0 - x			!eigenvalue
	  atot=atot+area(m)
	  w=area(m)/amean(m)		!amplitude
	  am=am + w/(x*x)			!accumulate mean
	  sum2=sum2 + w*(1.0+rho)/(x**3)    !accum E(r*r)
	  a=100.*area(m)		!percent
	  print 14,m,w,rho,a,amean(m)
	  if(pon.and.fprint) write(7,14) m,w,rho,a,amean(m)
        if(discprt) write(8,14) m,w,rho,a,amean(m)
14	  format(i3,3x,g13.6,1x,g13.6,3x,f11.6,2x,g13.6)
41	continue
c
	sd=sqrt(sum2 - am*am)
	print 15,atot
	if(pon.and.fprint) write(7,15) atot
      if(discprt) write(8,15) atot
15	format(
     & '       Total area= ',g13.6)
c	print 108
c	if(pon) write(7,108)
c	if(discprt) write(8,108)
c108	format(/)
	print 16,am,sd,sd/am
	if(pon) write(7,16) am,sd,sd/am
      if(discprt) write(8,16) am,sd,sd/am
16	format(
     & ' Mean = ',g13.6,'   SD= ',g13.6,'   SD/mean = ',g13.6)
c
	RETURN
	END


