	program RANTEST
c To do randomisation tests. Data stored exactly as in CVFIT but file
c is called RANDAT.DAT
c F90/Gino version
	real YOBS(500,10),XOBS(500,10),setx(10)
	real Y1(500),X1(500),D1(500),ALLOBS(1000)		! 1 D arrays for test
	real xsamp(500),ysamp(500)
	real data(500,10)
	real randiff(500000)
	integer njx(10),njy(10)
	integer iran(2000)
	character*79 heading,title
	character*60 TITLED(10)			!data set names
	character*14 titlex(10),titley(10),spaces	!col headings
	character*11 cdate,ctime
	character*1 ans,UC
c For ascii read
	character*20 colhead(2)		!for ASCREAD1
	integer nrow(10)
	allocatable::Xnum			!for ASCREAD2
	real*4 Xnum(:,:)
	character*33 ascfil
	character*70 text       !for ascread1
	logical dcfile
c
	logical discprt,slock,pon,debug,caplock,setvar,repeat
	logical addset,fopen,show,paired,binomial,deb
	logical gr1,gr2
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/rand/ix,iy,iz
c
c define function
	pon()=slock()
	debug()=caplock()
c
      filnam='RANTEST.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1     format(
     & ' RANTEST: RANDOMISATION TEST FOR TWO SAMPLES',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2       format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3       format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
	call GINO
c
	iread=1
	nrdim=500	!declared dimensions of data()
	ncdim=10
	nran=50000		!default
12	continue	!return for new data
	addset=.false.
	nr1=-1		!so window initially in default position
	fopen=.false.	!file not yet open
	paired=.false.
c
	print 90
	if(discprt) write(8,90)
90	format(
     & '   This program performs a randomisation test to compare two',/,
     & ' independent samples.  According to the null hypothesis of',/,
     & ' no-difference, each outcome would have been the same,',/,
     & ' regardless of which group the individual happened to',/,
     & ' be allocated. Therefore all N=n1+n2 observations are',/,
     & ' pooled and, as in the actual experiment, divided at',/,
     & ' random into groups of size n1 and n2.  The fraction',/,
     & ' of randomisations that gives rise to a difference',/,
     & ' between the groups at least as large as that observed',/,
     & ' gives the P value.',/,
     & '   In the binomial case, in which the measurement is the ',/,
     & ' fraction of ''successes'' in each sample (say r1 out ',/,
     & ' of n1, and r2 out of n2) a ''success'' is given a ',/,
     & ' score of 1, ''failure'' = 0',/)
c     & '',/,
c
	print 91
91	format(
     & ' (1) Continuously variable data (as for t test)',/,
     & ' (2) Binomial data (each result= yes or no)',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	binomial=iopt.eq.2
	if(binomial) then
	   paired=.false.
	   jset=1
	   titlex(jset)='  Set 1       '
	   titley(jset)='  Set 2       '
	   print 92
92	   format(' Sample 1: r1 successes out of n1. Enter r1, n1 = ')
	   call INPUT2i(ir1,n1)
	   print 93
93	   format(' Sample 2: r2 successes out of n2. Enter r2, n2 = ')
	   call INPUT2i(ir2,n2)
	   ntot=n1+n2
	   irtot=ir1+ir2		!total # of 'successes'
	   iftot=ntot-irtot	!total # of 'failures'
	   is2=ir2
	   is1=irtot-is2
	   if1=n1-is1		!number of 'failures' in group 1
	   if2=n2-is2		!number of 'failures' in group 1
c Test both criteria -should not differ!
9	   print 931
931	   format(' Do the test using',/,
     &    ' (1) number of successes in set 1 (r1) as criterion',/,
     &    ' (2) difference between the p=r/n values as criterion',/,
     &    ' (3) Help',/,
     &    ' Option number [1] = ')
	   icrit=1
	   call INPUTi(icrit)
	   if(icrit.eq.3) then
		print 7
		if(discprt) write(8,7)
7	 	format(
     & ' Both criteria give the same one-tail P value.  ',/,
     & ' Use of r1 as criterion is the direct Monte Carlo equivalent',/,
     & ' of summing the the exact Fisher test probabilities for the ',/,
     & ' observed 2x2 table with those for all tables that depart ',/,
     & ' further from the null hypothesis in the observed direction.',/,
     & ' A 2-tail probablilty can be found by doubling the one-tail',/,
     &' value, at least if the displayed distribution is symmetrical',/,
     & '   Use of (p1-p2) as criterion gives both one and two-tail ',/,
     &' probabilities directly by seeing how many random allocations',/,
     & ' of the observations to groups of size n1 and n2 produce',/,
     & ' and absolute value of (p1-p2) at least as big as that ',/,
     & ' observed.',/)
		goto 9
	   endif
	   ir1obs=ir1
	   nx=n1
	   ny=n2
	   ntot=n1+n2
	   irtot=ir1+ir2		!total # of 'successes'
	   iftot=ntot-irtot	!total # of 'failures'
	   do i=1,n1
		if(i.le.ir1) then
		   x1(i)=1.0
		else
		   x1(i)=0.0
		endif
	   enddo
	   do i=1,n2
		if(i.le.ir2) then
		   y1(i)=1.0
		else
		   y1(i)=0.0
		endif
	   enddo
c Put all obs into one array for unpaired test
	   k=0
	   stot=0.0
	   do i=1,nx
		k=k+1
		stot=stot+X1(i)
		ALLOBS(k)=X1(i)
         enddo
	   do i=1,ny
		k=k+1
		stot=stot+Y1(i)
		ALLOBS(k)=Y1(i)
	   enddo
	   ntot=nx+ny
	   if(k.ne.ntot) call BELL(10)
c  Use Gaussian approx to do 2 sample t test
	   p1=float(ir1)/float(n1)
	   p2=float(ir2)/float(n2)
	   xbar=p1
	   ybar=p2
	   ppool=float(ir1+ir2)/float(n1+n2)
	   sd1=sqrt(p1*(1.0-p1)/float(n1))
	   sd2=sqrt(p2*(1.0-p2)/float(n2))
	   sd1p=sqrt(ppool*(1.0-ppool)/float(n1))
	   sd2p=sqrt(ppool*(1.0-ppool)/float(n2))
	   sdiff=sqrt(sd1p*sd1p + sd2p*sd2p)
	   dobs=p1-p2	!as defined for dobs and dran, below
	   adobs=abs(dobs)
	   print 161,ir1,n1,p1,sd1,ir2,n2,p2,sd2,dobs
	   if(discprt) write(8,161) ir1,n1,p1,sd1,ir2,n2,p2,sd2,dobs
161	   format(
     & ' Set 1: ',i3,' successes out of ',i3,
     & ' p1 = ',g13.6,' SD(p1) = ',g13.6,/,
     & ' Set 2: ',i3,' successes out of ',i3,
     & ' p2 = ',g13.6,' SD(p2) = ',g13.6,/,
     & ' Observed difference between sets, p1-p2 = ',g13.6)
	   print 31
	   if(discprt) write(8,31)
31	   format(/,' Observed 2x2 table:')
	   print 4, is1,if1,is1+if1
	   if(discprt) write(8,4) is1,if1,is1+if1
4	   format(' Set 1:',i4,2x,i4,4x,i4)
	   print 5, is2,if2,is2+if2
	   if(discprt) write(8,5) is2,if2,is2+if2
5	   format(' Set 2:',i4,2x,i4,4x,i4)
	   print 6, is1+is2,if1+if2,is1+is2+if1+if2
	   if(discprt) write(8,6) is1+is2,if1+if2,is1+is2+if1+if2
6	   format(' Total:',i4,2x,i4,4x,i4,/)
c
	   adiff=adobs
	   tval=adiff/sdiff
	   ndf=100000	!to get Gaussian
	   vr=tval*tval
	   ndf1=1
	   call FDIST(VR,ndf1,ndf,P)
         print 261,adiff,sdiff,tval,P
         if(discprt) write(8,261) adiff,sdiff,tval,P
261	   format(/,
     &' Two-sample unpaired test',
     &' using Gaussian approximation to binomial:',/,
     & ' standard normal deviate = ',
     & g13.4,'/',g13.4,' = ',f10.4,/,' two tail P = ',g13.6,/)
	   goto 94
	endif
c
c	print 100
c100	FORMAT(' Read data from disk [Y] ? ')
c	read 101,ans
c	if(UC(ans).EQ.'N') goto 102
101	format(a1)
	print 100,iread
100	format(
     & ' (1) Enter data on screen',/,
     & ' (2) Read data from ASCII file (as two columns)',/,
     & ' (3) Read data from RANTEST data file',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iread)
c
c type in data:
	if(iread.eq.1) goto 102		!type data on screen
	if(iread.eq.3) goto 109		!read from randat.dat
	if(iread.eq.2) then		!read from ascii
	   print 151
151	   format(/,
     &' ASCII file must have two columns. If the number of values',/,
     &' is not the same in each column (sample), the blank fields',/,
     &' must be at the end of the column, and number in each sample',/,
     &' must be specified next (the default values will usually',/,
     &' be n for the larger sample)')
	   ioff=-1
	   ilinhead=-1
c       Get filename, number of rows and cols
c=	   ALLOCATE(colhead(ncols))
	   ncols=2		!=declared size of colhead and nrow
	   call ASCREAD3(ioff,ilinhead,ncols,nrow,nrowmax,len,
     &    ascfil,colhead,text,lentext,dcfile)	!call with nrow=array
	   nrows=nrowmax	!for below
	   nd1=nrowmax
	   nd2=ncols
	   ALLOCATE(xnum(nd1,nd2))
	   call ASCREAD4(ioff,ncols,nrow,len,
     & 	ascfil,xnum,nd1,nd2)		!call with nrow=array
	   nx=nrow(1)
	   ny=nrow(2)
	   if(ncols.ne.2) then
		print 110
110		format(' First 2 columns only will be used')
		ncols=2
	   endif
	   nc1=ncols
	   if(dcfile) then
		if(lentext.gt.0) then
		   print 213,text
213		   format(' Title: ',a70)
		endif
	   endif
c Print file name
         print 27,ascfil
         if(pon()) write(7,27) ascfil
         if(discprt) write(8,27) ascfil
27	   format(' Data from ASCII file: ',a33)
c Copy data to xobs,yobs (set 1)
	   do j=1,ncols
		do i=1,nrow(j)
		   xobs(i,1)=xnum(i,1)
		   yobs(i,1)=xnum(i,2)
		enddo
	   enddo
	   DEALLOCATE(xnum)
	   nset=1		!for use after 330
	   njx(1)=nx
	   njy(1)=ny
	   goto 71		!show in inwind1
	endif			!end of ASCII read
c
c for disc read:
109	continue
	ifile=1		!for read
	call RANDISK(Xobs,Yobs,nset,njx,njy,setvar,setx,
     & titled,titlex,titley,IFILE,FOPEN)
	if(nset.eq.-1) goto 999
c
330	print 112
112	FORMAT(' View/alter the data [Y]? ')
	read 101,ans
	if(UC(ans).eq.'N') GOTO 114
c loop to view each set
71	continue
c
	do 113 j=1,nset
	nx=njx(j)
	ny=njy(j)
	nrow(1)=nx		!for inwind2
	nrow(2)=ny		!for inwind2
	nlx=nblank(titlex(j))
	nly=nblank(titley(j))
	nl=nlx+nly
	spaces=' '
	ns=1
	if(nl.le.17) then
	   do 201 i=17,nl,-1
	   ns=ns+1
201	   spaces=spaces//' '
	endif
	if(ns.gt.14) ns=14
	heading='   '//titlex(j)(1:nlx)//spaces(1:ns)//titley(j)(1:nly)
c     & '   X value       Y value'
	ncol=2
c#######show also (X-Y),X/Y,....??
	iflag=ncol	!so input data displayed in all cols of window
	title=' SET '//char(j+48)//': '//charnb(titled(j))
c
	do i=1,nx
	   data(i,1)=xobs(i,j)
	enddo
	do i=1,ny
	   data(i,2)=yobs(i,j)
	enddo
	nmax=nx
	if(ny.gt.nx) nmax=ny
	nidisp=nmax
	if(nidisp.gt.20) nidisp=20	!best value- up to 20?
	iconst=0	!no constraint of columns
c
	call INWIND2(data,nmax,ncol,nidisp,title,heading,nr1,nc1,
     & iflag,iconst,nrow,nrdim,ncdim)
c
c  Reassign data (if it has been altered)
	if(iflag.eq.1) then
	   do 35 i=1,nx
35	   xobs(i,j)=data(i,1)
	   do 351 i=1,ny
	   yobs(i,j)=data(i,2)
351	   continue
	endif
	call PRINTSET(j,Xobs,Yobs,nx,ny,nmax,titled,titlex,titley,
     & setx,setvar,X1,Y1,D1,xbar,sdx,ybar,sdy,dbar,sdd)
113	continue		!end of jth set loop
c
c May need to insert options to change set titles here
c
73	print 72
72	format(' Options to alter data:',/,
     & ' [0] No changes ',/,
     & ' (1) Add another data set',/,
     & ' (2) Alter a set title',/,
     & ' Option number [0] = ')
	ialt=0
	call INPUTi(ialt)
	if(ialt.eq.0) goto 59
	if(ialt.lt.0.or.ialt.gt.2) goto 73
	goto (74,75) ialt
c
74	continue
	print 721
721	format(' (1) Type in data',/,
     & ' (2) Concatenate two existing data sets',/,
     & ' Option number = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   nset=nset+1
	   j1=nset		!so only new set read in below
	   addset=.true.
	   goto 203
	else if(iopt.eq.2) then
	   nset=nset+1
	   print 722,nset
722	   format(' Make set ',i3,' by concatenating sets n1,n2= ')
	   call INPUT2i(js1,js2)
	   j=nset
	   njx(j)=njx(js1)+njx(js2)
	   njy(j)=njy(js1)+njy(js2)
	   do 724 i=1,njx(js1)
		Xobs(i,j)=Xobs(i,js1)
724		data(i,1)=Xobs(i,j)
	   do 725 i=1,njy(js1)
		yobs(i,j)=yobs(i,js1)
725		data(i,2)=yobs(i,j)
	   do 726 i=njx(js1)+1,njx(j)
		Xobs(i,j)=Xobs(i-njx(js1),js2)
c		i1=i-njx(js1)
c		if(debug()) print 729,i,j,xobs(i,j),i1,js2,xobs(i1,js2)
c729		format(' i,j,xobs(i,j),i1,js2,xobs(i1,js2)=',/,2(2i5,g13.6))
726		data(i,1)=Xobs(i,j)
	   do 727 i=njy(js1)+1,njy(j)
		Yobs(i,j)=Yobs(i-njy(js1),js2)
727		data(i,2)=Yobs(i,j)
	   call TITENT(' Enter title for set:',titled(j),60)
	   title=' SET '//char(j+48)//': '//charnb(titled(j))
	   titlex(j)=titlex(js2)	!presumably same col heads as sets concat
	   titley(j)=titley(js2)
	   nx=njx(j)
	   ny=njy(j)
	   nmax=nx
	   if(ny.gt.nx) nmax=ny
	   nidisp=nmax
	   if(nidisp.gt.20) nidisp=20	!best value- up to 20?
	   ncol=2
	   iflag=ncol	!so input data displayed in all cols of window
c	   if(debug()) then
c		print 728,j,js1,js2,njx(j),njy(j),njx(js1),njy(js1),
c     &	njx(js2),njy(js2),nmax
c728		format(' j,js1,js2,njx(j),njy(j),njx(js1),njy(js1),'
c     & 	' njx(js2),njy(js2),nmax= ',/,10i5)
c		pause
c	   endif
c
         call INWIND2(data,nmax,ncol,nidisp,title,heading,nr1,nc1,
     &    iflag,iconst,nrow,nrdim,ncdim)
c
c  Reassign data (if it has been altered)
	   if(iflag.eq.1) then
		do 352 i=1,nx
352		xobs(i,j)=data(i,1)
	   	do 353 i=1,ny
	   	yobs(i,j)=data(i,2)
353	   	continue
	   endif
	   call PRINTSET(j,Xobs,Yobs,nx,ny,nmax,titled,titlex,titley,
     & setx,setvar,X1,Y1,D1,xbar,sdx,ybar,sdy,dbar,sdd)
	endif		!end of iopt=2
	goto 73	!any more alterations
c
75	continue
c option to change set title
	print 501
501	format(' Alter title for set number = ')
	call INPUTi(j)
	call TITENT(' Enter title for set:',titled(j),60)
	call TITENT(' Enter title for X column:',titlex(j),14)
	call TITENT(' Enter title for Y column:',titley(j),14)
	goto 73	!any more alterations
c
c Next option to alter
c
c76	continue
c	print 501
c501	format(' ')
c
59	goto 1101		!store on disc?
c
c
c READ YOBS,XOBS,Zobs,NSET,NJ FROM TERMINAL
102	continue
	print 103
103	FORMAT(' Number of data sets= ')
	call INPUTi(nset)
	j1=1		!get all sets from 1 to NSET
c jump to 203 to add a set
203	continue
c
	do 111 j=j1,nset
	print 106,J
106	FORMAT('&  Number of X (sample 1) observations in set ',I3,'= ')
	call INPUTi(njx(j))
	print 1061,j
1061	FORMAT('&  Number of Y (sample 2) observations in set ',I3,'= ')
	call INPUTi(njy(j))
	call TITENT(' Enter title for set:',titled(j),60)
	call TITENT(' Enter title for X column:',titlex(j),14)
	call TITENT(' Enter title for Y column:',titley(j),14)
111	CONTINUE
	SETVAR=.FALSE.
	print 331
331	FORMAT(' Set variable needed [N]? ')
C SET VARIABLE=A NUMBER (EG CONC) FOR EACH SET
	read 101,ans
	if(UC(ans).EQ.'Y') SETVAR=.TRUE.
c
c	heading='    X value       Y value'
	ncol=2
c
C  START LOOPS TO READ IN DATA
c
	if(.not.addset) j1=1	!read all sets
	do 105,j=j1,nset
	nx=njx(j)
	ny=njy(j)
	nrow(1)=nx		!for inwind2
	nrow(2)=ny		!for inwind2
	print 1093,J
1093	FORMAT(' Data set #',I3,/)
	IF(.NOT.SETVAR) GOTO 333
	print 332,J
332	FORMAT(' Set variable #',I3,' = ')
	call INPUTr(setx(j))
333	CONTINUE
	title=' SET '//char(j+48)//': '//charnb(titled(j))
	nlx=nblank(titlex(j))
	nly=nblank(titley(j))
	nl=nlx+nly
	spaces=' '
	ns=1
	if(nl.le.17) then
	   do 202 i=17,nl,-1
	   ns=ns+1
202	   spaces=spaces//' '
	endif
	if(ns.gt.14) ns=14
	heading='   '//titlex(j)(1:nlx)//spaces(1:ns)//titley(j)(1:nly)
	iflag=0	!display initially blank
	nmax=nx
	if(ny.gt.nx) nmax=ny
	nidisp=nmax
	if(nidisp.gt.20) nidisp=20	!best value- up to 20?
	iconst=0	!no constraint of columns
c
      call INWIND2(data,nmax,ncol,nidisp,title,heading,nr1,nc1,
     & iflag,iconst,nrow,nrdim,ncdim)
c
c allocate data
8	do 107,i=1,nx
107	xobs(i,j)=data(i,1)
	do 108 i=1,ny
108	yobs(i,j)=data(i,2)
	call PRINTSET(j,Xobs,Yobs,nx,ny,nmax,titled,titlex,titley,
     & setx,setvar,X1,Y1,D1,xbar,sdx,ybar,sdy,dbar,sdd)
105	continue	!end of jth set loop
c	if(addset) goto 1101
c
1101	continue
	addset=.false.	!if set earlier it is no longer needed
	print 116
116	FORMAT(' Store data on disc [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') GOTO 114
	ifile=0		!for write
	call RANDISK(Xobs,Yobs,nset,njx,njy,setvar,setx,
     & titled,titlex,titley,IFILE,FOPEN)
	if(nset.eq.-1) goto 999
c End of disc write
C
C NOW HAVE DATA. Do test
114	CONTINUE
	close(unit=12)
	fopen=.false.
c
191	continue
	if(nx.eq.ny) then
	   ans='N'			!default
	   if(paired) ans='Y'
	   call DCASK('Do paired test',ans,ans)
	   paired=ans.eq.'Y'
	endif
c
c Now decide which set to be tested (if more than one)
	jset=1	!pair of samples to be tested
	if(nset.gt.1) then
	   print 15,nset
15	   format(' Test data set number (1-',i3,') = ')
	   call INPUTi(jset)
	endif
	nx=njx(jset)
	ny=njy(jset)
	nmax=nx
	if(ny.gt.nx) nmax=ny
	call PRINTSET(jset,Xobs,Yobs,nx,ny,nmax,titled,titlex,titley,
     & setx,setvar,X1,Y1,D1,xbar,sdx,ybar,sdy,dbar,sdd)
c
c
c And do a 2-sample t-test
	if(paired) then
	   dobs=dbar
	   adobs=abs(dobs)
	   print 281,titlex(jset),titley(jset),dobs
281	   format(
     & ' For set: ',a14,' and ',a14,
     & ' Observed mean difference = ',g13.6)
	   sdbar=sdd/sqrt(float(ny))
	   tval=dbar/sdbar
	   ndf=nx-1
	   vr=tval*tval
	   ndf1=1
	   call FDIST(VR,ndf1,ndf,P)
         print 28,ndf,dbar,sdbar,tval,P
         if(pon()) write(7,28) ndf,dbar,sdbar,tval,P
         if(discprt) write(8,28) ndf,dbar,sdbar,tval,P
28	   format(/,
     &   ' Paired Student''s t test:',/,' t(',i3,') = ',
     &   g13.4,'/',g13.4,' = ',f9.3,/,' two tail P = ',g13.4,/)
	else
	   dobs=xbar-ybar
	   adobs=abs(dobs)
	   nlx=nblank(titlex(jset))
	   nly=nblank(titley(jset))
	   print 16,titlex(jset),xbar,titley(jset),ybar,dobs
16	   format(
     & ' X mean: ',a14,' = ',g13.6,' Y mean: ',a14,' = ',g13.6,/,
     & ' Observed difference between means= ',g13.6)
c
	   s=(sdx*sdx*float(nx-1)+sdy*sdy*float(ny-1))/float(nx+ny-2)
	   sdiff=sqrt(s*(1./float(nx)+1./float(ny)))
	   adiff=abs(xbar-ybar)
	   tval=adiff/sdiff
	   ndf=nx+ny-2
	   vr=tval*tval
	   ndf1=1
	   call FDIST(VR,ndf1,ndf,P)
         print 26,ndf,adiff,sdiff,tval,P
         if(pon()) write(7,26) ndf,adiff,sdiff,tval,P
         if(discprt) write(8,26) ndf,adiff,sdiff,tval,P
26	   format(/,
     &' Two-sample unpaired Student''s t test:',/,' t(',i3,') = ',
     & g13.4,'/',g13.4,' = ',f9.3,/,' two tail P = ',g13.4,/)
	endif
c
	if(paired) then
c put absolute differences into allobs() for paired test
	   do i=1,nx
		allobs(i)=abs(D1(i))
	   enddo
	else
c Put all obs into one array for unpaired test
	   k=0
	   stot=0.0
	   do i=1,nx
		k=k+1
		stot=stot+X1(i)
		ALLOBS(k)=X1(i)
         enddo
	   do i=1,ny
		k=k+1
		stot=stot+Y1(i)
		ALLOBS(k)=Y1(i)
	   enddo
	   ntot=nx+ny
	   if(k.ne.ntot) call BELL(10)
	endif
c
94	continue
	call RANDSK(ix,iy,iz,0,repeat)		!read IX,IY,IZ
c To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from DL1:
c
	print 19,nran
19	format(' Number of randomisations to be done [',i6,'] = ')
	call INPUTi(nran)
	print 192
192	format('&Type some of the samples [N] ? ')
	read 101,ans
	show=.false.
	if(UC(ans).eq.'Y') show=.true.
	if(show) then
	   print 193
193	   format('&Type samples of difference is between d1,d2= ')
	   call INPUT2r(d11,d12)
	endif
c
	enx=float(nx)
	eny=float(ny)
	ng1=0
	nl1=0
	na1=0
	ne1=0
	ne2=0
c
c Do paired test
	if(paired) then
	   do n=1,nran
		if(mod(n,1000).eq.0) then
		   call GETPOS(irow,icol)
		   call LOCATE(irow,0)
		   print 17,n
17		   format('&randomising . . .',i7)
		endif
		sd=0.0
		do i=1,nx
		   u=random()			!uniform(0,1)
		   if(u.lt.0.5) then
			sd=sd - allobs(i)
		   else
			sd=sd + allobs(i)
		   endif
		enddo
   		dran=sd/enx        !mean difference
		randiff(n)=dran
		if(dran.ge.dobs) ng1=ng1+1
		if(dran.le.dobs) nl1=nl1+1
		if(abs(dran).ge.adobs) na1=na1+1
		if(dran.eq.dobs) ne1=ne1+1
		if(abs(dran).eq.adobs) ne2=ne2+1
	   enddo
	   goto 502 	!skip unpaired test
	endif
c
	do 500 n=1,nran
	if(mod(n-1,1000).eq.0) then
	   deb=debug()
	   call GETPOS(irow,icol)
	   call LOCATE(irow,0)
	   print 17,n
c17	   format('&randomising . . .',i7)
	endif
c Choose NY random numbers in  range 0-ntot (will be bit faster if Yobs
c is the smaller sample). The LAST ny elements contain values after call
	call RANSAMP(ny,ntot,iran)
	sy=0.0
	do i=1,ny
	   j=ntot-i+1
	   sy=sy + allobs(iran(j))
	   if(deb) print 211,j,iran(j),allobs(iran(j))
211	   format(' j,iran(j),allobs(iran(j))= ',2i5,g13.6)
	enddo
	if(binomial) then
	   is2=ifixr(sy)
	   is1=irtot-is2
	   if1=n1-is1		!number of 'failures' in group 1
	   if2=n2-is2		!number of 'failures' in group 1
	   if(icrit.eq.1) then
		randiff(n)=float(is1)
		if(is1.ge.ir1obs) ng1=ng1+1
		if(is1.le.ir1obs) nl1=nl1+1
		if(is1.eq.ir1obs) ne1=ne1+1
	   else if(icrit.eq.2) then
		sx=stot-sy
		xb1=sx/enx        !mean
		yb1=sy/eny        !mean
		dran=xb1-yb1
		randiff(n)=dran
		if(dran.ge.dobs) ng1=ng1+1
		if(dran.le.dobs) nl1=nl1+1
		if(abs(dran).ge.adobs) na1=na1+1
		if(dran.eq.dobs) ne1=ne1+1
		if(abs(dran).eq.adobs) ne2=ne2+1
	   endif
c as check also calculate difference between p=r/n values to see when and
c if this criterion gives different result
	   sx=stot-sy
	   xb1=sx/enx        !mean
	   yb1=sy/eny        !mean
	   dran=xb1-yb1
	   gr1=is1.ge.ir1obs
	   gr2=dran.ge.dobs
	   if(gr1.neqv.gr2) then
		print 770,n,is1,if1,is2,if2
		if(discprt) write(8,770) n,is1,if1,is2,if2
770		format('  Randomisation number ',i8,/,
     &	   ' is1,if1,is2,if2 = ',4i8)
		print 771,sx,sy,xb1,yb1,dran
		if(discprt) write(8,771) sx,sy,xb1,yb1,dran
771		format(' sx,sy,xb1,yb1,dran = ',5g13.6)
		pause
	   endif
	else
	   yb1=sy
	   sx=stot-sy
	   xb1=sx/enx        !mean
	   yb1=sy/eny        !mean
	   dran=xb1-yb1
	   randiff(n)=dran
	   if(dran.ge.dobs) ng1=ng1+1
	   if(dran.le.dobs) nl1=nl1+1
	   if(abs(dran).ge.adobs) na1=na1+1
	   if(dran.eq.dobs) ne1=ne1+1
	   if(abs(dran).eq.adobs) ne2=ne2+1
	endif
c
	if(show) then
	   if(dran.gt.d11.and.dran.lt.d12) then
		do 195 i=1,ny
		j=ntot-i+1
		ysamp(i)=allobs(iran(j))
195		continue
		k=0
		do 196 i=1,nx
		xsamp(i)=allobs(iran(i))
196		continue
		call SHOWSET(xsamp,ysamp,nx,ny,nmax,
     &	 binomial,is1,if1,is2,if2,pon(),discprt)
		print 197,xb1,yb1,dran
197		format(' Means  ',g13.6,2x,g13.6,':  diff = ',g13.6)
	   endif
	endif
	if(deb) print 24,xb1,yb1,dran,ng1,nl1,na1
24	format(' xb1,yb1,dran,ng1,nl1,na1=',/,3g13.6,3i8)
500	continue	!end of loop for random samples
c
502	continue		!jump here if paired test
	pg1=float(ng1)/float(nran)
	pl1=float(nl1)/float(nran)
	pe1=float(ne1)/float(nran)
	if(.not.(binomial.and.icrit.eq.1)) then
	   pa1=float(na1)/float(nran)
	   pe2=float(ne2)/float(nran)
	endif
	if(paired) then
	   print 161,titlex(jset),titley(jset),dobs
	   if(pon()) write(7,161) titlex(jset),titley(jset),dobs
	   if(discprt) write(8,161) titlex(jset),titley(jset),dobs
	   print 251,nran,pg1,pl1,pa1,ne1,ne2
	   if(pon()) write(7,251) nran,pg1,pl1,pa1,ne1,ne2
         if(discprt) write(8,251) nran,pg1,pl1,pa1,ne1,ne2
251	   format(/,
     &    1x,i8,' randomisations',/,
     &    ' P values for mean difference were:',/,
     &    '  greater than or equal to observed: P = ',g13.6,/,
     &    '  less than or equal to observed: P = ',g13.6,/,
     &    '  greater than or equal in absolute value to observed: P = '
     &	,g13.6,/,/,
     &     ' (Number equal to observed = ',i6,')',/,
     &     ' (Number equal in absolute value to observed = ',i6,')',/)
	else
	   if(binomial.and.icrit.eq.1) then
	      print 161,ir1,n1,p1,sd1,ir2,n2,p2,sd2,dobs
	      if(discprt) write(8,161) ir1,n1,p1,sd1,ir2,n2,p2,sd2,dobs
	      print 252,nran,pg1,pl1,ne1,pe1
            if(discprt) write(8,252) nran,pg1,pl1,ne1,pe1
252	      format(/,
     &       1x,i8,' randomisations',/,
     &       ' P values for difference between sets are:',/,
     &       '  r1 greater than or equal to observed: P = ',g13.6,/,
     &       '  r1 less than or equal to observed: P = ',g13.6,/,
     &       '  r1 equal to observed: number = ',i6,' (P = ',g13.6,')')
	   else if(binomial.and.icrit.eq.2) then
	      print 161,ir1,n1,p1,sd1,ir2,n2,p2,sd2,dobs
	      if(discprt) write(8,161) ir1,n1,p1,sd1,ir2,n2,p2,sd2,dobs
	      print 253,nran,pg1,pl1,pa1,ne1,pe1,ne2,pe2
            if(discprt) write(8,253) nran,pg1,pl1,pa1,ne1,pe1,ne2,pe2
253	      format(/,
     &       1x,i8,' randomisations',/,
     &       ' P values for difference between p1 and p2 are:',/,
     &       '  greater than or equal to observed: P = ',g13.6,/,
     &       '  less than or equal to observed: P = ',g13.6,/,
     &     '  greater than or equal in absolute value to observed: P = '
     &	 ,g13.6,/,/,
     &       ' Number equal to observed = ',i6,' (P = ',g13.6,')',/,
     &       ' Number equal in absolute value to observed = ',i6,
     &       ' (P = ',g13.6,')',/)
	   else
	      print 16,titlex(jset),xbar,titley(jset),ybar,dobs
	      if(discprt)write(8,16) titlex(jset),xbar,
     &	   titley(jset),ybar,dobs
	      print 25,nran,pg1,pl1,pa1,ne1,pe1,ne2,pe2
            if(discprt) write(8,25) nran,pg1,pl1,pa1,ne1,pe1,ne2,pe2
25	      format(/,
     &       1x,i8,' randomisations',/,
     &       ' P values for difference between means are:',/,
     &       '  greater than or equal to observed: P = ',g13.6,/,
     &       '  less than or equal to observed: P = ',g13.6,/,
     &     '  greater than or equal in absolute value to observed: P = '
     &	 ,g13.6,/,/,
     &       ' Number equal to observed = ',i6,' (P = ',g13.6,')',/,
     &       ' Number equal in absolute value to observed = ',i6,
     &       ' (P = ',g13.6,')',/)
	   endif
	endif
c
	sval=dobs
	if(binomial.and.icrit.eq.1) sval=float(ir1obs)
	call RANDSP(randiff,nran,sval,paired,binomial,icrit)
	print 20
20	format('&Another run (same data; new random numbers) [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   if(binomial) goto 94
	   goto 191
	endif
	print 13
13	format('&Another run with new data [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(ans.eq.'Y') goto 12
c
	call RANDSK(ix,iy,iz,1,repeat)		!write IX,IY,IZ
	call flush(7)
	call ENDPRINT
999	continue
	END

	subroutine PRINTSET(j,Xobs,Yobs,nx,ny,nmax,titled,titlex,titley,
     & setx,setvar,X1,Y1,D1,xbar,sdx,ybar,sdy,dbar,sdd)
c First line of params+setx,setvar=input
c X1 to SDY=output
	real YOBS(500,10),XOBS(500,10),setx(10)
	real Y1(500),X1(500),D1(500)	! 1 D arrays for test
	character*60 TITLED(10)	!data set names
	character*79 heading
	character*14 titlex(10),titley(10),spaces	!col headings
	logical discprt,slock,pon,setvar
	common/dp/discprt
c
	pon()=slock()
c
	do 19 i=1,14
19	spaces(i:i)=char(32)
c First define X1,Y1 in sep loop before printing (allows print loop to
c be used to print ranks if req)
	do 114 i=1,nmax
	if(i.le.nx.and.i.le.ny) then
	   X1(i)=Xobs(i,j)	!values for current set
         Y1(i)=Yobs(i,j)	!values for current set
	   D1(i)=Xobs(i,j)-Yobs(i,j)	!differences for paired test
	else if(i.le.nx.and.i.gt.ny) then
	   X1(i)=Xobs(i,j)	!values for current set
	else if(i.gt.nx.and.i.le.ny) then
         Y1(i)=Yobs(i,j)	!values for current set
	endif
114	continue		!end of i loop
c Print out adjusted values
	print 10,j,titled(j)
	if(pon()) write(7,10) j,titled(j)
      if(discprt) write(8,10) j,titled(j)
10	format('  Data set # ',i3,3x,a60)
	IF(SETVAR) print 334,SETX(J)
334	FORMAT( ' Set variable= ',G13.6)
	nlx=nblank(titlex(j))
	nly=nblank(titley(j))
c	nl=nlx+nly
	ns=2		!at least 2 spaces between col headings
	if(nlx.le.14) ns=15-nlx
c	spaces=' '
c	ns=1
c	if(nl.le.17) then
c	   do 202 i=17,nl,-1
c	   ns=ns+1
c202	   spaces=spaces//' '
c	endif
	heading='          '//titlex(j)(1:nlx)//spaces(1:ns)//
     & titley(j)(1:nly)
	print 39,heading
	if(pon()) write(7,39) heading
	if(discprt) write(8,39) heading
39	format(1x,a79)
c	print 39
c	if(pon()) write(7,39)
c	if(discprt) write(8,39)
c39	format(
c     & ' Obs #       X value      Y value')
cc     & ' Obs #       X value      Y value           s(Y)       weight')
	do 115 i=1,nmax
	if(i.le.nx.and.i.le.ny) then
	   print 11,i,X1(i),Y1(i),D1(i)
	   if(pon())write(7,11)i,X1(i),Y1(i),D1(i)
	   if(discprt)write(8,11)i,X1(i),Y1(i),D1(i)
11	   format(I4,3X,3(2X,G13.6))
	else if(i.le.nx.and.i.gt.ny) then
	   print 12,i,X1(i)
	   if(pon())write(7,12)i,X1(i)
	   if(discprt)write(8,12)i,X1(i)
12	   format(I4,3X,2X,G13.6)
	else if(i.gt.nx.and.i.le.ny) then
	   print 13,i,Y1(i)
	   if(pon())write(7,13)i,Y1(i)
	   if(discprt)write(8,13)i,Y1(i)
13	   format(i4,20x,G13.6)
	endif
115	continue		!end of i loop
c print mean,var etc
	renx=sqrt(float(nx))
	reny=sqrt(float(ny))
	call VAR(X1,nx,xbar,varx)
	call VAR(Y1,ny,ybar,vary)
	sdx=sqrt(varx)
	sex=sdx/renx
	sdy=sqrt(vary)
	sey=sdy/reny
	print 14,nx,ny,xbar,ybar,sdx,sdy,sex,sey
	if(pon())write(7,14) nx,ny,xbar,ybar,sdx,sdy,sex,sey
	if(discprt) write(8,14) nx,ny,xbar,ybar,sdx,sdy,sex,sey
14	format(
     & '____________________________________',/,
     & ' n   ',3x,2(2x,i8),/,
     & ' Mean',3x,2(2x,g13.6),/,' s(y)',3x,2(2x,g13.6),/,
     & ' s(ybar)',2(2x,g13.6))
c
	if(nx.eq.ny) then
	   call VAR(D1,nx,dbar,vard)
	   sdd=sqrt(vard)
	   sed=sdd/renx
	   print 15,nx,dbar,sdd,sed
	   if(pon())write(7,15) nx,dbar,sdd,sed
	   if(discprt) write(8,15) nx,dbar,sdd,sed
15	   format(/,
     & '____________________________________',/,
     & ' n = ',i5,/,
     & ' Mean difference (dbar) = ',g13.6,/,
     & ' s(d) = ',g13.6,' s(dbar) = ',g13.6,/,/)
	endif
c
c
	RETURN
	end


	subroutine SHOWSET(x1,y1,nx,ny,nmax,
     & binomial,is1,if1,is2,if2,pon,discprt)
	real x1(nx),y1(ny)
	logical pon,discprt,binomial
c
c	if(binomial) goto 2
	do 115 i=1,nmax
	if(i.le.nx.and.i.le.ny) then
	   print 11,i,X1(i),Y1(i)
	   if(pon)write(7,11)i,X1(i),Y1(i)
	   if(discprt)write(8,11)i,X1(i),Y1(i)
11	   format(I4,3X,2(2X,G13.6))
	else if(i.le.nx.and.i.gt.ny) then
	   print 12,i,X1(i)
	   if(pon)write(7,12)i,X1(i)
	   if(discprt)write(8,12)i,X1(i)
12	   format(I4,3X,2X,G13.6)
	else if(i.gt.nx.and.i.le.ny) then
	   print 13,i,Y1(i)
	   if(pon)write(7,13)i,Y1(i)
	   if(discprt)write(8,13)i,Y1(i)
13	   format(i4,20x,G13.6)
	endif
115	continue		!end of i loop
c	goto 99
c Now section for binomial case
	print 3
	if(discprt) write(8,3)
3	format(' Random 2x2 table:')
	print 4, is1,if1,is1+if1
	if(discprt) write(8,4) is1,if1,is1+if1
4	format(' Set 1:',i4,2x,i4,4x,i4)
	print 5, is2,if2,is2+if2
	if(discprt) write(8,5) is2,if2,is2+if2
5	format(' Set 2:',i4,2x,i4,4x,i4)
	print 6, is1+is2,if1+if2,is1+is2+if1+if2
	if(discprt) write(8,6) is1+is2,if1+if2,is1+is2+if1+if2
6	format(' Total:',i4,2x,i4,4x,i4)
99	continue
	RETURN
	end


