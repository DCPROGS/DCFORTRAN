	subroutine HJCSIM1(nsim,nsims,simfile,simfile1,nset,conc,nintt,
     & nfile,pfile,qfile,
     & ligname,defname,readini,irect,nmax,apfile,repeat)

c To set up for generating a simulated .scn file in HJCIT
c True rates for simulation are read later, after constraints specified
c Actually simulation to make tint0 etc is done in call to HJCSIM2

c  Here must specify
c (a) nset= number of sets, and conc(i,j)
c (b) true rates (thtrue() in hjcfit) used to make simulated obs
c Rather than defining icur(i,j) as current amplitude for state i, channel j
c just use dgamma().  Amplitudes irrelevant in HJCFIT
c As in hjcdat1,
c  NINTT(j)=total no of intervals in set j before resolution imposed
c  NMAX=maximimum nintt(j) (=allocated dimension of tint0() etc).
c
	real*4 conc(10,10)
	integer nfile(10),nintt(10)
	character*20 ligname(10)
	character*2 ndev
	character defname*6
	character*11 cnum
	character*40 simfile,simfile1,qfile	!file names for output of simulation
	logical readini,repeat,abort
	character*33 pfile(20,10)	!path names for .SCN files
	logical present
	logical apfile
c For qmechs
	character ans*1
c  For parsname etc
	character path*30,pname*8,suffix*3	!for parsname
	logical nopath
C
c	character*74 mtitle	!title for model
c	character*2 charmod(25,40)	!to print model
c	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
c Commons with getqd
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
c=	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	COMMON/LIG/nlig,IL(100)
	integer IQ(100,100)
	COMMON/qblk/IQ
	logical discprt
	common/dp/discprt
c Addition for getbound
	integer nbound(100,10)
	common/nbnd/nbound
c
	common/rand/ix1,iy1,iz1
c
c
c True rates for simulation now defined
	print 412,nset
412	format(
     &' Number of different data sets (eg concentrations) [',i2,'] = ')
	call INPUTi(nset)
	nmax=0
	do j=1,nset
	   nfile(j)=1	!always for simulation
	enddo
c Replace next bit with call to GETCONC?
	do j=1,nset
	   if(nlig.gt.0.and.readini) then
		print 163,j
163		format(' SIMULATED DATA FOR SET ',i2,
     &	' Last run used concentration(s) (micromolar):',/)
		do i=1,nlig
		   xs=1.e6*conc(i,j)
		   print 164,i,ligname(i),xs
164		   FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		enddo
		call DCASK('Change the concentration(s)','n',ans)
	   else
		ans='Y'
	   endif
	   if(nlig.gt.0) then
		if(ans.eq.'Y') then
		   do i=1,nlig
			xs=1.e6*conc(i,j)
			print 159,i,ligname(i),xs
159			FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
			call INPUTr(xs)
			conc(i,j)=xs*1.0e-6
		   enddo
c		   if(discprt) write(8,108)
108		format(/,'------------------------------------------------')
		   do i=1,nlig
			xs=1.e6*conc(i,j)
c			if(discprt) write(8,164) i,ligname(i),xs
c164		   FORMAT(/,
c     &      ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		   enddo
c		   if(discprt) write(8,108)
		endif
	   endif
	   call INTCONV(j,cnum)
	   pfile(1,j)='sim'//charnb(cnum)//'.dat'	!define this for plotq
	   defname=pfile(1,j)(1:4)	!='sim#'
c	   call TITENT0('Name for simulated .scn file:',
c     &	pfile(1,j),33,.false.)
c
	   nintt(j)=20000		!default
	   print 3,nintt(j)
3	   format(' Number of intervals to be simulated [',i6,'] = ')
	   call INPUTi(nintt(j))
	   if(nintt(j).gt.nmax) nmax=nintt(j)
	enddo		!j=1,nset
c
c Now calculate amean(), peq() and picum() arrays that are used for actual
c simulation (these never change so calculate here). BUT they are different
c for each set, so perhaps better to calculate them at same time as simulation
c (time taken to do so is negligible compared with that needed for fitting)
c
c
c nbstmax not used in sim_hjc (see simsc.for)
c	nbstmax=5000
c	print 110,nbstmax
c110	format(' Maximum number of openings/burst expected [',i8,'] = ')
c	call INPUTi(nbstmax)
c
c Get random seeds
c (use ix1 to avoid clash with ix(10) array!)
c To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from DL1:
	call RANDSK(ix1,iy1,iz1,0,repeat)		!read IX,IY,IZ
c
c  Define output files
c
	if(nsim.lt.1) nsim=1
	if(nsim.gt.10000) nsim=10000
	print 721,nsim
721	format(' Number of experiments to be simulated [',i5,'] = ')
	call INPUTi(nsim)
	nsims=1	!number of simulated expts fitted =1,2,...,nsim
c
9	continue
c	if(nsim.gt.1) then
	   simfile1='SIMVAL.DAT'
	   call TITENT0(
     & 'File name for binary output of simulations',simfile1,40,.false.)
	   INQUIRE(file=SIMFILE1,exist=present,flen=len)
	   if(present.and.len.gt.0) then
		OPEN(unit=10,file=simfile1,status='UNKNOWN',
     &	   access='TRANSPARENT')
		read(unit=10,rec=1) iver,nsims1,abort
		CLOSE(unit=10)
		print 10,simfile1,nsims1
10		format(' There is already a file called ',a40,/,
     &      ' which contains ',i5,' simulations',/,
     &	' Append results to the existing file [Y] ')
		ans='Y'
		call INPUTa(ans)
		apfile=ans.eq.'Y'	!append file
	   endif
c Now the text output file
	   call PARSNAME(simfile1,path,ndev,pname,suffix,nopath,40)
	   simfile=charnb(pname)//'.txt'
	   defname=charnb(pname(1:6))
	   call TITENT0(
     &  'File name for text output of simulations',simfile,40,.false.)
	   INQUIRE(file=SIMFILE,exist=present,flen=len)
	   if(apfile.and.(.not.present.or.len.eq.0)) then
		call BELL(3)
		print 4,simfile
4		format(' Cannot find the corresponding text file ',a40)
		goto 9
	   endif
c And plotq file name
	   qfile=charnb(pname)//'.plq'
	   call TITENT0(
     &  'File name for plot queue file',qfile,40,.false.)
c If append, check next simulation number
c already in it
	   if(.not.apfile) then
		nsims=1
	   else if(apfile) then
	   endif
c          OPEN(unit=9,file=SIMFILE,status='UNKNOWN',
c     &    access='APPEND',form='FORMATTED',carriage control='FORTRAN')
c		if(ans.eq.'N') REWIND(unit=9)		!does not work here
c	   CLOSE(unit=9)
c If append, check that it is there already, and see how many simulations
c already in it
	   if(apfile) then
		nsims=nsims1+1	!number of simulated expts fitted =1,2,...,nsim
		print 722,nsims
722		format(
     &   ' To continue an earlier run, enter next simulation #',/,
     &   '  -start numbering at # [',i4,'] = ')
		call INPUTi(nsims)
	   endif
c	endif
c
	if(apfile) then
	   if(discprt) write(8,1) simfile,simfile1
1	   format(
     & ' Results of simulation will be appended as text to ',a40,/,
     & ' and as binary to ',a40)
	else
	   if(discprt) write(8,2) simfile,simfile1
2	   format(
     & ' Results of simulation will be written as text to ',a40,/,
     & ' and as binary to ',a40)
	endif
c
	RETURN
	END

