	program SCSIM
c
c To generate a random series of transitions for a given
c Q matrix
c Further analysis can be done in EKDIST,SIMCHAN...
c
c Modif 11/23/01 11:00am for obeymr(50). Rather than add this to .ini,
c set elements of nsc(i) negative (for .ini ONLY) if obeymr(i)=false
c
c Modif 11/14/01 11:43am for 100 state/10 ligand
c
c Modif 05/11/00 02:15pm
c   iscan=-103	data from SCSIM written in new scan.dat format (vs
c====check iscan=-103 in other progs
c   iscan=-3001 for files written to scan.dat disc)
c
c Modif for QGEN.INI etc 11/24/94 09:41am
c Modif 12/23/89 08:57pm. TINT, iAMPL enlarged, and tint1,tint2 now 4 (rather
c than 5) times bigger (to allow for elim of short events when resolution
c imposed). See def of ndim,ndim1 below. And TINSIM,ISTATE removed (used
c before to display indiv state transitions at end).
c Max is still 1024 if transitions are
c to be written to an expt file, but all NDIM can be written to
c a special SCSIM.DAT file for further inspection (eg by SIMCHAN.FOR)
c TINSIM,ISTATE removed for now.
c
c Modif March 89 so reads Q defined in QGEN1
c Modif May 87 to  use RANDOM() for transitions, and to use new
c routines QMOD, QDEF to define Q matrix
	allocatable::tint
	allocatable::iampl
	allocatable::iprops
	real*4 tint(:)
	integer*2 iampl(:)
	integer*1 iprops(:)
	integer nsim(100)
	real*4 pisim(100,100),tsim(100)
	real*4 Q(100,100),pi(100,100),picum(100,100)
	REAL*8 QT(100,100),Q1(100,100),Pinf(100)
	real*8 QT1(100,100)	!copy of QT for common (used in derivsim)
C QM IS TO HOLD MINORS OF Q IN EQOCC. SEEMS THAT IT MUST BE
C DECLARED IN MAIN PROG FOR CORRECT CALCS (SEE COMMENTS IN EQOCC)
	real*8 dgamma(100)		!for getqd
	character*10 titlep(200)	!for getqd
	real*4 Peq(100)
cc	real*4 pstar(4),amean(10),gamma(10)
	real*4 amean(100),gamma(100)
	integer*2 icur(100,100),ilast,icurmax
	character*1 ans,UC
	logical repeat,setres,readini,present
	character*40 path
c	character ndev*2
c Following needed for disc read (only ix,jx,il actually used)
c=	integer IX(10),JX(10),IL(10)
	logical cjump,cjsav
	logical debug,caplock,errflag
	character*11 cdate,ctime
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c For new version that uses qmechs.dat
	real*4 conc(10),concc(10),concb(10),x
	common/cconc1/conc0(10),conc1(10),conc2(10),nljump !for jumps (and .ini)
	integer irate(200),jrate(200)
	character*20 ligname(10)
c
	logical discprt
	logical student,cluster
c
	common/dp/discprt
	COMMON/KBLK/KA,KB,KC,KD
	COMMON/dimblk/K,KMAX,ir,nmod
c	common/Vval/vhold,vkin,vref,gamr,enchan		!V in mV
c===Commons for getqd
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/LIG/nlig,IL(100)
c for eqoccd, getqd
	character qfilem*40
	character*74 mtitle1
c==	character*2 charmod(20,30)	!to print model
	character*2 charmod(25,40)	!to print model
c  (NB ilast is *2 here)
	common/model/imod0,charmod,jlast,ilast1,mtitle1,imodold
c
	common/rand/ix1,iy1,iz1
	integer*2 iacrit
	common/RBLCK/treso,tresg,avamp,iacrit
c=	common/qblk1/QD1,cA1,cB1		!for derivsim
	common/qblk1/QT1,concc,v1		!for derivsim
	common/user/student,cluster,iwindows
c
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
c
c Modif 06/25/03 09:06am in getqd to put npar and irate, jrate also in commons
c  Common for GETQD,calcmr/dfpmin/getrev
	common/np1/npar
	common/ir/irate,jrate
c
c  For getqd to enable read/write of models defined in Windows version
	character*3 snumw(100)		! state number
	integer*4 icolwin(100)		! colour for open/schut
	real*4 wposx(100),wposy(100)	! position
	integer*4 nwidwin,nhghtwin	!window dimensions
	common/winmod/snumw,icolwin,wposx,wposy,nwidwin,nhghtwin
c
c
	debug()=caplock()
c
c
	errflag=.true.
	call UNDFL(errflag)
	cjump=.false.
	njump=0
	tjmax=0.0
c
	cluster=.false.
	student=.false.
	call MYPATH(path)
	if(path(1:8).eq.'O:\CVFIT') cluster=.true.
	filnam='SCSIM.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(' SCSIM: Single channel simulation ',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
	kmax=100		!dimension of Q
	setres=.true.	!impose resolution if setres=true
c
C READ DATA FROM DISC
c Read the INIT file (tres kept in microsec here)
	readini=.false.
	ans='Y'
	call DCASK('Read defaults from last run from disc',ans,ans)
      if(UC(ans).ne.'N') then
	 INQUIRE(file='SCSIM.INI',exist=present,flen=nlen)
	 if(present.and.nlen.gt.0) then
	   readini=.true.
	   if(nlen.eq.1024) then		!old.ini
		OPEN(unit=19,file='SCSIM.INI',status='UNKNOWN',
     &	access='DIRECT',form='UNFORMATTED',recl=1024)
		read(19,rec=1) treso,tresg,xa11,xb11,irecq,
     & 	 cjump,icalc,tpulse,tdec,trise,tjmax,repeat,ktmax,cmax1,
     &	 kA,kB,kC,kD,imodold
		cjsav=cjump
		if(irecq.lt.1) irecq=1		!temp!
		concb(1)=xa11
		concb(2)=xb11
c irecq=file # for rates
		CLOSE(unit=19)
	   else		!read new .ini
	     	OPEN(unit=19,file='SCSIM.INI',status='UNKNOWN',
     &    	access='DIRECT',form='UNFORMATTED',recl=10240)
		read(19,rec=1) iver,treso,tresg,concb,irecq,
     & 	 cjump,icalc,tpulse,tdec,trise,tjmax,repeat,ktmax,cmax1,
     &	 kA,kB,kC,kD,imodold,conc0,conc1,conc2,nljump,
     &	ncyc,(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	qfilem
		if(UC(qfilem(1:1)).ne.'Q') qfilem='qmechs.mec'
		cjsav=cjump
		if(irecq.lt.1) irecq=1		!temp!
		do i=1,ncyc		!if nsc(i) is neg, set obeymr(i)=F and restore nsc
		   if(nsc(i).lt.0) then
			obeymr(i)=.false.
			nsc(i)=iabs(nsc(i))
		   else
			obeymr(i)=.false.
		   endif
		enddo
		CLOSE(unit=19)
	   endif
	 endif
	endif
c
c 11/27/97 08:55am Now use GETQD to define Q matrix
c kflag=0 to omit question about reallocation of kA,kB,kC,kD (e.g
c   these not needed in SCALCS or SCSIM) or
c kflag=1 to ask if realloc required.
c kflag=2 to use input values of kA etc (e.g. from .ini) and ignore
c   values from disk
c Call with idest=105 to go straight to bit that changes current params;
c  if idest=105 then value(s) of changed params are printed
c Many arrays in commons so those needed can be transmitted to calling prog
	idest=0
	kflag=0
	iprint=0
c
c Note GETQD now returns QT without conc (or diagonals)
c Modif 06/25/03 09:06am in getqd to put npar and irate, jrate also in commons
	call GETQD(QT,nchan,dgamma,vkin,vhold,vref,
     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
     &  kflag,idest)
c	call GETQD(QT,irate,jrate,nchan,dgamma,vkin,vhold,vref,
c     &  npar,titlep,ligname,iflag,iprint,readini,irecq,qfilem,
c     &  kflag,idest)
c
	k=kA+kB+kC+kD
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c
	do i=1,k
	   gamma(i)=1.e12*sngl(dgamma(i))		!in pS
	enddo
	do i=1,k
	   do j=1,k
		QT1(i,j)=QT(i,j)*1.0d-3		!copy for common, in 1/ms
	   enddo
	enddo
	if(readini) then
	   do i=1,10
		conc(i)=concb(i)	!overwrite with conc from last run (SCSIM.ini)
	   enddo
	endif
	do i=1,10
	   concc(i)=conc(i)		!copy for common
	enddo
c
c
	if(cjsav) then
	   iopt=2
	else
	   iopt=1
	endif
	print 30,iopt
30	format(/,
     & ' (1) Simulate a steady state record',/,
     & ' (2) simulate concentration jumps',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	cjump=iopt.eq.2
	nchan=1
	if(.not.cjump) then
	   print 4
4	   format(' Number of channels [1] = ')
	   call INPUTi(nchan)
	endif
	do 11 j=1,nchan
	 print 12,j
12	 format(' Channel # ',i3,':'/)
	 do 13 i=1,k
	  icur(i,j)=0
	  if(i.le.ka) then
c	   ia=50	!default
	   ia=ifixr(gamma(i))*0.1
	   print 14,i,ia
14	   format(
     &   '&''Amplitude'' for state #',i3,' (pos integer [',i3,']) = ')
	   call INPUTi(ia)
	   icur(i,j)=int2(ia)
	   icur(i,j)=i2abs(icur(i,j))	!ensure positive
	  endif
13	 continue
11	continue
c
	icurmax=0
	do j=1,nchan
	   print 12,j
	   if(discprt) write(8,12) j
	   do i=1,ka
		print 17, i,icur(i,j)
		if(discprt) write(8,17) i,icur(i,j)
17		format('   state #',i3,'  amplitude = ',i5)
c         NB i2abs() is integer*2 with int*2 argument in F90
	      if(i2abs(icur(i,j)).gt.icurmax) icurmax=icur(i,j)
	   enddo
	enddo
c
c
	if(cjump) goto 35		!concentrations defined in simjump
c
	if(nlig.gt.0) then
	   print 163
163	   format(' Last run used concentration(s) (micromolar):')
	   do i=1,nlig
		xs=1.e6*conc(i)
		print 164,i,ligname(i),xs
164		FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
	   enddo
	   call DCASK('Change the concentration(s)','n',ans)
	   if(ans.eq.'Y') then
		do i=1,nlig
		   xs=1.e6*conc(i)
		   print 159,i,ligname(i),xs
159		   FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
		   call INPUTr(xs)
		   conc(i)=xs*1.0e-6
		enddo
		if(discprt) write(8,108)
108		format(/,'------------------------------------------------')
		do i=1,nlig
		   xs=1.e6*conc(i)
		   if(discprt) write(8,164) i,ligname(i),xs
c164		   FORMAT(/,
c     &      ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		enddo
		if(discprt) write(8,108)
c
c=	      call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,kmax)
  		call QSETD(conc,IL,V1,QT,Q1,k,.false.)
	      call EQOCCd(Q1,k,k-1,kmax,pinf)	!calc new init occs
		kdim=100
		allmr=.true.
		do i=1,ncyc
		   if(.not.obeymr(i)) allmr=.false.
		enddo
	      if(allmr) then
		   call MRTEST(Q1,pinf,' SCSIM  ',ifcall,k,kdim)
		endif
		call SLIFED2(Q1,pinf,.false.,discprt,kmax,kmax)
	  else

		if(discprt) write(8,108)
		do i=1,nlig
		   xs=1.e6*conc(i)
		   if(discprt) write(8,164) i,ligname(i),xs
c164		   FORMAT(/,
c     &      ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		enddo
		if(discprt) write(8,108)
c
  		call QSETD(conc,IL,V1,QT,Q1,k,.false.)
	      call EQOCCd(Q1,k,k-1,kmax,pinf)	!calc new init occs
		call SLIFED2(Q1,pinf,.false.,discprt,kmax,kmax)
	  endif
	else if(nlig.eq.0) then
  	   call QSETD(conc,IL,V1,QT,Q1,k,.false.)
	   call EQOCCd(Q1,k,k-1,kmax,pinf)	!calc new init occs
	   call SLIFED2(Q1,pinf,.false.,discprt,kmax,kmax)
	endif
c
	do i=1,k
	   peq(i)=sngl(pinf(i))
	   nsim(i)=0
	   tsim(i)=0.
	enddo
c Set single channel currents here (no need for gamma, vkin etc; just set
c values as arbitrary integers in ICUR, and set ACAL=1.0.
c ADDITION 12/22/89 12:02pm: 	if there is more than one channel add option
c to set conductance differently for each (channels are assumed identical
c but doing this will show which of the channels was open in the summed
c record, eg in order to tell whether all openings in a burst came from
c the same channel). Keep the 'amplitude' in ICUR(i,j)='amplitude' for
c the ith state, jth channel
	do i=1,k
	   print 36,i,peq(i)
	   if(discprt) write(8,36) i,peq(i)
36	   format(' State #',i3,': equilibrium occupancy = ',g13.6)
	   do j=1,k
		Q(i,j)=sngl(Q1(i,j))
	   enddo
	enddo
c
c calc mean lifetimes
	do i=1,k
	   amean(i)=-sngl(1.d3/q1(i,i))
	enddo
c
c Test data
c	k=4
c	q(1,2)=100.
c	q(1,3)=0.
c	q(1,4)=300.
c	q(2,1)=10.
c	q(2,3)=50.
c	q(2,4)=60.
cc	q(2,4)=0.
c	q(3,1)=0.
c	q(3,2)=400.
c	q(3,4)=40.
c	q(4,1)=500.
c	q(4,2)=400.
cc	q(4,2)=0.
c	q(4,3)=200.
c	do 4 i=1,k
c	q(i,i)=0.
c	do 5 j=1,k
c5	if(i.ne.j) q(i,i)=q(i,i)-q(i,j)
c4	continue
c End of test data
c
	ans='Y'
	call DCASK('Type Q matrix',ans,ans)
	if(UC(ans).ne.'N') call ATYPE(Q,' Q      ',k,k,kmax,kmax)
cc
c Calc PI
	do i=1,k
	 do j=1,k
	  pi(i,j)=-q(i,j)/q(i,i)
	 enddo
	enddo
	ans='Y'
	call DCASK('Type PI matrix',ans,ans)
	if(UC(ans).eq.'Y')call ATYPE(pi,' PI     ',k,k,Kmax,kmax)
c
c Calc cumulative PI matrix
	do i=1,k
	 do j=1,k
	  picum(i,j)=0.
	  do n=1,j
	   if(n.ne.i) picum(i,j)=picum(i,j)+pi(i,n)
	  enddo
	 enddo
	enddo
c
	if(debug())call ATYPE(picum,' PI(cum)',k,k,Kmax,kmax)
c
c Jump here for cjump -all conc defined in simjump
35	continue
c
	icmax=0
	do i=1,k
	   do j=1,nchan
		if(iabs(int4(icur(i,j))).gt.icmax) icmax=icur(i,j)
	   enddo
	enddo
	iavamp=icmax	!'full level' = largest conductance
	avamp=float(iavamp)
c
c Make special file to hold seed for random number gen
c (called IR1,IR2 here; IRAN1, IRAN2 are ignored)
c Now also has seeds for RANDOM (=ix,iy,iz)
c	repeat=.true.			!default -get from .ini
	call RANDSK(ix1,iy1,iz1,0,repeat)		!read IX,IY,IZ
c (use ix1 to avoid clash with ix(10) array!)
c To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from DL1:
c
	if(ktmax.lt.10) ktmax=10000
	print 10,ktmax
10	format(' Number of intervals to be simulated [',i8,'] = ')
	call INPUTi(ktmax)		!intervals, not transitions
	nbmax=5000
	print 110,nbmax
110	format(' Maximum number of openings/burst expected [',i8,'] = ')
	call INPUTi(nbmax)
	ndim=ktmax
	ALLOCATE(tint(ktmax),iampl(ktmax),iprops(ktmax))
	do i=1,ktmax
	   iprops(i)=0
	enddo
	if(cjump) then
	   print 22,tjmax
22	   format(' Length of sample after the jump (ms) [',f10.1,'] = ')
	   call INPUTr(tjmax)
	   print 23,tjmax
	   if(discprt) write(8,23) tjmax
23	   format(/,
     &  ' Simulation of c-jumps: sample length = ',f10.1,' ms')
	endif
c
c Start loop to generate transitions
21	continue		!return to repeat run
c
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	do i=1,ncyc
	   if(.not.obeymr(i)) then
		nsc(i)=-iabs(nsc(i))
	   endif
	enddo
	OPEN(unit=19,file='SCSIM.INI',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=10240)
	  write(19,rec=1) iver,treso,tresg,concb,irecq,
     &      cjump,icalc,tpulse,tdec,trise,tjmax,repeat,ktmax,cmax1,
     &      kA,kB,kC,kD,imodold,conc0,conc1,conc2,nljump,
     &	ncyc,(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc)
	do i=1,ncyc	     !reset nsc
	   nsc(i)=iabs(nsc(i))
	enddo
	CLOSE(unit=19)
c
	if(.not.cjump) then
	   call SIMSC(tint,iampl,pisim,nsim,tsim,
     &    picum,icur,amean,peq,topen,nopen,nt,ndim,nt1,kt,ktmax,
     &    ilast,ttot,nchan,setres,kA,kB,kC,kD,nbmax)
	else
	   call SIMJUMP(tint,iampl,iprops,tjmax,pisim,nsim,tsim,
     &	 QT,icur,ndim,nt1,kt,ktmax,vhold,vref,ligname,
     &	 concb,vkin,ilast,icalc,cmax1,tpulse,tdec,trise,njump)
	endif
c
	ans='Y'
	call DCASK('Write results to experiment (SCAN.SCN) file',ans,ans)
	if(UC(ans).eq.'N') goto 99
	print 274,kt
274	format(' Number of usable intervals= ',i6)
c	iscan=-3001	!data from SCSIM
	iscan=-103	!data from SCSIM written in new scan.dat format
c Call DSKWRIT1 with debug
	calfac2=1.0		!for DSKWRT only
	ffilt=-1.
	rms=0.0
	nint=kt
	call DSKWRIT2(tint,iampl,iprops,nint,ffilt,rms,avamp,
     & calfac2,cdate,iscan,treso,tresg,vkin,ndim,
     & cjump,njump,tjmax)
c
99	continue
	if(.not.cjump) then
	   ans='N'
	   call DCASK('Write results to a simulated CONSAM file',ans,ans)
	   if(ans.eq.'Y') then
		nint=kt
		call CONSOUT(tint,iampl,ndim,nint,ffilt,rms,avamp,
     &	 calfac2,cdate,ctime,iscan,icur,icurmax)
	   endif
	endif
c
	ans='N'
	call DCASK('Another run (same except for random numbers)',ans,ans)
	if(UC(ans).ne.'Y') goto 999
	goto 21
999	continue
	ans='N'
	call DCASK('List some intervals and amplitudes',ans,ans)
	if(UC(ans).ne.'Y') goto 9999
	print 204
204	format('  between numbers n1,n2 = ')
	call INPUT2i(kt1,kt2)
	do i=kt1,kt2
	   print 171,i,tint(i),iampl(i),iprops(i)
	   if(discprt) write(8,171)i,tint(i),iampl(i),iprops(i)
171	   format(' Interval #',i8,8x,g13.6,2x,i5,2x,i2)
	enddo
9999	continue
c Write defaults back to disc?
	call DCASK('Write defaults to disc','Y',ans)
c Write irecq=position of rate data, back to .INI
      if(ans.eq.'Y') then
	   imodold=imod0
c for .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	   do i=1,ncyc
		if(.not.obeymr(i)) then
		   nsc(i)=-iabs(nsc(i))
		endif
	   enddo
         OPEN(unit=19,file='SCSIM.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=10240)
	   write(19,rec=1) iver,treso,tresg,concb,irecq,
     &      cjump,icalc,tpulse,tdec,trise,tjmax,repeat,ktmax,cmax1,
     &      kA,kB,kC,kD,imodold,conc0,conc1,conc2,nljump,
     &	ncyc,(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	qfilem
	   do i=1,ncyc	     !reset nsc
		nsc(i)=iabs(nsc(i))
	   enddo
	   CLOSE(unit=19)
	endif
c
	call RANDSK(ix1,iy1,iz1,1,repeat)		!write IX,IY,IZ
	call ENDPRINT
	CLOSE(unit=7)
	CLOSE(unit=8)
	call NUMCLR		!spindrift
c
	end

