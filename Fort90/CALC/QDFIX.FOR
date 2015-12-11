	program QDFIX
c to convert QDISC.DAT, QDAT2.DAT to format used by V5.n version of QGEN
c
c New version: replace QDISC with QGEN.INI, and replace multiple QDAT2.DAT
c files with a single multi-record file called QDAT.DAT.
c All three files (QGEN.INI, QDAT.DAT, QMODEL.DAT) now kept in current root
c segment (they need to be available to several different progs)
c
c In old version:
c	QDAT2 used two records, of recl=2048 (3rd record used only for 2-channel
c	case which is not in at present -make a sep file for it when needed)
c	QDISC recorded which model number had data (QDAT2) on each disk
c	 -array IMOD(i) = model number (imod0) on the ith disc (i=1-6
c 	 for disc=A-F Tosh, or i=1-12 for A: to L: on the DELL) -also recorded
c	 idisc=ndev for disc used for QDAT2 in last run.
c New version: Rather than keeping disc, keep record number in QDAT where
c	 data is: irecq=1,2,3,...,(actual 1st record # is jrecq=2*irecq-1=1,3,5,...
c	 since each data set uses 2 records). Thus IMOD(irecq)=model # (imod0)
c	 that has data located at point in QDAT.DAT defined by irecq
c	 (irecq=1,2,3,...,nrecq; nrecq=number of data sets currently defined)
c
c NB idisc=1 for A:, but ndisc= number of HARD disc partitions
	integer imod(20),jmod(20)
	real*8 QT(10,10),QD(10,10),P0(10),Pinf(10),pzero(10)
	real*8 gamma(10)	!use Peq as arg for EQOCC
	character*10 titlep(50),titlep1(50)
	character*8 titles(10)
	character*74 mtitle	!title for MODWIND models
	character*74 mtits(41)	!to read all ditto from disc
	character*2 charmod(20,30),charmod1(20,30)
	real*4 PI(10,10)
	REAL*4 PSTAR(4)		!KM2 OCCS
	integer IC(2,20)
	integer NSC(8),IM(8,15),JM(8,15),IX(10),JX(10),IL(10)
	integer NSC1(8),IM1(8,15),JM1(8,15),IX1(10),JX1(10),IL1(10)
	integer kmcon(9)
	character*1 ans,UC
	LOGICAL VJUMP,CJUMP,JUMP,epsim
	LOGICAL SCHAN,NOISE,READP,KMFAST,sameq,incond,ranrate,newmod
	logical pon,slock,debug,caplock
	character ndev*2,infil*14
	character*40 mtitle1		!for common/dpp/
	logical discprt,present
	integer IV(10),JV(10)
	real HPAR(10)
	character*11 cdate,ctime
	character*32 filnam
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
c
	pon()=slock()
	debug()=caplock()
c
	filnam='QDFIX.PRT'
	OPEN(unit=7,file='PRN',iostat=nerr)
	call WINPRINT	!print file control
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(
     & ' QDFIX: program to rewrite QDAT2 files to single QDAT.DAT',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle1
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle1
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle1
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
c
c===make sep prog MSETUP, to redefine winprint.ini?
      OPEN(unit=17,file='D:\QDISC.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	read(17,rec=1) idisc,ndev,imod
	CLOSE(unit=17)
c
c Open new data file (unit=19)
      OPEN(unit=19,file='\QDAT.DAT',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
c Now look for all the QDAT2 files on disc
	irecq=0
c
	do i=3,ndisc+2    !idisc=3 for C:
	  ndev=char(i+64)//':'        !C:, D:, . . .
	  INFIL=ndev//'\QDAT2.DAT'
	  INQUIRE(file=INFIL,exist=present,flen=nlen)
	  if(present.and.nlen.gt.0) then
	      OPEN(unit=18,file=INFIL,status='UNKNOWN',
     &	 access='DIRECT',form='UNFORMATTED',recl=2048)
	      read(18,rec=1) QD,pinf,k,ka,kb,kc,kd,nmod,imod0,mtitle,
     &       xa0,xa1,xb0,xb1,nlig,P0,cur0,gamma,kmfast,pstar,vhold,vkin,
     &       nchan,vjump,cjump,titles,ncdep,IL,IX,JX
		read(18,rec=2) QT,Pinf,nmod,imod0,vref,Vkin,pzero,pstar,
     &	 kmfast,kmcon,hpar,IV,JV,nvdep,nchan,gamma
		CLOSE(unit=18)
c Put this data into new QDAT, and define locations for new QGEN.INI
		irecq=irecq+1
		jmod(irecq)=imod0		!record position in QDAT
c Actual 1st record # is jrecq=2*irecq-1=1,3,5,...
		jrecq=2*irecq-1		!write 1st record
	      if(i.eq.idisc) then	!this is data set recorded in QDISC
		   jreclast=jrecq		!last one used -record in in QGEN.INI
		endif
		write(19,rec=jrecq) QD,pinf,k,ka,kb,kc,kd,nmod,imod0,mtitle,
     &       xa0,xa1,xb0,xb1,nlig,P0,cur0,gamma,kmfast,pstar,vhold,vkin,
     &       nchan,vjump,cjump,titles,ncdep,IL,IX,JX
		write(19,rec=jrecq+1) QT,Pinf,nmod,imod0,vref,Vkin,pzero,
     &	 pstar,kmfast,kmcon,hpar,IV,JV,nvdep,nchan,gamma
c Get the details of model from CHARQ.DAT
c D:\CHARQ.DAT renamed as \QMODEL.DAT
            OPEN(unit=14,file='\QMODEL.DAT',status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=3072)
		read(14,rec=1) nmodc,(mtits(j),j=1,nmodc)
		read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
     &	ncyc,nsc,im,jm,kA,kB,kC,kD,titlep,ncon
      	CLOSE(unit=14)
		npar=2*ncon
		mtitle=mtits(imod0)
		print 181,char(i+64)//':',imod0,mtitle(1:72)
		if(discprt) write(8,181)char(i+64)//':',imod0,mtitle(1:72)
181		format(1x,a2,' Model # = ',i2,/,1x,a72)
c
		print 182,jrecq,jrecq+1
		if(discprt) write(8,182) jrecq,jrecq+1
182		format(' Data for this model now written to records ',i5,
     &	' and ',i5,' of QDAT.DAT',/,
     &	' CHARQ.DAT copied to QMODEL.DAT')
c
	   endif	!if(exist...
	enddo		!end of ndisc loop
c
	CLOSE(unit=19)    !close new QDAT.DAT
	nrecq=irecq		!number of data sets recorded
c
c
	do i=1,20
         print 30, i,jmod(i)
         if(discprt) write(8,30) i,jmod(i)
30	   format(' i, jmod(i) = ',2i8)
	enddo
      print 31,jreclast,nrecq
      if(discprt) write(8,31) jreclast,nrecq
31	format(' jreclast,nrecq = ',2i8)
c Keep new values in QGEN.INI
      OPEN(unit=16,file='\QGEN.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	write(16,rec=1) nrecq,jreclast,jmod
	CLOSE(unit=16)
c
	call ENDPRINT
	END


