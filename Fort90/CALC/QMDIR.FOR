	program QMDIR
c To inspect records in QMECHS.DAT
c
	real*8 QT(100,100),Pinf(100)
	real*8 dgamma(100)	!use Peq as arg for EQOCC
	real*8 Qdum(100,100)
	character*10 titlep(200)
	character*8 titles(100)
	character*74 mtitle	!title for MODWIND models
	character*74 mtits(100)	!to read all ditto from disc
c	character*74 rtitle(40)	!title to describe rate constants
	character*74 rtitles(500)	!title to describe rate constants
	character*74 rtitle	!title of current rate constants
	character*2 charmod(25,40)
	character qfilem*40
c=	real*4 PI(100,100)
	integer SCRNBUF0(1024)
c	integer IQ(10,10),jmod(40)
	integer IQ(100,100),IQQ(100,100),jmod(500)
	integer irate(200),jrate(200)
c=	integer irateq(200),jrateq(200)
C QM IS TO HOLD MINORS OF Q IN EQOCC. SEEMS THAT IT MUST BE
C DECLARED IN MAIN PROG FOR CORRECT CALCS (SEE COMMENTS IN EQOCC)
C **Do not ned this when DETD or DETV rather than DETERM used to
c calc determinants in EQOCC
	real*4 conc(10)
	integer jstart(500)
	integer jstart1(500)
	character*20 ligname(10)
	character*10 statname(100)
c=	character*11 cnum0,cnum1,cnum2
	character*11 cnum0
	logical chardef,boundef
c
	REAL*4 PSTAR(4)		!KM2 OCCS
	integer IC(2,200)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100),IL(100)
	integer kmcon(9)
	character*1 ans,UC
	LOGICAL READP,KMFAST,newmod,altmod,blank,sameq
c	logical ranrate
	logical pon,slock,debug,caplock
	logical discprt
	logical indmod
	integer nbound(100,10)
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
c
	print 10
10	format(
     & ' QMDIR: prints the contents of a mechanisms file, QMECHS.DAT',/,
     & ' Output file is QMDIR.PRT',/)
c
	qfilem='QMECHS.DAT'
	call TITENT0('Name/path for models file:',qfilem,40,.false.)
c
      OPEN(unit=8,file='qmdir.prt',status='UNKNOWN',
     & access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	REWIND(unit=8)
c
      OPEN(unit=16,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
	read(unit=16,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart

	do irq=1,nrecs
	   irec=jstart(irq)	!previous nextrec
c	  Read data record
	   read(unit=16,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,kstat1,
     &	((charmod(i,j),i=1,ilast),j=1,jlast),
     &	(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),
     &	(QT(irate(i),jrate(i)),i=1,nrateq),
     &	(titlep(i),i=1,npar),
     &      (ligname(i),i=1,nlig),
     &	((nbound(i,j),i=1,k),j=1,nlig),
     &	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),
     &	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),
     &	(hpar(i),i=1,nvdep),
     &	(pstar(i),i=1,4),(kmcon(i),i=1,9),
     &	(ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),
     &	(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),
     &	(efacq(i),i=1,nsetq),
     &	(statname(i),i=1,kstat1),
     & 	nsub,kstat0,npar0,kcon,npar1,ncyc0
c
	   print 1,irq,imod,jstart(irq),rtitle,
     &	nlig,(ligname(i),i=1,nlig)
	   write(8,1) irq,imod,jstart(irq),rtitle,
     &	nlig,(ligname(i),i=1,nlig)
1	   format(/,
     &  ' Rec #',i3,': model ',i3,' start rec = ',i11,/,
     &   1x,a74,/,
     &   ' no of ligands = ',i2,/,2x,10a20,/,/)
	enddo
c
	CLOSE(unit=16)
	CLOSE(unit=8)
	end
