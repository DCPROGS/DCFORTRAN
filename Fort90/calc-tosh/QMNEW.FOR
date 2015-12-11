	program QMNEW
c To make new DC's qmechs.dat file with elimination of all models for which
c no rates were stored -can no longer access a model if it does not
c appear in one of the records
c NB the number of records in qmechs.dat is the same, it is only nmods
c and mtits() that differ

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
      OPEN(unit=8,file='qmnew.prt',status='UNKNOWN',
     & access='APPEND',form='FORMATTED',carriage control='FORTRAN')
	REWIND(unit=8)
c
      OPEN(unit=15,file='qmechs.dat',status='UNKNOWN',
     &	access='TRANSPARENT')
      OPEN(unit=16,file='qmnew.dat',status='UNKNOWN',
     &	access='TRANSPARENT')
c       Write header part (re-written at each loop)

	read(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
=====
	do irq=1,nrecs
	   irec=jstart(irq)	!previous nextrec
	   read(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle
	   jmod(irq)=imod
	   rtitles(irq)=rtitle
	enddo
=======
c Find the new number of models, i.e. the number of models that appear
c in jmod()
	nm=0
	do im=1,nmods
	   do irq=1,nrecs
		if(jmod(irq).eq.im) then	!this model is present
		   do jm=1,nm-1			!is model #im already in mtits1?
			if
		   nm=nm+1
		   mtits1(nm)=mtits(im)
===snag -model numbering will change!
c
c For output record, first record starts at same place as input so
	nextrec=jstart(1)		!initially
c NOW LOOP, skipping any model that does not occur in jmod()
=========so have new nmods and nrecs values
	nmods1=0
	do irq=1,nrecs
	   irec=jstart(irq)	!previous nextrec
	   read(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,kstat,
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
     &	(statname(i),i=1,kstat)
c
c Check whether imod occurs anywhere in jmod() -if not, skip it
	   skip= true.
	   do ir=1,nrecs
		if(jmod(irq).eq.imod) skip=.false.	!this model is present
	   endif
	   if(skip) goto 9
===
	   nmods1=nmods1+1
	   mtits
	   isum=0
	   do i=1,ncyc
		isum=isum + nsc(i)
	   enddo
	   nbytes=4+4+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+
     &    8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &    4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &    4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat
c
	   irec1=nextrec		!where to write the record
	   ireclas1=irec1
	   jstart1(irq)=irec1
	   nextrec=nextrec + nbytes + 256 	!leave gap of 256 bytes
c
	   write(unit=16,rec=1) iver,
     &	nrecs,nextrec,ireclas1,nmods,mtits,jstart1
c	  Write data record
	   write(unit=16,rec=irec1) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	indmod,npar,nsetq,kstat,
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
     &	(statname(i),i=1,kstat)
c
	   print 1,irq,imod,jstart(irq),jstart1(irq)
	   write(8,1) irq,imod,jstart(irq),jstart1(irq)
1	   format(
     &  ' Rec #',i3,': model ',i3,' old start = ',i11,' new = ',i11)
c
9	   continue
	enddo
c
	CLOSE(unit=15)
	CLOSE(unit=16)
	CLOSE(unit=8)
	end

