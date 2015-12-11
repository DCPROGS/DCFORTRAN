	program CONVMECH
c Utility to convert QMechs.dat files so the mtitle is present
c in every record, and arrays mtits() and nmods no longer needed
c (these are incorrect in most qmechs files anyway -nmods is greater
c that the number of mechanisms actually rperesented in the file).

	real*8 QT(100,100)
	REAL*8 dgamma(100)
	character qfilem*40,qfile2*40
	character*2 charmod(25,40)	!to print model
	integer nbound(100,10)
	character*10 titlep(200)
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100)
	character*74 rtitle	!title to describe rate constants
	integer irate(200),jrate(200)
	logical boundef,chardef,indmod
	real*4 pstar(4)
	integer kmcon(9)
c
	character*74 mtits(100)
	character*74 mtitle
c
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	COMMON/LIG/nlig,IL(100)

	qfilem='QMECHS.DAT'
	call TITENT0('Name/path for input file:',qfilem,40,.false.)
	qfile2='QMNEW.DAT'
	call TITENT0('Name/path for input file:',qfile2,40,.false.)
c
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
      OPEN(unit=16,file=qfile2,status='UNKNOWN',
     &	access='TRANSPARENT')
c
c       Header part (re-written at each loop)
	read(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
c	  Data record
	do ir=1,nrecs
	   irec=jstart(ir)
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
c Now rewrite each record in iver=102 format, with mtitle in the record
c Write back to same record -only 74 bytes added and there is separation
c of 1024 bytes between records so should be no problem
	   if(iver.eq.101) then
		iver=102
		mtitle=mtits(imod)
		write(unit=16,rec=irec) iver,
     &	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,
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
	   endif
	enddo
	CLOSE(unit=15)
c Write header for output -no need for nmods or ntits now
	write(unit=16,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart
	CLOSE(unit=16)

	end

