	subroutine CONVQM(qfilem)
c To convert QMechs.dat files so the mtitle is present
c in every record, and arrays mtits() and nmods no longer needed
c (these are incorrect in most qmechs files anyway -nmods is greater
c that the number of mechanisms actually rperesented in the file).
	real*8 QT(100,100)
	REAL*8 dgamma(100)
	character qfilem*40,qfile2*40,qfile3*40
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
	character qpath*30,qname*8,suffix*3,ndev*2
c
	character*74 mtits(100)
	character*74 mtitle
c
	integer NVDEP,IV(100),JV(100)
	real*4 HPAR(100)
	integer NCDEP,IX(100),JX(100)
	integer NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer nsetq,ieq(200),jeq(200),ifq(200),jfq(200)
	real*4 efacq(200)
	integer nlig,IL(100)
c
	character*1 ans
	logical discprt,nopath
	character*100 command
	common/dp/discprt
c
c	qfilem='QMECHS.DAT'

      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='Direct',form='BINARY',RECL=1)
	read(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
	if(iver.ne.101) then
	  STOP
	endif
	
	qfile2='QMNEW.DAT'
      OPEN(unit=16,file=qfile2,status='UNKNOWN',
     &	access='Direct',form='BINARY',RECL=1)
c
c       Header part (re-written at each loop)
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
c
	CLOSE(unit=15)
c Write header for output -no need for nmods or ntits now
	write(unit=16,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart
	CLOSE(unit=16)
	nf=len_trim(qfilem)
	np=len_trim(qpath)
	nn=len_trim(qname)
	call PARSNAME(qfilem,qpath,ndev,qname,suffix,nopath,40)
	qfile3=qpath(1:np)//qname(1:nn)//'.BAK'
	n2=len_trim(qfile2)
	n3=len_trim(qfile3)
	command='copy /B /V /Y '//qfilem(1:nf)//' '//qfile3(1:n3)
	call SYSTEM(command)
	command='copy /B /V /Y '//qfile2(1:n2)//' '//qfilem(1:nf)
	call SYSTEM(command)
	
	  
999	continue
	RETURN
	end

