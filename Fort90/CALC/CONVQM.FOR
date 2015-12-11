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
	logical discprt
	character*100 command
	common/dp/discprt
c
c	qfilem='QMECHS.mec'
c	call TITENT0('Name/path for input file:',qfilem,40,.false.)
      OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
	read(unit=15,rec=1) iver,
     &	nrecs,nextrec,ireclast,nmods,mtits,jstart
	if(iver.ne.101) then
	   print 105,iver
105	   format(' ERROR in QCONVERT: iver = ',i4,' (should be 101)')
	   STOP
	endif
	print 101,qfilem
101	format(' The mechanisms file, ',a40,',',/,
     &  ' is the old version, v101: convert to v102 now [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'N') then
	   close(unit=15)
	   goto 999
	endif
	qfile2='QMNEW.DAT'
c	call TITENT0('Name/path for input file:',qfile2,40,.false.)
c
      OPEN(unit=16,file=qfile2,status='UNKNOWN',
     &	access='TRANSPARENT')
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
	   nsub=0
	   kstat0=0
	   npar0=0
	   kcon=0
	   npar1=0
	   ncyc0=0

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
     &	(statname(i),i=1,kstat),
     & 	nsub,kstat0,npar0,kcon,npar1,ncyc0
	   endif
	enddo
c
	CLOSE(unit=15)
c Write header for output -no need for nmods or ntits now
	write(unit=16,rec=1) iver,
     &	nrecs,nextrec,ireclast,jstart
	CLOSE(unit=16)
	call PARSNAME(qfilem,qpath,ndev,qname,suffix,nopath,40)
	qfile3=charnb(qpath)//charnb(qname)//'.BAK'
	print 102,nrecs,qfilem,qfile2,qfile3,qfilem
102	format(' The ',i3,' records in ',a40,/,
     & ' have been converted to v102 format in ',a40,/,/,
     & ' Rename old file as ',a40,/,
     & ' and rename new file as ',a40,/,
     & ' O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'Y') then
c=	   call RENAME(qfilem,qfile3)
c  switches for 'copy' /B=binary, /V=verify, /Y =ovewrite without asking
	   command='copy /B /V /Y '//charnb(qfilem)//' '//charnb(qfile3)
	   call SYSTEM(command)
c=	   call RENAME(qfile2,qfilem)
	   command='copy /B /V /Y '//charnb(qfile2)//' '//charnb(qfilem)
	   call SYSTEM(command)
	   print 103,nrecs,qfilem,qfile3
	   if(discprt) write(8,103) nrecs,qfilem,qfile3
103	   format(' The ',i3,' records in ',a40,/,
     & ' have been converted to v102 format.',/,
     & ' The original (v101) file has been renamed as ',a40,/)
	else
	   print 104,nrecs,qfilem,qfile2
	   if(discprt) write(8,104) nrecs,qfilem,qfile2
104	   format(' The ',i3,' records in ',a40,/,
     & ' have been converted to v102 format in ',a40,/)
	endif
c
999	continue
	RETURN
	end

