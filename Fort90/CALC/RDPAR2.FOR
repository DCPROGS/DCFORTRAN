	subroutine RDPAR2(readp,nmod,theta,nchan,gamma,ndev,idest)
c To read parameters in new QDEF1- reads QT and theta only- not QT now
	real*8 theta(50)
	real*8 gamma(10)
	character*1 ans,UC
	logical readp
	character ndev*2,infil*14
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
c
101	format(a1)
	idest=0
	print 301
301	FORMAT( ' Read parameters from disc? ')
	input 101,ans
	READP=.FALSE.
	if(UC(ans).EQ.'Y') READP=.TRUE.
	IF(.NOT.READP) GOTO 302
C Read Q etc from disc	(default for read/write defined above as always
c write but may not always read)
	call discnum(idisc,ndev,0,1)
	INFIL=ndev//'QMOD.DAT'
C READ RATES ETC FROM DISK
C Read rates from disc. In this version (unlike QMOD) read theta
c from record 4 only. Connections etc are defined for each model above
c so no need to read them from disc
      OPEN(unit=18,file=INFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=1024)
c NB recl is in bytes (4*value for PDP)
	read(18,rec=1) nmod1,theta,nchan,gamma,nvdep,iv,jv,hpar
	CLOSE(unit=18)
	if(nmod1.eq.nmod) goto 24
	if(nmod1.eq.31.and.nmod.eq.3) goto 24	!OK -both have same param
	if(nmod1.eq.3.and.nmod.eq.31) goto 24	!OK -both have same param
	call bell(3)
	print 25,nmod1
25	format(' ***Data on disc is for WRONG model ( for #',i4,')')
	goto 23
24	continue
	RETURN
302	idest=1
	RETURN
23	idest=2
	RETURN
	end

