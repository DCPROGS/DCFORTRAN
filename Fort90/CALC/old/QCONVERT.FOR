	subroutine QCONVERT(qfilem)
c To find and read the old rate and mechanism files, qmodel.dat and qdat.dat,
c and convert them to a single QMECHS.DAT file that can be read with GETQD
c
c______________________________________________________________
c New QMECHS.DAT format for storing models and rate constants
c_______________________________________________________________
c Note how 2D array must be written/read if one endex depends on the other
c (see t2Dwrite.for in \ekdist-new) write(19,1) ((jmat(i,j),1=1,n(j)),j=1,n)
c
c (1) Model and rate constants are kept in the same file

c (2) for number of states> 20??? the character model is not kept, but
c	when it is kept, it is repeated for each set of rates for that
c	model, rather than being in a separate file

c (3) QMECHS.dat is transparent file, which starts with index. This cannot
c	be expanded so make it big enough for say 100 models/500 rate constant
c	data sets

c (4) QMECHS.DAT starts with 'header', written starting at record 1
c	iver = version number =101 initially
c	nrecs = number of records of model/rates stored in the file
c	nextrec=next vacant record (byte) # = jstart() value for next record
c		(need to count everything written to calculate this each time)
c	jreclast=number of record used in last run of any theory prog
c	  (as previously in qgen.ini) -may be overwritten by value from prog.ini)
c	nmods=number of different models (.le. nrecs)
c	character*74 mtits(100)	!mtits(i)=title for model #1 (whole array written
c	   =14800 bytes)
c	jstart(500)=start byte # for storage of the ith record (2000 bytes)
c	   whole array written)
c	   NB in old qdat.dat, jmod() indicated the model used in each record
c	   which must mow be found by reading each record.  There was no need
c	   for jstart() in old qdat.dat becuase record lengths were fixed
c  Total of these = 4+4+4+4+14800+2000 = 16816 bytes
c  Start writing the records at byte # = 20000 (leaves gap for expamsion of header)
c  so jstart(1)=20000
c
c (5) Record number i, which starts at byte # = jstart(i) contains the following
c     In general put the scalars at start, arrays later
c											bytes
c	iver =version number again						4
c 	imod = model number for this record					4
c	k, kA, kB, kC, kD								20
c	rtitle*74=title for rates (need to read all into array)	74
c	ilast, jlast=row and col number for charmod (=1 if charmod
c		does not exist, e.g. for 3D models)				8
c	nrconst=number of rate constants=2*ncon				4
c===		nrconst called npar below
c	ncon                                                        4
c	ncdep										4
c	nlig										4
c     chardef (T if charmod define)						4
c	boundef (T if nbound defined)                               4
c	ncyc										4
c	vref=potential at which rate constants are specfied		4
c	nvdep                                                       4
c	kmfast									4
c    now arrays                                                   4
c	((charmod(i,j),i=1,ilast),j=1,jlast)				4*ilast*jlast
c	(irate(i),i=1,nrconst)							4*nrconst
c	(jrate(i),i=1,nrconst)							4*nrconst
c	(QT(irate(i),jrate(i)),i=1,nrconst)					8*nrconst
c	(titlep(i),i=1,nrconst)							10*nrconst
c     (ligname(i),i=1,nlig)							20*nlig
c	((nbound(i,j),i=1,k),j=1,nlig)					4*k*nlig
c	(IX(i),i=1,ncdep)								4*ncdep
c	(JX(i),i=1,ncdep)								4*ncdep
c	(IL(i),i=1,ncdep)								4*ncdep
c	(dgamma(i),i=1,kA)							8*kA
c	(nsc(i),i=1,nyc)								4*ncyc
c	((im(i,j),j=1,nsc(i)),i=1,ncyc)					4*ncyc*SUM[nsc(i)]
c	((jm(i,j),j=1,nsc(i)),i=1,ncyc)					4*ncyc*SUM[nsc(i)]
c	(iv(i),i=1,nvdep)								4*nvdep
c	(jv(i),i=1,nvdep)                                           4*nvdep
c	(hpar(i),i=1,nvdep)                                         4*nvdep
c	(pstar(i),i=1,4)                                            4*4=16
c	(kmcon(i),i=1,9)								4*9=36
c Other things (imod0, conc, nchan etc) all go in .ini for each program
c
	logical present
	character qfile*40,qfile1*40,qfile2*40,qfilem*40
	character qpath*30,qname*8,suffix*3,ndev*2

c Declarations for old qmodel/qdat (add 0 to name if not same as for new)
c No need for new arrays, since the only bits that are written to qmechs.dat
c are those actually defined. Need new arrays only for those for which
c whole array is written in header bit, mtits(100) and jstart(500)
	REAL*8 QD(10,10),Pinf(10),P0(10),dgamma(10)
	character*2 charmod(20,30)	!to print model
	real*4 pstar(4)
	character*8 titles(10)
	character*10 titlep(50)
	real*8 QT(10,10)
	real*8 pzero(10)		!not used
	integer kmcon(9),IV(10),JV(10),IX(10),JX(10)
	integer NSC(8),IM(8,15),JM(8,15),IL(10)
	real*4 HPAR(10)
	character*74 rtitles(500)	!title to describe rate constants
c
	logical vjump,cjump,kmfast
	character*74 mtitle	!title for model
c	character*74 mtits(41)	!to read all ditto from disc
	character*74 mtits(100)	!to read all ditto from disc
c	integer jmod(40)
	integer jmod(100)
c	LOGICAL slock
c	logical debug,caplock
	logical discprt
c Addition for getbound
	integer nbound(10,2)
	common/nbnd/nbound
c Declarations for new qmechs.dat
	allocatable::irate,jrate,jstart
	integer irate(:),jrate(:),jstart(:)
	character*74 rtitle		!title to describe rate constants
	logical boundef,chardef
	character*20 ligname(10)
	character*11 cnum
	real*4 conc(10)
c
	common/dp/discprt
c
c Allocate arrays for writing the new qmechs.dat
	ALLOCATE(irate(200),jrate(200),jstart(500))
c Locate files
	qfile='QMODEL.DAT'
22	INQUIRE(file=qfile,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   qfile='\QMODEL.DAT'
	   INQUIRE(file=qfile,exist=present,flen=nlen)
	   if(.not.present.or.nlen.eq.0) then
		print 21
21	   	format(
     & ' File QMODEL.DAT not found in local directory or root')
	      call TITENT0('Path for QMODEL.DAT:',qfile,40,.false.)
		goto 22
	   endif
	endif
	call PARSNAME(qfile,qpath,ndev,qname,suffix,nopath,40)
	qfile1=charnb(qpath)//'QGEN.INI'
23	INQUIRE(file=qfile1,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   print 210,qfile1
210	   format(' QGEN.INI not found in: ',a40)
	   call TITENT0('Path for QGEN.INI:',qfile1,40,.false.)
	   goto 23
	endif
	qfile2=charnb(qpath)//'QDAT.DAT'
24	INQUIRE(file=qfile2,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   print 211,qfile2
211	   format(' QDAT.DAT not found in: ',a40)
	   call TITENT0('Path for QDAT.DAT:',qfile2,40,.false.)
	   goto 24
	else
	   nlendat=nlen
	endif
c Open qgen.ini
      OPEN(unit=17,file=qfile1,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	read(17,rec=1) nrecq,jreclast,jmod
	CLOSE(unit=17)
c If the wrong qgen.ini is present, the number of records in qgen.dat, which
c is recorded ONLY in qgen.ini (!!), may be wrong, and if it is too big
c will cause 'read beyond end of record' error below, so check here
	nr1=nlendat/4096		!should be exact number
	if(nrecq.gt.nr1) then
	   call BELL(1)
	   print 7,nrecq,nr1
7	   format(
     & ' According to QGEN.INI the number of files in QDAT.DAT',/,
     & ' should be ',i5,' but only ',i5,' found')
	   nrecq=nr1
	   pause
	endif
c Read all titles for sets of rate constants initially here (otherwise
c reading rtitle() involves reading also QT, nchan etc so any values of these
c set here may be overwritten
c Array nbound(100) added to QDAT, and ibflag as indicator of whether
c valid values for nbound have been put into qdat.dat yet -set ibflag=1001
c if nbound() has been defined
c Now change ibflag to boundef=true if nbound is defined
      OPEN(unit=18,file=qfile2,status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
	do i=1,nrecq
	   jrecq=2*i	!read SECOND record of data
	   read(18,rec=jrecq) QT,Pinf,nmod,imod1,vref,Vkin,PZERO,PSTAR,
     &      KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,dgamma,rtitles(i),
     &	ibflag,nbound
	enddo
	CLOSE(unit=18)
c   but reset ibflag to 0 here (interested in value ONLY for the rates actually used)
	ibflag=0
c In F90/Gino get problems with crazy undefined values in kmcon, pstar,
c when kmfast=false (it reads as false under debugger, but program does
c not work as though it was false!!).  Put in catch here
	if((pstar(1).gt.1.).or.(pstar(1).lt.0.)) then
	   kmfast=.false.
	   do i=1,4
		pstar(i)=1.
	   enddo
	   do i=1,9
		kmcon(i)=2
	   enddo
	endif
c
c If get to here then old files all found, so now read them and convert
c to the new format qmechs.dat.  Do loop for each of the rate records in
c qdat.dat, and for each read relevant qmodel.dat record to get what is
c needed to write qmechs.dat
c
c Open qgen.ini
c      OPEN(unit=17,file=qfile1,status='UNKNOWN',
c Open qdat.dat
      OPEN(unit=18,file=qfile2,status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=2048)
c Open qmodel.dat
      OPEN(unit=14,file=qfile,status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=3072)
c
c
c Start loop for each record in qdat.dat
c
	nextrec=20000	!byte # to start writing first data rec in qmechs.dat
c
	do irq=1,nrecq
c
c    Read QDAT
	  read(18,rec=irq) QD,Pinf,K,ka,kb,kc,kd,nmod,imod0,mtitle,
     &   xa,xa,xb,xb,nlig,P0,c0,dgamma0,kmfast,pstar,vhold,vkin,
     &   nchan,vjump,cjump,titles,ncdep,IL,IX,JX
	  read(18,rec=irq+1) QT,Pinf,nmod,imod,vref,Vkin,PZERO,PSTAR,
     &   KMFAST,KMCON,HPAR,IV,JV,NVDEP,nchan,dgamma,rtitles(irq),
     &   ibflag,nbound
	  conc(1)=xA
	  conc(2)=xB
c only QT0 needed (?)
c Now read model stuff for model imod0 from qmodel.dat
         OPEN(unit=14,file=qfile,status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=3072)
	   read(14,rec=1) nmodc
	   if(nmodc.gt.41) then	!look for qtitles.dat
      	OPEN(unit=16,file='QTITLE.DAT',status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=10240)
		read(16,rec=1) nmodc,(mtits(i),i=1,nmodc)
		CLOSE(unit=16)
	   else
		read(14,rec=1) nmodc,(mtits(i),i=1,nmodc)
	   endif
	   read(14,rec=imod0+1) charmod,ilast,jlast,ncdep,ix,jx,il,
     &    ncyc,nsc,im,jm,kA,kB,kC,kD,titlep,ncon
	   close(unit=14)
c All in now so construct everything for qmechs.dat
	   iver=101
	   nrecs=nrecq		!# of sets of rate constants
c	   nextrec=nextrec
	   nmods=nmodc		!# of different models
c	   mtits(100) 		!unchanged
c      end of 'header' section -now data
	   rtitle=rtitles(irq)
	   chardef=.true.		!all old models were defined in charmod form
	   boundef=iblag.eq.1001	!true if nbound has been defined
	   jstart(irq)=nextrec	!start byte # for this record
	   if(nlig.gt.0) then
		do i=1,nlig
		  call INTCONV(i,cnum)
	        call TITENT0('Give name for ligand # '//charnb(cnum)//':',
     &	  ligname(i),20,.true.)
		enddo
	   endif
c now make irate(), jrate() from QT0. This is done via IC() and IQ() in getqd,
c but IC() comes from charQ which is not used here -simpler to look for
c zero values (off-diagonal)
	   m=0
	   do i=1,k
		do j=1,k
		   if(i.ne.j) then
			if(QT(i,j).gt.1.d-30) then
			   m=m+1
			   irate(m)=i
			   jrate(m)=m
			endif
		   endif
		enddo
	   enddo
	   if(m.ne.2*ncon) then
		call BELL(2)
		print 1,m,2*ncon
1		format(' ERROR in QCONVERT: m, 2*ncon = ',2i7)
		pause
	   endif
	   nrconst=m	!numberof rate constants in model=2*ncon
c Add up number of bytes written and update nextrec so value in qmechs.dat
c is where write should start next time a new model/rates is defined
c 	   Calc isum=SUM[nsc(i)]
	   isum=0
	   do i=1,ncyc
		isum=isum + nsc(i)
	   enddo
c==check by writing single record
	   nbytes=4+4+20+74+8+40+4*ilast*jlast+4*nrconst+4*nrconst+
     &    8*nrconst+10*nrconst++20*nlig+4*k*nlig+4*ncdep+4*ncdep+
     &    4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+
     &    4*nvdep+4*nvdep+4*nvdep+16+36
c Update start record for next write
c
	   qfilem='QMECHS.DAT'
         OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &	access='TRANSPARENT')
c       Write header part (re-written at each loop)
	   write(unit=15,rec=1) iver,
     &	nrecs,nextrec,jreclast,nmods,mtits,jstart
c	  Write data record
	   nextrec=nextrec + nbytes + 100 	!leave gap of 100 bytes
	   irec=jstart(irq)	!previous nextrec
	   write(unit=15,rec=irec) iver,
     &	imod,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrconst,
     &	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,
     &	((charmod(i,j),i=1,ilast),j=1,jlast),
     &	(irate(i),i=1,nrconst),(jrate(i),i=1,nrconst),
     &	(QT(irate(i),jrate(i)),i=1,nrconst),
     &	(titlep(i),i=1,nrconst),
     &      (ligname(i),i=1,nlig),
     &	((nbound(i,j),i=1,k),j=1,nlig),
     &	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),
     &	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),
     &	(hpar(i),i=1,nvdep),
     &	(pstar(i),i=1,4),(kmcon(i),i=1,9)
c
	   CLOSE(unit=15)
c
c
	enddo		!end of loop for all sets of rate constants
c Print what was done
	print 30,nrecq,nmodc,nextrec
	if(discprt) write(8,30)nrecq,nmodc,nextrec
30	format(/,
     & ' The old files that defined models and rates (qgen.ini,',/,
     & ' qmodel.dat and qdat.dat) have been converted into a single',/,
     & ' new file QMECHS.DAT, which contains definitions of',i4,/,
     & ' models and ',i4,' sets of rate constants.',/,
     & ' Next vacant record for write to QMECHS.DAT = byte ',i11)
	pause
c
	DEALLOCATE(irate,jrate,jstart)
c
	RETURN
	end


