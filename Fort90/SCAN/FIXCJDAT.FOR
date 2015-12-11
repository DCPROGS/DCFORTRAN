	program FIXCJDAT
c To read selected jumps for one CJUMP.DAT file and transfer them to another
	character*30 adcfil1,adcfil2
      character cdate*11,ctime*11,ndev*2
	character title1*79
	logical vjump,sampv,control
c	logical discprt,pon,slock,debug,caplock
	integer*2 iver1,iver2
	integer*4 ikeep1(4,2)
	integer*2 ikeep12(4,2)
	integer*2 kstep(5)
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhclamp,ipatch
	integer*2 istrec1(250),istrec2(250)
	ALLOCATABLE::iADC,iadc1,avcur,avolt
	integer*2 iADC(:),iADC1(:)
	real*4 avcur(:),avolt(:)
c
	print 251
251	format(' Specify disk partition for input CJUMP data file:')
	call DISCNUM1(idisc,ndev,-1,1)
	if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	adcfil1=ndev//'\CJUMP.DAT'
	call TITENT0('Give name/path for input CJUMP data file:',adcfil1
     & ,30,.false.)
c
	print 2
2	format(' Specify disk partition for OUTPUT CJUMP data file:')
	call DISCNUM1(idisc,ndev,-1,1)
	if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	adcfil2=ndev//'\CJUMP.DAT'
	call TITENT0('Give name/path for input CJUMP data file:',adcfil2
     & ,30,.false.)

c
      OPEN(unit=14,file=adcfil1,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
	read(14,rec=1) njd,nxtrec,(ISTREC1(i),i=1,250)
c
	print 622,njd,adcfil1
622	format(' ',i4,' sweeps on disc in ',a30)
c
      OPEN(unit=15,file=adcfil2,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
c
	print 60
60	format(' Transfer sweeps n1 to n2 to output file:  n1 (,n2) = ')
	call INPUT2i(n1,n2)
c
	njd1=n2-n1+1	!new number of jumps
c new istrec array -istrec1(n1) becomes 2
	j=0
	do i=n1,n2
	   j=j+1
	   istrec2(j)=istrec1(i)-istrec1(n1)+2
	enddo
	next1=nxtrec-istrec1(n1)+2
	write(15,rec=1) njd1,next1,(ISTREC2(i),i=1,250)
c
	irec=int4(istrec1(n1))
c prelim read to get iver1
	   read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &   vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &   ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &   ncjump,ilen2c,igap2c,nDv,iDd,iver1
c
	do 64 i1=n1,n2
261	   irec=int4(istrec1(i1))
	  if(iver1.ge.-1003) then	!CJUMP3 or earlier
	    read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep12,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt
	  else
	    read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt
	  endif
c Allocate iadc/avcur
	if(allocated(iADC)) DEALLOCATE(iADC,avcur)
	ALLOCATE(iADC(nsamp),avcur(nsamp))
c read old data
	nrec=1 + (nsamp-1)/256		!for iADC(i)  =int*2 so 1 rec=256 values
	lastn=nsamp - (nrec-1)*256	!number to read from last record
	if(iav.ne.0) nrec=2*nrec	!for AVCUR(i) =real*4
	if(iav.eq.0) then
	   n=1
	   do j=1,nrec
		m=n+255
		if(j.eq.nrec) m=n+lastn-1
		irec=irec+1
		read(14,rec=irec) (iADC(i),i=n,m)
		n=n+256
	   enddo
	else
	   n=1
	   do j=1,nrec
		m=n+127
		if(j.eq.nrec) m=n+lastn-1
		irec=irec+1
		read(14,rec=irec) (AVCUR(i),i=n,m)
		n=n+128
	   enddo
	endif
c If voltage sampled, read it too
	if(.not.sampv) goto 91
	if(allocated(iADC1)) DEALLOCATE(iADC1,avolt)
	ALLOCATE(iADC1(nsamp),avolt(nsamp))
	if(iav.eq.0) then
	   n=1
	   do 503 j=1,nrec
	   m=n+255
	   if(j.eq.nrec) m=n+lastn-1
	   irec=irec+1
	   read(14,rec=irec) (iADC1(i),i=n,m)
	   n=n+256
503	   continue
	else
	   n=1
	   do 504 j=1,nrec
	   m=n+127
	   if(j.eq.nrec) m=n+lastn-1
	   irec=irec+1
	   read(14,rec=irec) (AVOLT(i),i=n,m)
	   n=n+128
504	   continue
	endif
91	continue
c
c
c Now write data back to 15
	  irec=int4(istrec2(i1-n1+1))	!=1,2,....
	  if(iver1.ge.-1003) then	!CJUMP3 or earlier
	    write(15,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep12,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt
	  else
	    write(15,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver2,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt
	  endif
c and write data
	nrec=1 + (nsamp-1)/256		!for iADC(i)  =int*2 so 1 rec=256 values
	lastn=nsamp - (nrec-1)*256	!number to read from last record
	if(iav.ne.0) nrec=2*nrec	!for AVCUR(i) =real*4
	if(iav.eq.0) then
	   n=1
	   do j=1,nrec
		m=n+255
		if(j.eq.nrec) m=n+lastn-1
		irec=irec+1
		write(15,rec=irec) (iADC(i),i=n,m)
		n=n+256
	   enddo
	else
	   n=1
	   do j=1,nrec
		m=n+127
		if(j.eq.nrec) m=n+lastn-1
		irec=irec+1
		write(15,rec=irec) (AVCUR(i),i=n,m)
		n=n+128
	   enddo
	endif
c If voltage sampled, read it too
	if(.not.sampv) goto 92
	if(iav.eq.0) then
	   n=1
	   do j=1,nrec
	      m=n+255
	      if(j.eq.nrec) m=n+lastn-1
	      irec=irec+1
	      write(15,rec=irec) (iADC1(i),i=n,m)
	      n=n+256
	   enddo
	else
	   n=1
	   do j=1,nrec
		m=n+127
		if(j.eq.nrec) m=n+lastn-1
		irec=irec+1
		write(15,rec=irec) (AVOLT(i),i=n,m)
		n=n+128
	   enddo
	endif
92	continue
c
64	continue	!end of n1,n2 loop
c
	close(unit=14)
	close(unit=15)
c
999	continue
	end
