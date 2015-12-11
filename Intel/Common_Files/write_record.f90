 subroutine write_record(iplot,ifiltype,nset,nsfit,&
			nrows,niobs,njobs,stitle,hdisp,xobs,yobs,w,ndatset,&
			dfile,titlef,titlex,titley,iw,setx,setvar,ilabel,iunit,fopen_11,fopen_12)

    logical fopen_11,fopen_12
	ALLOCATABLE datcop	
	real datcop(:)
	real*4 xobs(niobs,nset),yobs(niobs,nset),w(niobs,nset)
	integer*4 nj(nset),setx(nset),ndatset(200,20)
	character*60 :: dfile
	
	character*64 title1
	character*60 stitle(10,200)
    CHARACTER*60 TITLEF,TITLED(20)
    character*40 titlex,titley

    INTEGER*4 jstrec(100),lstrec(100),jstrec1(100),lstrec1(100),nsets(20)
	integer*2 ititw(5)		!for old queue
	CHARACTER*22 CNUM(10),CNUM1
	logical hdisp

	


	kwi=niobs   !dimensions of weight()
	kwj=nset
	ncurvc=0		!none here
		if(iunit.eq.12) then
		if(fopen_12) close(unit=12)
		
    
	else if(iunit.eq.11) then
		if(fopen_11) close(unit=11)
		
    endif
	OPEN(unit=iunit,file=dFILe,status='UNKNOWN',access='DIRECT', form='BINARY',RECL=1)
	read(iunit,rec=1) nplot,jstrec,lstrec
	nfsav=nplot
	if(nplot.eq.0) then
	   jstrec(1)=1025		!now transparent -byte addresses
	   istrec=jstrec(1)
	else
	   istrec=(lstrec(nPLOT))+32  !start record for transparent write
	endif
	ntot=0
	do j=1,nset
	nj(j)=ndatset(iplot,j)
	   ntot=ntot+nj(j)
  	enddo
	if(allocated(datcop)) deallocate(datcop)
	ALLOCATE(datcop(3*ntot))
	k=0		!index for datcop
	do j=1,nset
	
	   n=nj(j)
	   do i=1,n
	      k=k+1
	      datcop(k)=Xobs(i,j)
	   enddo
	   if(.not.hdisp) then	!X only for histograms
	       do i=1,n
	              k=k+1
	              datcop(k)=Yobs(i,j)
			enddo
			do i=1,n
	              k=k+1
	              datcop(k)=w(i,j)
			enddo
	    endif
	  enddo
	  nvalt=k*4			!in bytes
	  kdat=k
	  ioffset=2048		!in bytes
	  nvalt=nvalt+ioffset	!1024 bytes for stuff before datcop
	  iver=1003		!set the value for output
	  irec=istrec
	  k=kdat
	  hdisp=.false.
	  nplot=nplot+1
	  lastr=istrec+nvalt-1		!last byte # for current data
	  jstrec(NPLOT)=istrec !set value for current plot
	  lstrec(NPLOT)=lastr	!set value for current plot
	  irec=istrec
	  hdisp=.false.
	  do j=1,nset
		titled(j)=stitle(j,iplot)
      enddo
	  write(iunit,rec=1) NPLOT,jstrec,lstrec,iver
	  write(iunit,rec=irec) nset,titlef,(titled(j),j=1,nset),&
     	   (nj(j),j=1,nset),(setx(j),j=1,nset),setvar,iw,&
          titlex,titley,ilabel,hdisp,ioffset,k
	  write(iunit,rec=irec+ioffset) (DATCOP(i),i=1,k)
	  deallocate(datcop)
		close(unit=iunit)
	      
end