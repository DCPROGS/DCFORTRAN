	subroutine write_plotq(iqplot,iplotype,xval,yval,xcal,ycal,ndimd,
     & ndimc,ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,
     & syms,xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & xlo,xhi,ylo,yhi,itit,title1,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,qfile,sval,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     & theta,ifitype,ncomp,idest,interp,RLTH,y0,yinf,ntrace,ytsep,ndv1,
     & ndc1,weight,kwi,kwj,icurvw,kmax,n1,jmiss,
     & xwbase,isval,lt2,mono,ifont0,ameant,areat,
     & title3,xval1,yval1,z1,bad,nx,ny,ndx,ndy,
     & xtitle3,ytitle3,ztitle3,idrawt,idrawx,idrawy,idrawz,
     & kcol,posneg,i3col,ifnt3,ifnl,alfa,beta,gama,delta,
     & ijust,ijusx,ijusy,ijusz,
     & xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     & xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,
     & ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,
     & nxstep,nystep,istyle,isup,
     & fill,inter,axis,fillbad,cross,ioffs)
	
	USE DFLIB
	use menu_f90
	real*4 z1(ndx,ndy)
	real*4 xval1(ndx),yval1(ndy)
	logical bad(ndx,ndy)
	logical fillbad
	integer*4 i3col(100)
	character*75 xtitle3,ytitle3,ztitle3
	character*75 title3
	logical all,cross,after,posneg,monsav
	logical plot,fill,inter,landplot,axis
	logical quarter
      INTEGER ISTYLE(6),ISUP(6)
      
	allocatable::datcop,int2cop,int4cop,charcop,zeta1,zeta2
	TYPE (FILE$INFO) info
	real*4 DATCOP(:),sizet(100),zeta1(:),zeta2(:)
	integer*2 INT2COP(:)
	integer*4 INT4COP(:)
	character*10 CHARCOP(:)
	integer*1 idum(1)		!to fill file to length that is multiple of 1024
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum1*11,cnum2*11		!for dialog
	real*4 XVAL(n1:ndv1,ndimd),YVAL(n1:ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	dimension ndat1(20),ncal1(20),ijoin1(20),iline1(20)  !for queue
	real*4 syms(ndimd)
	real theta(kmax)
	integer*2 jstrec(200),lstrec(200)		!for queue
	character ndev*2,qfile*40
	character*40 titlex,titley
	character*64 title1,title2	!plot title
	LOGICAL doframe,slock,pon,ivplot,present,newfil,mono
	logical interp

c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	real RLTH(100) 		!for line thickness
	real rx(100),ry(100),rxbox(4,100),rybox(4,100),angle(100)
	character*10 cnumx(25),cnumy(25),cnumz(25),cexpx(25),cexpy(25)
	character*10 cexpz(25)
	
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10),ilvtype(10),ifnt(30)				!line type for ditto
	character*80 newtext(20)		!extra text
c new form for weights in VPLOT5
	real*4 weight(kwi,kwj)
	integer*4 icurvw(ndimd),jmiss(20)
c integer*2 copies to save disc space
	integer*2 IJUS1(100)
	integer*2 idraw1(100),icol1(100)
	COMMON/TPOS/IDRAW(250),ICOL(250),THICK(250),C_THICK(250),
     &ITYPE(250),ifont(100),
     &rANGLE(100),IJUS(100),SIZEtext(100),rRXBOX(4,100),rRYBOX(4,100),
     &rrX(100),rRY(100),NARROW,NLINE,NHLINE,NVLINE, XBEG(50),YBEG(50),
     &XEND(50),YEND(50),
     &nTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,
     &nUMBX,NUMBY,NUMBZ,IHLINREL(10),IVLINREL(10)
	real ameant(100),areat(100)
	common/dp/discprt
      logical discprt
	
3	format(i8)
	idest=0

!	if(ncomp.eq.0) ncomp=15
	select case(iplotype)
		case(-1)
			iptype=16
		case(1)
			iptype=16
		case(2)
			iptype=23
		case(3)
			iptype=16
		case(4)
			iptype=42
	end select
      
	do i=1,ndimd
	if(i.le.20) then
		ndat1(i)=ndat(i)
		ijoin1(i)=ijoin(i)
	endif
	enddo
	do i=1,ndimc
	if(i.le.20) then
		ncal1(i)=ncal(i)
		iline1(i)=iline(i)
	endif
	enddo
      iver=1200	!NB also set below

	
	OPEN(unit=11,file=qfile,status='UNKNOWN',access='DIRECT',
     &   form='BINARY',RECL=1)
		READ(11,REC=1,IOSTAT=I_VAR) NPLOT,JSTREC,LSTREC,IVER        
	
	idum(1)=0		!to fill file to length that is multiple of 1024
	ndim=0
	if(iplotype.eq.4) then
	  
		indk=ndx*ndy
		allocate(zeta1(indk+1),zeta2(indk+1))
		k=1
		do i3=1,ndx
			do j=1,ndy
				zeta1(k)=z1(i3,j)
				zeta2(k)=bad(i3,j)
				k=k+1
				enddo
			enddo
		
        nbyte1=4*100
        nbyte2=4*ndx+4*ndy+8*indk
        ioffset=2048
	  nbytet=ioffset+nbyte1+nbyte2+1024
	  nrect=1 + (nbytet-1)/1024	!for everything
	  if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	      iplot=1
	      jstrec(1)=2
	      istrec=1025		!for transparent write
	      nplot1=iplot
	  else
		    if(iqplot.eq.nplot+1) then
			istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
			iplot=nplot+1		!next number
			nplot1=iplot
		    else if(iqplot.eq.nplot) then	!overwrite the last plot
			iplot=iqplot
			istrec=int4(lstrec(iplot))*1024+1  !start record for transparent write
			nplot1=iqplot
		    else
			iplot=iqplot
			if(iplot.eq.1) then
			istrec=1025
			else
			istrec=int4(lstrec(iplot-1))*1024+1  !start record for transparent write
			endif
			read(11,rec=istrec) iptype0
			if(iptype0.ne.42) then 
				istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
				iplot=nplot+1		!next number
				nplot1=iplot
				imes=gmdisplaymessagebox('',
     &    'Cannot override record. New record created',ginformation,gok)
			endif
			nplot1=nplot
		    endif
	  endif	
	  lastr=istrec+nbytet-1	
	  lastr1=lastr		!rounded up
	  n=mod(lastr,1024)
	  if(n.ne.0) then
	   nfill=1024-n
	   lastr1=lastr+nfill
	  endif
		jstrec(iplot)=1 + ((istrec-1)/1024)
		lstrec(iplot)=jstrec(iplot)+(nrect) - 1
		lstrec(iplot)=1 + ((lastr-1)/1024)	!should be same!
		
        istr1=istrec+ioffset
        write(11,rec=istrec)iptype,title3,nx,ny,ndx,ndy,numx,numy,
     &  xtitle3,ytitle3,ztitle3,idrawt,idrawx,idrawy,idrawz,xst,yst,
     &  xspx,yspx,xspy,yspy,xspz,yspz,ifnt3,ifnl,alfa,beta,gama,delta,
     &  ijust,ijustx,ijusty,ijustz,xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,
     &  xtic3,ytic3,ztic3,ratio,rat,radius,theta3,phi,ifram,numaxi,
     &  nwidth,npow,nplace,nxstep,nystep,istyle,isup,kcol,
     &  fill,inter,axese,fillbad,cross,posneg,ioffset
		write(11,rec=istr1) (i3col(k),k=1,100),(XVAL1(k),k=1,ndx),
     &  (YVAL1(k),k=1,ndy),(zeta1(k),k=1,indk),(zeta2(k),k=1,indk)
	  write(11,rec=1) nplot1,jstrec,lstrec,iver
	  
	  CLOSE(UNIT=11)
	  DEALLOCATE(zeta1,zeta2)
      else
	
	if(ncurvd.gt.0) then
	   do j1=1,ncurvd
		j=icurvd(j1)
		ndim=ndim+ndat(j)*2	!for xval, yval
		if(jmiss(j1).eq.1) icol(100+j1)=-1
	   enddo
	   if(icurvw(j).ge.0) then
		do j1=1,ncurvd
		   j=icurvd(j1)
		   ndim=ndim+ndat(j)	!for weight()
		enddo
	   endif
	endif
	if(ncurvc.gt.0) then
	   do j1=1,ncurvc
		j=icurvc(j1)
		ndim=ndim+ncal(j)*2	!for xcal, ycal
	   enddo
	endif
	ndim=ndim+kmax+80+ntext*8+numbx*7+numby*7+narrow*4+nline*4
	ndim=ndim+(nhline+nvline)*3
	ndim=ndim + 256		!+256 in case!
c
	if(iptype.eq.16) then
	ALLOCATE(datcop(ndim),int2cop(1000),charcop(100))
	
	else
	ALLOCATE(datcop(500000),int2cop(1000),charcop(100))
	endif
	ALLOCATE(int4cop(ndimd+ndimc))
c
	if(iver.eq.1100.or.iver.eq.1200) then
	do i=1,10
		xb(i)=xbeg(i)
		yb(i)=ybeg(i)
		xe(i)=xend(i)
		ye(i)=yend(i)
		xlb(i)=xbeg(i+10)
		ylb(i)=ybeg(i+10)
		xle(i)=xend(i+10)
		yle(i)=yend(i+10)
	    iltype(i)=itype(i+200)
		xhlb(i)=xbeg(i+20)
		yhline(i)=ybeg(i+20)
		xhle(i)=xend(i+20)
		yhline(i)=yend(i+20)
		ilhtype(i)=itype(i+210)
		xvline(i)=xbeg(i+30)
		yvlb(i)=ybeg(i+30)
		xvline(i)=xend(i+30)
		yvle(i)=yend(i+30)
		ilvtype(i)=itype(i+220)
		
		icol1(i+50)=icol(i+200)
		icol1(i+60)=icol(i+210)
		icol1(i+71)=icol(i+220)
		icol1(i+81)=icol(i+230)
		idraw1(i+10)=idraw(i+80)
		ifnt(i+10)=ifont(i+80)
		icol1(i+30)=icol(i+80)
		sizet(i+10)=sizetext(i+80)
		icol1(i)=icol(i+100)
		icol1(i+10)=icol(i+150)
		idraw1(i)=idraw(i+100)
		idraw1(i+10)=idraw(i+150)
		
	enddo

	do i=6,55
		ifnt(7)=ifont(i)
		sizet(7)=sizetext(i)
		idraw1(i+25)=idraw(i)
		icol1(24)=icol(i)
		angle(i+25)=rangle(i)
		rlth(24)=thick(i)
		rx(i+25)=rrx(i)
		ry(i+25)=rry(i)
		do j=1,4
			rxbox(j,i+25)=rrxbox(j,i)
			rybox(j,i+25)=rrybox(j,i)
		enddo
	enddo
	
	icol1(25)=icol(1)
	idraw1(10)=idraw(1)
	ifnt(10)=ifont(1)
	angle(10)=rangle(1)
	sizet(10)=sizetext(1)
	rlth(25)=thick(1)
	rx(10)=rrx(1)
	ry(10)=rry(1)
	do j=1,4
		rxbox(j,1)=rrxbox(j,1)
		rybox(j,1)=rrybox(j,1)
	enddo

	icol1(26)=icol(2)
	idraw1(6)=idraw(2)
	ifnt(6)=ifont(2)
	angle(6)=rangle(2)
	sizet(6)=sizetext(2)
	rlth(26)=thick(2)
	rx(6)=rrx(2)
	ry(6)=rry(2)
	do j=1,4
		rxbox(j,6)=rrxbox(j,2)
		rybox(j,6)=rrybox(j,2)
	enddo

	icol1(23)=icol(3)
	idraw1(8)=idraw(3)
	ifnt(8)=ifont(3)
	angle(8)=rangle(3)
	sizet(8)=sizetext(3)
	rlth(23)=thick(3)
	rx(8)=rrx(3)
	ry(8)=rry(3)
	do j=1,4
		rxbox(j,8)=rrxbox(j,3)
		rybox(j,8)=rrybox(j,3)
	enddo

	icol1(23)=icol(4)
	idraw1(9)=idraw(4)
	ifnt(9)=ifont(4)
	angle(9)=rangle(4)
	sizet(9)=sizetext(4)
	rlth(23)=thick(4)
	rx(9)=rrx(4)
	ry(9)=rry(4)
	do j=1,4
		rxbox(j,4)=rrxbox(j,9)
		rybox(j,4)=rrybox(j,9)
	enddo
	
	

	icol1(22)=icol(244) !frame
	icol1(21)=icol(245) !axis
	rlth(22)=thick(244)
	rlth(21)=thick(245)

	
	endif


	k=0		!index for datcop
	if(ncurvd.le.0) goto 365
	if(iptype.eq.16) then
	do j1=1,ncurvd
	   j=icurvd(j1)
	   n=ndat(j)
	   do i=1,n
		k=k+1
		datcop(k)=Xval(i,j)
	   enddo
	   do i=1,n
		k=k+1
		datcop(k)=Yval(i,j)
	   enddo
	  
	   k=k+1
	   datcop(k)=syms(j)
	   if(icurvw(j).ge.0) then
		
		do i=1,n
		   k=k+1
		   datcop(k)=weight(i,j)
		enddo
		endif
	  
	enddo
	else
	   do 37 j=1,ncurvd
	   n=ndat(j) +2
	   do 371 i=0,n
	   k=k+1
371	   datcop(k)=Xval(i,j)
	   do 372 i=0,n
	   k=k+1
372	   datcop(k)=Yval(i,j)
37	continue
	endif
365	continue
c Write theta() here now (will be defined only if ncurvc>0)

	if(ncurvc.gt.0) then
	   do jk=1,ncurvc
		if(ncal(jk).gt.ndc1) ncal(jk)=ndc1
	   enddo
	   do j1=1,ncurvc
		j=icurvc(j1)
		n=ncal(j)
		do i=1,n
		   k=k+1
		   datcop(k)=xcal(i,j)
		enddo
		do i=1,n
		   k=k+1
 		   datcop(k)=ycal(i,j)
		enddo
	   enddo
		 if(iptype.eq.16) then
	   do i=1,kmax
		k=k+1
 		datcop(k)=theta(i)
	   enddo
	endif
	endif		!end of ncurvc>0
366	continue
c
c Now add onto DATCOP the real*4 data for posh plots- keep incrementing k
c
	do 682 i=1,10
	   k=k+1
	   DATCOP(k)=sizet(i)
         k=k+1
         DATCOP(k)=angle(i)


	   k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 680 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
680	   continue
682	continue		!keep 1-10 always
c
	if(ntext.gt.0) then
	do 68 i1=1,ntext
	   i=i1+10
	   k=k+1
	   DATCOP(k)=sizet(i)
	   k=k+1
         DATCOP(k)=angle(i)
         k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 681 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
681	   continue
68	continue
	endif
	do 69 i1=1,numbx
	   i=i1+30

	   k=k+1
         DATCOP(k)=angle(i)
         k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 691 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
691	   continue
69	continue
	do 70 i1=1,numby
	   i=i1+55
	   k=k+1

         DATCOP(k)=angle(i)
         k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 701 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
701	   continue
70	continue
	if(narrow.gt.0) then
	   do 71 i=1,narrow
	   k=k+1
	   DATCOP(k)=xb(i)
	   k=k+1
	   DATCOP(k)=xe(i)
	   k=k+1
	   DATCOP(k)=yb(i)
	   k=k+1
	   DATCOP(k)=ye(i)
71	   continue
	endif
	if(nline.gt.0) then
	   do 72 i=1,nline
	   k=k+1
	   DATCOP(k)=xlb(i)
	   k=k+1
	   DATCOP(k)=xle(i)
	   k=k+1
	   DATCOP(k)=ylb(i)
	   k=k+1
	   DATCOP(k)=yle(i)
72	   continue
	endif
c now add new lines
	if(nhline.gt.0) then
	   do i=1,nhline
		k=k+1
		DATCOP(k)=xhlb(i)
		k=k+1
		DATCOP(k)=xhle(i)
		k=k+1
		DATCOP(k)=yhline(i)
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
		k=k+1
		DATCOP(k)=xvline(i)
		k=k+1
		DATCOP(k)=yvlb(i)
		k=k+1
		DATCOP(k)=yvle(i)
	   enddo
	endif
      do 60 i=1,30	!keep 1st 30 RLTH always
	   k=k+1
	   DATCOP(k)=RLTH(i)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   DATCOP(k)=RLTH(i+30)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   DATCOP(k)=RLTH(i+50)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   DATCOP(k)=RLTH(i+60)
63	   continue
	endif

c end of DATCOP
	k1=k
	nbyte1=4*k
c
c Write newtext as they are -number of nytes need are:
	k2=ntext
	nbyte2=k2*80	!newtext(i) are character*80
c
c and for CNUMX etc
	k=0		!index for CHARCOP
	do 73 i=1,numbx
	k=k+1
	CHARCOP(k)=cnumx(i)
	k=k+1
	CHARCOP(k)=cexpx(i)
73	continue
c
	do 74 i=1,numby
	k=k+1
	CHARCOP(k)=cnumy(i)
	k=k+1
	CHARCOP(k)=cexpy(i)
74	continue
	k3=k
	nbyte3=10*k
c
c make compressed array for int*2 values
	k=0		!index for int2cop
	if(nhline.gt.0) then
	   do i=1,nhline
		k=k+1
		INT2COP(k)=ilhtype(i)
		k=k+1
		INT2COP(k)=(ihlinrel(i))	!ihlinrel actually int*1 !
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
		k=k+1
		INT2COP(k)=ilvtype(i)
		k=k+1
		INT2COP(k)=(ivlinrel(i))	!ivlinrel actually int*1 !
	   enddo
	endif
c
	do 64 i=1,10
	   k=k+1
	   INT2COP(k)=ijus1(i)
	   k=k+1
	   INT2COP(k)=idraw1(i)
64	continue
	if(ntext.gt.0) then
	   do 65 i=1,ntext
	   k=k+1
	   INT2COP(k)=ijus1(i+10)
	   k=k+1
	   INT2COP(k)=idraw1(i+10)
65	   continue
	endif
	do 66 i=1,numbx
	   k=k+1
	   INT2COP(k)=ijus1(i+30)
	   k=k+1
	   INT2COP(k)=idraw1(i+30)
66	continue
	do 67 i=1,numby
	   k=k+1
	   INT2COP(k)=ijus1(i+55)
	   k =k+1
	   INT2COP(k)=idraw1(i+55)
67	continue
c
c Now store ICOL() array (at end if INT2COP, so read only for versions
c that have colour)
	do i=1,30	!keep 1st 30 ICOL always
	   k=k+1
	   INT2COP(k)=icol1(i)
	enddo
	if(ntext.gt.0) then
	    do i=1,ntext
	      k=k+1
	      INT2COP(k)=icol1(i+30)
	    enddo
	endif
	if(narrow.gt.0) then
	    do i=1,narrow
	      k=k+1
	      INT2COP(k)=icol1(i+50)
	    enddo
	endif
	if(nline.gt.0) then
	    do i=1,nline
	      k=k+1
	      INT2COP(k)=icol1(i+60)
	    enddo
	endif
	if(nhline.gt.0) then
	    do i=1,nhline
	      k=k+1
	      INT2COP(k)=icol1(i+71)
	    enddo
	endif
	if(nvline.gt.0) then
	    do i=1,nvline
	      k=k+1
	      INT2COP(k)=icol1(i+81)
	    enddo
	endif
c record background colour
	k=k+1
	INT2COP(k)=icol1(71)
c Add isym,ijoin,iline,icurvw() for VPLOT5
c and add icurvw() for VPLOT5
	do j1=1,ncurvd
	   j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT2COP(k)=(isym(j))
	enddo
	do j1=1,ncurvd
	   j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT2COP(k)=(ijoin(j))
	enddo
c= Best copy whole of icurvw() always, so then need not write
c   weights if icurvw(j)=-1, ie no weights defined
c=	if(isdev.ge.0) then
	   do j1=1,ncurvd
		j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
		k=k+1
	      INT2COP(k)=(icurvw(j))
	   enddo
c=	endif
	do j1=1,ncurvc
	   j=icurvc(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT2COP(k)=(iline(j))
	enddo
c
	k4=k
	nbyte4=k*2	!no of bytes in  INT2COP
c End of int2cop
c Finally integer*4 values: ndat() and ncal() must be integer*4
c (they could be > 32k)
c -number of bytes that these occupy is = 4*ncurvd+4*ncurvc
	k=0		!index for int4cop
	do j1=1,ncurvd
	   j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT4COP(k)=ndat(j)
	enddo
	do j1=1,ncurvc
	   j=icurvc(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT4COP(k)=ncal(j)
	enddo
c
	k5=k
	nbyte5=4*(ncurvd+ncurvc)
c
c== Total number of records
c Total number of bytes
c First bit actually occupies 552 bytes at present, but leave gap after
c this before writing int2cop etc, in case it is necessary to add other
c things that have to be read first.
c	nbytet=552        !'first record'
	ioffset=768
	nbytet=ioffset    !'first record'
	nbytet=nbytet+nbyte1+nbyte2+nbyte3+nbyte4+nbyte5+1024
c Calculate the number of 1024 byte records that this would occupy if
c fixed record length had been use.
c
	nrect=1 + (nbytet-1)/1024	!for everything
c OPEN DISC FILE
	
c DC fixed code 7-11-06 so that it ia always possible to overwrite the last entry in the plot queue	
	if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	   iplot=1
	   jstrec(1)=2
	   istrec=1025		!for transparent write
	   nplot1=iplot
	else
		if(iqplot.eq.nplot+1) then
			istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
			iplot=nplot+1		!next number
			nplot1=iplot
		else if(iqplot.eq.nplot) then	!overwrite the last plot
			iplot=iqplot
			istrec=int4(lstrec(iplot))*1024+1  !start record for transparent write
			nplot1=iqplot
		else
			iplot=iqplot
			if(iplot.eq.1) then
			istrec=1025
			else
			istrec=int4(lstrec(iplot-1))*1024+1  !start record for transparent write
			endif
			read(11,rec=istrec) iptype0
			if(iptype0.ne.23.and.iptype0.ne.16) then 
				istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
				iplot=nplot+1		!next number
				nplot1=iplot
				imes=gmdisplaymessagebox('',
     &'Cannot override record. New record created',ginformation,gok)
			endif
			nplot1=nplot
		endif
	endif



	!if(iplot.eq.1) jstrec(1)=2	!new start record (rec 1=jstrec etc now)
	!istrec=1 + (jstrec(iplot)-1)*1024	!start record transparent
			!value to be written to 1st record

	lastr=istrec+nbytet-1	
	lastr1=lastr		!rounded up
	n=mod(lastr,1024)
	if(n.ne.0) then
	   nfill=1024-n
	   lastr1=lastr+nfill
	endif
		jstrec(iplot)=1 + ((istrec-1)/1024)
		lstrec(iplot)=jstrec(iplot)+(nrect) - 1
		lstrec(iplot)=1 + ((lastr-1)/1024)	!should be same!
		jcol=1002
	if(iptype.eq.16) then
	    ioffset=2048
		write(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     &	itit,title1,
     &	xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,
     &	ifont0,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
     &	ncurvc,inumx,inumy,sval,
     &	ifitype,ncomp,numbx,numby,ifnt,ntext,k1,k2,k3,k4,k5,
     &	narrow,nline,iltype,y0,yinf,ntx,nty,itx,ity,
     &	ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     &	mono,interp,nhline,nvline,ntrace,ytsep,ioffset,jmiss

	else if(iptype.eq.23) then
		 
	     ioffset=2048
		 write(11,rec=istrec) iptype,ndimd,ndv1,ndimc,ndc1,kmax,
     &     itit,title1,xmin,xmax,ymin,ymax,
     &     xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont0,ilog,
     &     iscal,doframe,titlex,titley,ilabel,ncurvd,ndat,
     &     ncurvc,ncal,ijoin,
     &	 iline,inumx,inumy,sval,isval,
     &     theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     &     nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     &     jcol,mono,ioffset,
     &     (ameant(i),i=1,ncomp),(areat(i),i=1,ncomp)
	else
	endif	
c
c Now write NEWTEXT,CHARCOP,INT2COP,INT4COP and DATCOP to disk
	istr1=istrec+ioffset
	if(iptype.eq.16) then
	if(ntext.eq.0) then
	   write(11,rec=istr1) (INT2COP(i),i=1,k4),(INT4COP(i),i=1,k5),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3)
	else
	   write(11,rec=istr1) (INT2COP(i),i=1,k4),(INT4COP(i),i=1,k5),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3),(NEWTEXT(i),i=1,ntext)
	endif
	else
	if(ntext.eq.0) then
	   write(11,rec=istr1) (INT2COP(i),i=1,k4),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3)
	else
	   write(11,rec=istr1) (INT2COP(i),i=1,k4),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3),(NEWTEXT(i),i=1,ntext)
	endif
	endif
	
	INQUIRE(IOLENGTH=LEN) qFILE
	n=mod(len,1024)
	ihandle=FILE$FIRST
	length = GETFILEINFOQQ(qfile, info, ihandle)
	nLEN=info%length
	n=mod(nlen,1024)
	if(n.ne.0) then
	   nfill=1024-n
!	   write(11,rec=len+1) (idum(1),i=1,nfill)
	endif
c

c UpDATEW rec #1 (after data written safely!)
	write(11,rec=1) nplot1,jstrec,lstrec,iver
c
	
45	CLOSE(UNIT=11)
c put what was done on the printout
      if(discprt) write(7,46) title1,iplot,istrec,lastr,qfile,
     & jstrec(iplot),lstrec(iplot)
46    format(/,1x,a64,'*******************',/,
     & ' Plot queue no ',i3,' in bytes ',i8,' to ',i8,' in ',a40,/,
     & ' (i.e. 1024 byte records numbers ',i5,' to ',i5,')',/)
     
	DEALLOCATE(datcop,int2cop,int4cop,charcop)
99	continue
      endif	
	RETURN
c
	end

