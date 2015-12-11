subroutine store_record(iplotype,modplot,iplot,ixp,iyp,ipos,xval,yval,w,nj,niobs,njset,nplot,nset,&
		juse,nsfit,jset,titlex,titley,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
		wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
		ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
		xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,&
		oldrecords,dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)


!ILOG=0 for arithmetic plot,
!	 =1 for plot Y vs log(x)
!	 =2 for plot log(Y) vs x
!	 =3 for plot log(Y) vs log(x)
!	 =4 for Hill plot
!	 =5 for sqrt(y) vs x
!	 =6 for sqrt(y) vs log(x)
use hjcrecords
real xval(n1:Ndv1,Ndimd),yval(n1:ndv1,Ndimd),w(kwi,kwj)
integer nj(njset),juse(ndimd),ndat(njset),icurvd(ndimd)
real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
real*4 symsiz(ndimd)	
integer*4 ncal(ndimc),icurvc(ndimc),isym(ndimd),ijoin(ndimd),iline(ndimc)
integer*4 icurvw(ndimd),jmiss(ndimd)
logical logx,logy,sqrty,logity,DOFRAME,hdisp,calbarx,calbary,IVPLOT
character*75 titlex,titley
character*64 title1
CHARACTER*10 CNUMX(25),CNUMY(25),CEXPX(25),CEXPY(25),CNUMZ(25),CEXPZ(25)
CHARACTER*80 NEWTEXT(20)
CHARACTER*200 PARVAL

COMMON/TPOS/IDRAW(250),ICOL(250),THICK(250),C_THICK(250),ITYPE(250),IFNT(100),&
	 ANGLE(100),IJUS(100),SIZEtext(100),RXBOX(4,100),RYBOX(4,100),&
     RX(100),RY(100),NARROW,NLINE,NHLINE,NVLINE, XBEG(50),YBEG(50),XEND(50),YEND(50),&
     NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,&
     NUMBX,NUMBY,NUMBZ,IHLINREL(10),IVLINREL(10)

COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),&
    xoff1,y1v,y2v,y1c,y2c,ncjump,nvjump,ivplot

common/defaults/xmin10,ymin10,xmax10,ymax10
	
	
	TYPE (RECORD_ATTRIBUTES) oldrecords(25)

	if(isens.eq.1) then
	oldrecords(modplot)%iplot=iplot
	oldrecords(modplot)%iplotype=iplotype
	oldrecords(modplot)%hdisp=hdisp
	oldrecords(modplot)%STR(1)=title1
	oldrecords(modplot)%STR(2)=PARVAL
	oldrecords(modplot)%STR(3)=titleX
	oldrecords(modplot)%STR(4)=titleY
	DO K=1,NUMBX
		nl=len_trim(cnumx(k))
		if(logx.and. cnumx(k)(1:2).eq.'0.') then
	      j=3
		  do while (cnumx(k)(j:j).ne.'1')
			j=j+1
		  enddo
		  oldrecords(modplot)%str(k+5)=cnumx(k)(1:j)
	      cnumx(k)=oldrecords(modplot)%str(k+5)
		else
		oldrecords(modplot)%str(k+5)= cnumx(k)
		endif
	ENDDO
	DO K=1,NUMBy
	nl=len_trim(cnumy(k))
		if(logy.and. cnumy(k)(1:2).eq.'0.') then
	      j=3
		  do while (cnumy(k)(j:j).ne.'1')
			j=j+1
		  enddo
		  oldrecords(modplot)%str(k+30)=cnumy(k)(1:j)
	      cnumy(k)=	oldrecords(modplot)%str(k+30)
		else
		oldrecords(modplot)%str(k+30)= cnumY(k)
		endif
	ENDDO
	DO K=1,NTEXT
		oldrecords(modplot)%STR(K+80)=NEWTEXT(K)
	ENDDO
	oldrecords(modplot)%kwi=kwi
	oldrecords(modplot)%kwj=kwj
	oldrecords(modplot)%IxP=IxP
	oldrecords(modplot)%IYP=IYP
	oldrecords(modplot)%IPOS=IPOS
	oldrecords(modplot)%xmin=xmin
	oldrecords(modplot)%ymin=ymin
	oldrecords(modplot)%ymax=ymax
	oldrecords(modplot)%xmax=xmax
	oldrecords(modplot)%xmin1=xmin1
	oldrecords(modplot)%ymin1=ymin1
	oldrecords(modplot)%ymax1=ymax1
	oldrecords(modplot)%xmax1=xmax1
	oldrecords(modplot)%xmin10=xmin10
	oldrecords(modplot)%ymin10=ymin10
	oldrecords(modplot)%ymax10=ymax10
	oldrecords(modplot)%xmax10=xmax10
	oldrecords(modplot)%Wxmin=Wxmin
	oldrecords(modplot)%wymin=wymin
	oldrecords(modplot)%wymax=wymax
	oldrecords(modplot)%wxmax=wxmax
	oldrecords(modplot)%dxs=dxs
	oldrecords(modplot)%dys=dys
	oldrecords(modplot)%numsets=nset
	oldrecords(modplot)%ncurvd=ncurvd
	oldrecords(modplot)%nsfit=nsfit
	oldrecords(modplot)%nDV1=nDV1
	oldrecords(modplot)%nDIMC=nDIMC
	oldrecords(modplot)%nDIMD=nDIMD
	oldrecords(modplot)%nDC1=nDC1
	ilog=0
	if(logx) ilog=1
	if(logy) ilog=2
	if(logx.and.logy) ilog=3
	if(logity) ilog=4
	if(sqrty) ilog=5
	if(logx.and.sqrty) ilog=6
	oldrecords(modplot)%ilog=ilog
	oldrecords(modplot)%x0=x0
	oldrecords(modplot)%y0=y0
	oldrecords(modplot)%yinf=yinf
	oldrecords(modplot)%ncurvc=ncurvc
	do i=1,njset
	oldrecords(modplot)%ndat(i)=ndat(i)
	oldrecords(modplot)%nj(i)=nj(i)
	enddo
	do i=1,ncurvd
		
		
		oldrecords(modplot)%icurvd(i)=icurvd(i)
		oldrecords(modplot)%icurvw(i)=icurvw(i)
		oldrecords(modplot)%jmiss(i)=jmiss(i)
		
		oldrecords(modplot)%juse(i)=juse(i)
		
		oldrecords(modplot)%isym(i)=isym(i)
		oldrecords(modplot)%symsiz(i)=symsiz(i)
		oldrecords(modplot)%ijoin(i)=ijoin(i)
	
		j1=0
		if(hdisp) j1=1
		if(ncurvd.gt.0) then
		if(ndat(i).ge.20480) then
		do j=1,20480
			oldrecords(modplot)%xval(j,i)=xval(j,i)
			oldrecords(modplot)%yval(j,i)=yval(j,i)
			if(.not.hdisp.and.kwi.gt.1) oldrecords(modplot)%w(j,i)=w(j,i)
		enddo
		else
		do j=1,ndat(i)+j1
			oldrecords(modplot)%xval(j,i)=xval(j,i)
			oldrecords(modplot)%yval(j,i)=yval(j,i)
			if(.not.hdisp.and.kwi.gt.1) oldrecords(modplot)%w(j,i)=w(j,i)
		enddo
		endif
		endif

	enddo
	
	!if(ncurvc.gt.0) then
	do i=1,ncurvc
		oldrecords(modplot)%icurvc(i)=icurvc(i)
		oldrecords(modplot)%ncal(i)=ncal(i)
		oldrecords(modplot)%iline(i)=iline(i)
			if(ncal(i).gt.0) then
			if(ncal(i).ge.20480) then
		do j=1,20480
		oldrecords(modplot)%xcal(j,i)=xcal(j,i)
			oldrecords(modplot)%ycal(j,i)=ycal(j,i)
		enddo
		else
		do j=1,ncal(i)
			oldrecords(modplot)%xcal(j,i)=xcal(j,i)
			oldrecords(modplot)%ycal(j,i)=ycal(j,i)
		enddo
		endif
		endif
	enddo
	!endif
	oldrecords(modplot)%lines%nline=nline
	oldrecords(modplot)%lines%nhline=nhline
	oldrecords(modplot)%lines%nvline=nvline
	oldrecords(modplot)%lines%narrow=narrow
	do i=1,50
	oldrecords(modplot)%lines%xbeg(i)=xbeg(i)
	oldrecords(modplot)%lines%ybeg(i)=ybeg(i)
		oldrecords(modplot)%lines%xend(i)=xend(i)
	oldrecords(modplot)%lines%yend(i)=yend(i)
	enddo
		oldrecords(modplot)%numbers%numbx=numbx
		oldrecords(modplot)%numbers%numby=numby
		oldrecords(modplot)%numbers%inumx=inumx
		oldrecords(modplot)%numbers%inumy=inumy
		oldrecords(modplot)%numbers%ntext=ntext
		oldrecords(modplot)%param_axis%itx=itx
		oldrecords(modplot)%param_axis%ity=ity
		oldrecords(modplot)%param_axis%ntx=ntx
		oldrecords(modplot)%param_axis%nty=nty
		oldrecords(modplot)%param_axis%nx1=nx1
		oldrecords(modplot)%param_axis%ny1=ny1
		oldrecords(modplot)%param_axis%xtic=xtic
		oldrecords(modplot)%param_axis%ytic=ytic
		oldrecords(modplot)%param_axis%xcross=xcross
		oldrecords(modplot)%param_axis%ycross=ycross
		oldrecords(modplot)%param_axis%tlenx=tlenx
		oldrecords(modplot)%param_axis%tleny=tleny
		oldrecords(modplot)%param_axis%doframe=doframe
		oldrecords(modplot)%param_axis%calbarx=calbarx
		oldrecords(modplot)%param_axis%calbary=calbary
		do j=1,100
			oldrecords(modplot)%attributes%IFNT(j)=ifnt(j)
			oldrecords(modplot)%attributes%IFNT0(j)=ifnt(j)
			oldrecords(modplot)%attributes%IJUS(j)=ijus(j)
			oldrecords(modplot)%attributes%SIZETEXT(j)=sizetext(j)
			oldrecords(modplot)%attributes%angle(j)=angle(j)
			oldrecords(modplot)%attributes%rx(j)=rx(j)
			oldrecords(modplot)%attributes%ry(j)=ry(j)
			do k=1,4
				oldrecords(modplot)%attributes%rxbox(k,j)=rxbox(k,j)
				oldrecords(modplot)%attributes%rybox(k,j)=rybox(k,j)
			enddo
		enddo
		do j=1,250
			oldrecords(modplot)%attributes%Idraw(j)=idraw(j)
			oldrecords(modplot)%attributes%thick(j)=thick(j)
			oldrecords(modplot)%attributes%ItypE(j)=itype(j)
			oldrecords(modplot)%attributes%ICOL(j)=iCOL(j)
		enddo
	oldrecords(modplot)%JUMPS%IVPLOT=IVPLOT
	oldrecords(modplot)%JUMPS%NCJUMP=NCJUMP
	oldrecords(modplot)%JUMPS%NVJUMP=NVJUMP
	oldrecords(modplot)%JUMPS%Y1V=Y1V
	oldrecords(modplot)%JUMPS%Y2V=Y2V
	oldrecords(modplot)%JUMPS%Y1C=Y1C
	oldrecords(modplot)%JUMPS%Y2C=Y2C
	oldrecords(modplot)%JUMPS%XOFF1=XOFF1
	DO I=1,10
	oldrecords(modplot)%JUMPS%T1C(I)=T1C(I)
	oldrecords(modplot)%JUMPS%T2C(I)=T2C(I)
	oldrecords(modplot)%JUMPS%T1V(I)=T1V(I)
	oldrecords(modplot)%JUMPS%T2V(I)=T2V(I)
	ENDDO
else
do i=1,50
	xbeg(i)=oldrecords(modplot)%lines%xbeg(i)
	ybeg(i)=oldrecords(modplot)%lines%ybeg(i)
	enddo
	kwi=oldrecords(modplot)%kwi
	kwj=oldrecords(modplot)%kwj
	iplot=oldrecords(modplot)%iplot
	iplotype=oldrecords(modplot)%iplotype
	NDV1=oldrecords(modplot)%nDV1
	NDIMC=oldrecords(modplot)%nDIMC
	NDIMD=oldrecords(modplot)%nDIMD
	NDC1=oldrecords(modplot)%nDC1
	hdisp=oldrecords(modplot)%hdisp
	title1=oldrecords(modplot)%STR(1)
	PARVAL=oldrecords(modplot)%STR(2)
	titlex=oldrecords(modplot)%STR(3)
	titley=oldrecords(modplot)%STR(4)
	IXP=oldrecords(modplot)%IxP
	IYP=oldrecords(modplot)%IYP
	IPOS=oldrecords(modplot)%IPOS
	xMIN=oldrecords(modplot)%xMIN
	yMIN=oldrecords(modplot)%yMIN
	XMAX=oldrecords(modplot)%XMAX
	YMAX=oldrecords(modplot)%YMAX
	xmin1=oldrecords(modplot)%xmin1
	ymin1=oldrecords(modplot)%ymin1
	ymax1=oldrecords(modplot)%ymax1
	xmax1=oldrecords(modplot)%xmax1
	xmin10=oldrecords(modplot)%xmin10
	ymin10=oldrecords(modplot)%ymin10
	ymax10=oldrecords(modplot)%ymax10
	xmax10=oldrecords(modplot)%xmax10
	wxMIN=oldrecords(modplot)%wxMIN
	wyMIN=oldrecords(modplot)%wyMIN
	wXMAX=oldrecords(modplot)%wXMAX
	wYMAX=oldrecords(modplot)%wYMAX
	dxs=oldrecords(modplot)%dxs
	dys=oldrecords(modplot)%dys
	nset=oldrecords(modplot)%numsets
	ncurvd=oldrecords(modplot)%ncurvd
	nsfit=oldrecords(modplot)%nsfit
	ilog=oldrecords(modplot)%ilog
	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	logity=ilog.eq.4		!for Hill plot
	sqrty=ilog.eq.5.or.ilog.eq.6
	x0=oldrecords(modplot)%x0
	y0=oldrecords(modplot)%y0
	yinf=oldrecords(modplot)%yinf
	ncurvc=oldrecords(modplot)%ncurvc
	nline=oldrecords(modplot)%lines%nline
	nhline=oldrecords(modplot)%lines%nhline
	nvline=oldrecords(modplot)%lines%nvline
	narrow=oldrecords(modplot)%lines%narrow
	do i=1,njset
		nJ(i)=oldrecords(modplot)%NJ(i)
		ndat(i)=oldrecords(modplot)%ndat(i)
	enddo
	do i=1,ncurvd
	icurvw(i)=oldrecords(modplot)%icurvw(i)
		jmiss(i)=oldrecords(modplot)%jmiss(i)
		symsiz(i)=oldrecords(modplot)%symsiz(i)
	
		JUSE(i)=oldrecords(modplot)%JUSE(i)
		
		
		
		icurvd(i)=oldrecords(modplot)%icurvd(i)
		isym(i)=oldrecords(modplot)%isym(i)
		ijoin(i)=oldrecords(modplot)%ijoin(i)
			j1=0
		if(hdisp) j1=1
		if(ndat(i).gt.20480) then
		do j=1,20480
		xval(j,i)=oldrecords(modplot)%xVal(j,i)
			yval(j,i)=oldrecords(modplot)%yVal(j,i)
			if(.not.hdisp.and.kwi.gt.1) w(j,i)=oldrecords(modplot)%w(j,i)
		enddo
		else
		do j=1,ndat(i)+j1
			xval(j,i)=oldrecords(modplot)%xVal(j,i)
			yval(j,i)=oldrecords(modplot)%yVal(j,i)
			if(.not.hdisp.and.kwi.gt.1) w(j,i)=oldrecords(modplot)%w(j,i)

		enddo
	    endif
	
	enddo
!	if(ncurvc.gt.0) then
	do i=1,ncurvc
		iline(i)=oldrecords(modplot)%iline(i)
		icurvc(i)=oldrecords(modplot)%icurvc(i)
		ncal(i)=oldrecords(modplot)%ncal(i)
		if(ncal(i).gt.0) then
		if(ncal(i).ge.20480) then
		do j=1,20480
		xcal(j,i)=oldrecords(modplot)%xcal(j,i)
			ycal(j,i)=oldrecords(modplot)%ycal(j,i)
		enddo
		else
		do j=1,ncal(i)
			xcal(j,i)=oldrecords(modplot)%xcal(j,i)
			ycal(j,i)=oldrecords(modplot)%ycal(j,i)
		enddo
		endif
		endif
	enddo
!	endif
	numbx=oldrecords(modplot)%numbers%numbx
	numby=oldrecords(modplot)%numbers%numby
	ntext=oldrecords(modplot)%numbers%ntext
	DO K=1,NUMBX
		cnumx(k)=oldrecords(modplot)%str(k+5)
	ENDDO
	DO K=1,NUMBy
		cnumy(k)=oldrecords(modplot)%str(k+30)
	ENDDO
	DO K=1,NTEXT
		NEWTEXT(K)=oldrecords(modplot)%STR(K+80)
	ENDDO
		inumx=oldrecords(modplot)%numbers%inumx
		inumy=oldrecords(modplot)%numbers%inumy
		itx=oldrecords(modplot)%param_axis%itx
		ity=oldrecords(modplot)%param_axis%ity
		ntx=oldrecords(modplot)%param_axis%ntx
		nty=oldrecords(modplot)%param_axis%nty
		nx1=oldrecords(modplot)%param_axis%nx1
		ny1=oldrecords(modplot)%param_axis%ny1
		xtic=oldrecords(modplot)%param_axis%xtic
		ytic=oldrecords(modplot)%param_axis%ytic
		xcross=oldrecords(modplot)%param_axis%xcross
		ycross=oldrecords(modplot)%param_axis%ycross
		tlenx=oldrecords(modplot)%param_axis%tlenx
		tleny=oldrecords(modplot)%param_axis%tleny
		doframe=oldrecords(modplot)%param_axis%doframe
		calbarx=oldrecords(modplot)%param_axis%calbarx
		calbary=oldrecords(modplot)%param_axis%calbary
	
		do j=1,100
			ifnt(j)=oldrecords(modplot)%attributes%IFNT(j)
			ijus(j)=oldrecords(modplot)%attributes%IJUS(j)
			sizetext(j)=oldrecords(modplot)%attributes%SIZETEXT(j)
			angle(j)=oldrecords(modplot)%attributes%angle(j)
			rx(j)=oldrecords(modplot)%attributes%rx(j)
			ry(j)=oldrecords(modplot)%attributes%ry(j)
			do k=1,4
				rxbox(k,j)=oldrecords(modplot)%attributes%rxbox(k,j)
				rybox(k,j)=oldrecords(modplot)%attributes%rybox(k,j)
			enddo
		enddo
		do j=1,250
			idraw(j)=oldrecords(modplot)%attributes%Idraw(j)
			iCOL(j)=oldrecords(modplot)%attributes%ICOL(j)
			thick(j)=oldrecords(modplot)%attributes%thick(j)
			itype(j)=oldrecords(modplot)%attributes%itype(j)
		enddo
	IVPLOT=oldrecords(modplot)%JUMPS%IVPLOT
	NCJUMP=oldrecords(modplot)%JUMPS%NCJUMP
	NVJUMP=oldrecords(modplot)%JUMPS%NVJUMP
	Y1V=oldrecords(modplot)%JUMPS%Y1V
	Y2V=oldrecords(modplot)%JUMPS%Y2V
	Y1C=oldrecords(modplot)%JUMPS%Y1C
	Y2C=oldrecords(modplot)%JUMPS%Y2C
	XOFF1=oldrecords(modplot)%JUMPS%XOFF1
	DO I=1,10
	T1C(I)=oldrecords(modplot)%JUMPS%T1C(I)
	T2C(I)=oldrecords(modplot)%JUMPS%T2C(I)
	T1V(I)=oldrecords(modplot)%JUMPS%T1V(I)
	T2V(I)=oldrecords(modplot)%JUMPS%T2V(I)
	ENDDO
endif
end
		