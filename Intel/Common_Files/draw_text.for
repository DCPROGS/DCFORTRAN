	subroutine draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,
     &  cnumy,cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,
     &  inumx,inumy,intx,angle,sizes,ijus,thick,rx,ry,idraw,
     &  icol,ifnt,str,dxs,dys)

	character*10   cnumx(25),cnumy(25),cnumz(25)
      character*10   cexpz(25),cexpx(25),cexpy(25)
	
	character*64   TITLE1
	character*75   xtitle,ytitle,ztitle	!output from LAXES
	character*80   newtext(20),nwtext		!extra text
	
	character*200  parval		!parameter values
	character*200  str(100)
      integer IFNT(100),IJUS(100),IDRAW(250),ICOL(250)
	real RX(100),RY(100),THICK(250),SIZES(100),ANGLE(100)
	logical bold,italik,underline
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
	logical logx,logy,sqrty,logity
	common/logval/logx,logy,sqrty,logity
	
	
	call linwid(0.)
	do i=1,6
		if(idraw(i).ne.0.and.idraw(i).ne.-2) then
		
		call write_text(parval,xtitle,ytitle,ztitle,title1,
     &	cnumx,cnumy,cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,
     &    ntext,inumx,inumy,i,angle,sizes,ijus,thick,icol,ifnt
     &	,rx,ry,idraw,1,str,dxs,dys)
		endif
	enddo
	if(idraw(31).ne.0.and.idraw(31).ne.-2) then
		
		call write_text(parval,xtitle,ytitle,ztitle,title1,
     &	cnumx,cnumy,cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,
     &	ntext,inumx,inumy,31,angle,sizes,ijus,thick,icol,
     &	ifnt,rx,ry,idraw,1,str,dxs,dys)
	endif
	
	do i=1,ntext
		if(idraw(i+80).ne.0.and.idraw(i+80).ne.-2) then
	
		call write_text(parval,xtitle,ytitle,ztitle,title1,
     &	cnumx,cnumy,cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,
     &    ntext,inumx,inumy,i+80,angle,sizes,ijus,thick,icol,
     &	ifnt,rx,ry,idraw,1,str,dxs,dys)
		endif
	enddo
      
	end