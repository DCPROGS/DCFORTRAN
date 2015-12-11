
	subroutine write_text(parval,xtitle,ytitle,ztitle,title1,cnumx,
     &  cnumy,cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,
     &  inumx,inumy,intx,angle,sizes,ijus,thick,icol,ifont,
     &  rx,ry,idraw,imode,str,dxs,dys)
	
	use gino_f90

	character*10   cnumx(25),cnumy(25),cnumz(25)
      character*10   cexpz(25),cexpx(25),cexpy(25)
	character*11   cnum
	character*64   TITLE1
	character*75   xtitle,ytitle,ztitle	!output from LAXES
	character*80   newtext(20),nwtext		!extra text
	character*200  str(100)
	character*200  parval		!parameter values
	
      integer IFoNT(100),IJUS(100),icol(250)
	real RX(100),RY(100),THICK(250),SIZES(100),ANGLE(100)
	integer idraw(250)
	real xbox(4),ybox(4)
	logical bold,italik,underline
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
	logical logx,logy,sqrty,logity
	common/logval/logx,logy,sqrty,logity
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,
     &	xmin,xmax,ymin,ymax
	

	call gsetlinewidth(0.)
	ang=angle(intx)
	select case(intx)
	case(1)
		x=rx(1)
		y=ry(1)
		
	    nl=len_trim(title1)
	    str(1)=title1(1:nl)
	    if(nl.gt.0) then
	    do ik=1,nl
	    ich=ichar(str(1)(1:1))
	    enddo
	    call write_string(STR(1),x,y,angle(1),ijus(1),ifont(1),
     &	sizes(1),icol(1),dxs,dys)
          endif
		if(idraw(1).eq.-1) then
!			call string_box(str(1),x,y,angle(1),ijus(1),sizes(1),
!     &		icol(1),imode,xbox,ybox,dxs,dys)
		endif
	case(2)
		x=rx(2)
		y=ry(2)
		nl=len_trim(parval)
		str(2)=parval(1:nl)
		if(nl.gt.0) then
		call write_string(STR(2),x,y,angle(2),ijus(2),ifont(2),
     &	sizes(2),icol(2),dxs,dys)
        endif
		if(idraw(2).eq.-1.and.nl.gt.0) then
!			call string_box(str(2),x,y,angle(2),ijus(2),sizes(2),
!     &		icol(2),imode,xbox,ybox,dxs,dys)
		endif
	    
	case(3)
		
		x=rx(3)
		y=ry(3)

		nl=len_trim(xtitle)
		str(3)=xtitle
		if(nl.gt.0) then
	call write_string(STR(3),x,y,angle(3),ijus(3),ifont(3),
     &	sizes(3),icol(3),dxs,dys)
      endif
		if(idraw(3).eq.-1) then
!			call string_box(str(3),x,y,angle(3),ijus(3),sizes(3),
 !    &		icol(3),imode,xbox,ybox,dxs,dys)
		endif    
	case(4)
		
		x=rx(4)
		y=ry(4)

		nl=len_trim(ytitle)
		str(4)=ytitle
	if(nl.gt.0) then
	call write_string(STR(4),x,y,angle(4),ijus(4),ifont(4),
     &	sizes(4),icol(4),dxs,dys)
      endif
	    	if(idraw(4).eq.-1) then
!			call string_box(str(4),x,y,angle(4),ijus(4),sizes(4),
!     &		icol(4),imode,xbox,ybox,dxs,dys)
		endif
	case(5)
		
		x=rx(5)
		y=ry(5)

		nl=len_trim(ztitle)
		str(5)=ztitle
		if(nl.gt.0) then
	call write_string(STR(5),x,y,angle(5),ijus(5),ifont(5),
     &	sizes(5),icol(5),dxs,dys)
      endif
	    if(idraw(5).eq.-1) then
!			call string_box(str(5),x,y,angle(5),ijus(5),sizes(5),
!     &		icol(5),imode,xbox,ybox,dxs,dys)
		endif
	case(6:30)
		
	   do k=1,numbx
	   if(k.le.25) then
		x=rx(k+5)
		y=ry(k+5)
		if(x.le.xmax) then
	   	nl=len_trim(cnumx(k))
		if(logx.and. cnumx(k)(1:2).eq.'0.') then
	      j=3
		  do while (cnumx(k)(j:j).ne.'1')
			j=j+1
		  enddo
		  str(k+5)=cnumx(k)(1:j)
	      cnumx(k)=str(k+5)
		else		
	   		str(k+5)= cnumx(k)(1:nl)
	   	endif
		if(nl.gt.3) nolabx=1
		if(mod(k,2).eq.0) then
			if(numbx.gt.4.and.ipow.ne.-10000) goto 100
             if(numbx.gt.7.and.nolabx.eq.1) goto 100
		   
		endif
		call write_string(str(k+5),x,y,angle(6),ijus(6),ifont(6),
     &	sizes(6),icol(6),dxs,dys)
	    if(idraw(6).eq.-1) then
!			call string_box(str(k+5),x,y,angle(6),ijus(6),sizes(6),
!     &		icol(6),imode,xbox,ybox,dxs,dys)
		endif
100		continue
		endif
		endif
         enddo
	case(31:55)
		
	do k=1,numby
	if(k.le.25) then
		x=rx(k+30)
		y=ry(k+30)
		ymaxo=ymax
		if(sqrty) ymaxo=sqrt(ymax)
		if(y.le.ymaxo) then
	    nl=len_trim(cnumy(k))
		if(logy.and. cnumy(k)(1:2).eq.'0.') then
	      j=3
		  do while (cnumy(k)(j:j).ne.'1')
			j=j+1
		  enddo
		  str(k+30)=cnumy(k)(1:j)
	      cnumy(k)=str(k+30)
		else
			
			if(cnumy(k)(1:2).eq.'0.'.or.cnumy(k)(1:3).eq.'-0.') then
				if(nl.gt.5) nl=5	
			endif
			str(k+30)= cnumy(k)(1:nl)
	   endif
		call write_string(str(k+30),x,y,angle(31),ijus(31),ifont(31),
     &	sizes(31),icol(31),dxs,dys)
	  if(idraw(31).eq.-1) then
!		call string_box(str(k+30),x,y,angle(31),ijus(31),sizes(31),
!     &		icol(31),imode,xbox,ybox,dxs,dys)
		endif
	endif
	endif
	enddo
	case(56:80)
	
	do k=1,numbz
	if(k.le.25) then
		x=rx(k+55)
		y=ry(k+55)
		
	   nl=len_trim(cnumz(k))
	   str(k+55)= cnumz(k)(1:nl)
	   if(logy) then
	
		call CHAEXI(0.8,0.8,0.6,0.3)
	      nl1=len_trim(cexpz(k))
		nl=len_trim(cnumz(k))
	      str(k+55)=cnumz(k)(1:nl)//char(42)//char(69)
     &      //cexpz(k)(1:nl1)
	   endif
	call write_string(str(k+55),x,y,angle(56),ijus(56),ifont(56),
     &	sizes(56),icol(56),dxs,dys)
	   if(idraw(56).eq.-1) then
!			call string_box(str(k+55),x,y,angle(56),ijus(56),sizes(56),
!    &		icol(56),imode,xbox,ybox,dxs,dys)
		endif
		endif
	enddo

	case(81:100)
	
	   x=rx(intx)
	   y=ry(intx)
	
		if(sqrty) then
	        if(y.le.0) y=0.0001
			y=sqrt(y)
		else if(logy) then
	     if(y.le.0) y=0.0001
			y=alog10(y)
		endif
		if(logx) then
	 if(x.le.0) x=0.0001
			x=alog10(x)
		endif
	
	   nwtext=newtext(intx-80)
	   nt=len_trim(nwtext)
	   str(intx)= nwtext(1:nt)
	    call write_string(str(intx),x,y,angle(intx),ijus(intx),
     &	ifont(intx),
     &	sizes(intx),icol(intx),dxs,dys)
	   if(idraw(intx).eq.-1) then
!		call string_box(str(intx),x,y,angle(intx),ijus(intx),
!     &		sizes(intx),icol(intx),imode,xbox,ybox,dxs,dys)
		endif
	end select

      call gFlushGraphics() 
	end
