	subroutine show_boxes(x,y,mm,str,rx,ry,angle,ijus,sizetext,icol,itemp,idraw,&
	inewpos,dxs,dys)

	real xbox(4),ybox(4)
	DIMENSION IFNT(100),IJUS(100),SIZEtext(100),ANGLE(100),ICOL(250)
	DIMENSION RX(100),RY(100),THICK(250),IDRAW(250)
	integer itemp(100),flag
	character*150  str(100)
	logical bold,italik,underline
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
    if(inewpos.eq.1) then
	!	   do k=1,mm
	!			call write_string(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
	!			ijus(itemp(k)),ifnt(itemp(K)),sizetext(itemp(k)),0,dxs,dys)
	!			rx(itemp(k))=x
	!			ry(itemp(k))=y
	!			call string_box(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
	!			ijus(itemp(k)),sizetext(itemp(k)),12,1,xbox,ybox,dxs,dys)
	!	   enddo
    else		
	if(mm.gt.0) then
	    do k=1,mm
			if( idraw(itemp(k)).eq.-1) then
				imode=1
            else
				imode=-1
			endif
			call string_box(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
			ijus(itemp(k)),sizetext(itemp(k)),icol(itemp(k)),imode,xbox,ybox,dxs,dys)
      	enddo
	    mm=0
	endif
    do i=1,100
		call string_box(str(i),rx(i),ry(i),angle(i),ijus(i),sizetext(i),icol(i),0,&
		xbox,ybox,dxs,dys)
	   	call inside(x,y,xbox,ybox,flag) !screen-screen
		if(flag.eq.1.and.idraw(i).ne.0) then
		    mm=mm+1
		    itemp(mm)=i
		endif
	enddo
	if(mm.gt.0) then       		! show box(es) if inside
		do k=1,mm
			call glincols(icol(itemp(k)),idev)    !!!!
			call string_box(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
			ijus(itemp(k)),sizetext(itemp(k)),icol(itemp(k)),1,xbox,ybox,dxs,dys)
		
      	enddo
	endif
    
    endif
    end