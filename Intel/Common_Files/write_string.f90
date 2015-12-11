subroutine write_string(string,x,y,angle,justify,ifont,textsize,icol1,dxs,dys)

use gino_f90
integer ifont,ifont1
real x,y
character*(*) string
character*200 string1
logical bold,italik,underline,nolabx
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue 
logical logx,logy,sqrty,logity
common/logval/logx,logy,sqrty,logity
type (GFNTFILSTY) style
do i=1,200
	string1(i:i)=' '
enddo
ifont1=ifont
width=textsize*dxs
height=textsize*dys
xr=x
yr=y
width1=0.8*width
height1=1.45*height
if(angle.eq.90.) then
	width=textsize*dys
	height=textsize*dxs
	t=width1
	width1=0.7*height1
	height1=1.8*t
!
endif
call gStartTextBlock(xr,yr)
!call movto2(xr,yr)
call glincols(icol1,idev)

call gsetmixedchars()
call gEnqLineWidth(wid)

x1=x
xr=x
yr=y
if(idev.ge.0)  then
	if (ifontrue.ge.100.and.ifontrue.le.108) then
	!	call gsethardchars()
		ifont1=ifontrue
	else if (ifontrue.ge.150.and.ifontrue.le.170) then
	!	call gsethardchars()
		ifont1=ifontrue
	else
		call gsetmixedchars()
	endif
endif
if (ifont1.ge.100) call gsethardchars()
call gSetFontWeight(0)
!call genqcharattribs(rep)
if(ifont1.le.20 ) then 
	call gSetCharSize(width,height)
else
   !	if(idev.gt.0) call gsethardchars()
   if(idev.eq.0) then
!	call gsetmixedchars()
	if(angle.eq.0) then
	 call gSetCharSize(width1,height1)
	else
!		width1=1.3*width
!		height1=1.15*height
		call gsetmixedchars()
		ifont1=15
		call gSetFontWeight(0)
	
		call gSetCharSize(1.3*width,height)
	endif	
   else if(idev.gt.0.and.idev.lt.3) then
		height1=1.1*height
		width1=0.9*width
		 call gSetCharSize(width1,height1)
   else if(idev.ge.3) then
   	if(angle.eq.0) then
	width1=0.8*width
	height1=1.45*height
	 call gSetCharSize(width1,height1)
	 	else
		call gsetmixedchars()
		ifont1=15
		call gSetFontWeight(0)
	
		call gSetCharSize(width,height)
		call gSetCharSize(1.3*width,height)
	endif
   endif
  
   !call gsetcharsizepoint(12.)
endif
call gSetStrAngle(angle)

call gSetStrJustify(justify)
nl=len_trim(string)
icount=0
do i=1,nl
	string1(i:I)=' '
	string1(i:I)=string(i:i)
enddo
do i=1,nl-1
	if(string1(i:I+1).eq.'//') then
		string1(i:I+1)='£N'
	else if(string1(i:i).eq.'*') then
!	string1(i:I)='£'
	endif
enddo

if(nl.gt.0) then
  
  if(nl.gt.5.and.ifont1.ge.100.and.angle.eq.0.) then
  x1=x
  i=1
  do while (i.le.nl-1)
	if(string1(i:I+1).eq.'£N') then
	k=i-1
	m=0
	n=0
	x1=x1-k*width
	do j=1,k-2
		if(string1(j:j+2).eq.'£F0') n=n+1
		if(string1(j:j+2).eq.'£FR') m=m+1
    enddo
    x1=x1+(3*m+5*n)*width
	
	goto 2
	endif
	i=i+1
  enddo
2 continue 
  call gStartTextBlock(x1,yr)
  !call movto2(x1,xr)
   l=1
   m=1
   i=1

 ! icount=icount+1
  
  do while(i.le.nl-5)
   
	if(string1(i:I+4).eq.'£F010') then
		icount=icount+1
		if(i.ge.2) then
		call gsethardchars()
		call gSetCharFont(ifont1)
		call movto2(x1,yr)
		call gSetStrJustify(-1)
		call gDisplayStr(string1(l:i-1))
		d=(i-1-l)*width
		if(d.lt.0) d=0
		x1=x1+d
		endif
		j=i+5
	

		do while(j.le.nl-3)
			if(string1(j:j+2).eq.'£FR') then
			call gsetmixedchars()
			    call gSetCharFont(4)
				call movto2(x1,yr)
					call gSetStrJustify(-1)
				call gDisplayStr(string1(i:j-1))
				d=(j-1-5)*width
				i=j+3
				l=i
				
				x1=x1+d
				goto 1
			else
			 j=j+1
			endif
		enddo
1		continue
	else
		i=i+1
	endif
	
  enddo

	if(icount.eq.0) then
	!    call gsethardchars()
		call gSetCharFont(ifont1)
		k=ichar(string1(nl:NL))
		if(k.eq.0) nl=nl-1
!!		call movto2(xr,yr)
		call gDisplayStr(string1(1:NL))

	else
		call gsethardchars()
		call gSetCharFont(ifont1)
		k=ichar(string1(nl:NL))
		if(k.eq.0) nl=nl-1
		!call movto2(xr,yr)
		call gDisplayStr(string1(l:NL))
	endif
  else
    call gSetCharFont(ifont1)
    call gSetStrJustify(justify)
	k=ichar(string1(nl:NL))
	if(k.eq.0) nl=nl-1
	call movto2(xr,yr)
	call gDisplayStr(string1(1:NL))
	!call gDisplayStr(string(1:nl))
  endif
endif
!x=x1
12	call gFlushGraphics() 
end