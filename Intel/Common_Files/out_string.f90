subroutine write_string(string,x,y,angle,justify,ifont,textsize,icol1,dxs,dys)

use gino_f90
integer ifont,ifont1
real x,y
character*(*) string
character*200 string1
logical bold,italik,underline,nolabx
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue 

! dxs,dys scale factors for viewport 
width=textsize*dxs
height=textsize*dys
if(angle.eq.90.) then
	width=textsize*dys
	height=textsize*dxs
endif
call gStartTextBlock(x,y)
call glincols(icol1,idev)
call gsetmixedchars()
x1=x
!  first deal with fonts : I have to chamge size for any device and type of font -although I maintain the 
!  same proportion h/w on any device output
if(idev.eq.0) then ! display
	if(ifontrue.eq.109) then
		ifont1=ifont
	
		call gSetCharSize(width,height)
			call gsetmixedchars()
	else ! true fonts
		ifont1=ifontrue
	
		height1=1.45*height
		width1=0.9*width
		call gSetCharSize(width1,height1)
		!	call gsethardchars()
	endif	
else 
	if ((ifontrue.ge.100.and.ifontrue.le.108).or.(ifontrue.ge.150.and.ifontrue.le.170)) then ! true fonts
		call gsethardchars()
		ifont1=ifontrue
		if(idev.gt.0.and.idev.lt.3) then ! wmf,bmp
			height1=1.25*height
			width1=0.9*width
		else if(idev.ge.3) then  ! printer
			width1=0.9*width
			height1=1.45*height
		endif
		call gSetCharSize(width1,height1)
	else 
	    ifont1=ifont
		call gsetmixedchars()
		call gSetCharSize(width,height)
	endif

endif

call gSetStrAngle(angle)
!call gSetFontWeight(0)
!call gSetStrJustify(justify)
nl=nblank1(string)
icount=0
do i=1,nl
	string1(i:I)=' '
	string1(i:I)=string(i:i)
enddo
do i=1,nl-1
	if(string1(i:I+1).eq.'//') then
		string1(i:I+1)='£N'
	else if(string1(i:i).eq.'*') then
	endif
enddo
x1=x
call gSetCharFont(ifont1)
	call gSetStrJustify(justify)
	k=ichar(string1(nl:NL))
	if(k.eq.0) nl=nl-1
	call gDisplayStr(string1(1:NL))
	goto 11
! here I have to chase greek characters,etc in my string - if I use TT fonts and to split
! the string in substrings -and if it is greek to reverse to sof fonts,etc
if(nl.gt.0) then
  
  if(nl.gt.5.and.ifont1.ge.100) then
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
  call gStartTextBlock(x1,y)
   l=1
   m=1
   i=1
 
  do while(i.le.nl-5)
   
	if(string1(i:I+4).eq.'£F010') then
		icount=icount+1
		if(i.ge.2) then
		call gsethardchars()
		call gSetCharFont(ifont1)
		call movto2(x1,y)
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
				call movto2(x1,y)
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
	    call gsethardchars()
		call gSetCharFont(ifont1)
	
		k=ichar(string1(nl:NL))
		if(k.eq.0) nl=nl-1
	
		call gDisplayStr(string1(1:NL))

	else
		call gsethardchars()
		call gSetCharFont(ifont1)
		k=ichar(string1(nl:NL))
		if(k.eq.0) nl=nl-1
		call gDisplayStr(string1(l:NL))
	endif
    else
    call gSetCharFont(ifont1)
	call gSetStrJustify(justify)
	k=ichar(string1(nl:NL))
	if(k.eq.0) nl=nl-1
	call gDisplayStr(string1(1:NL))
    endif
endif

11	call gFlushGraphics() 
end