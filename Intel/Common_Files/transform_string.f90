subroutine transform_string(text,strings)

character*(200) text,string(10),strl(10,10)
integer nextline(10)

  nl=nblank1(text)
  if(nl.gt.2) then
  
  i=1
  j=0
  k=1
  do while (i.le.nl-1)
	if(text(i:I+1).eq.'£N') then
		j=j+1
		string(j)=text(k:i-1)
		k=i+2
		i=k
     else
		i=i+1
	 endif
   enddo 
  
   nlines=j

   l=1
   m=1
   i=1
 
  do j=1,nlines
  nl=nblank1(string(j))
  do while(i.le.nl-2)
   
	if(string(j,i:I+1).eq.'£F'.and.string(j,i+2:I+2).ne.'R') then		
		if(i.ge.1) then
			strl(j,m)=string(j,l:i-1)
			ifont(j,m)=0
			call chatoint()
            ifont(j,m+1)=
			do while(j.le.nl-3)
				if(text(j:j+2).eq.'£FR') then
				call gsetmixedchars()
			    call gSetCharFont(4)
				call movto2(x1,y)
					call gSetStrJustify(-1)
				call gDisplayStr(text(i:j-1))
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
	
	endif
	
  enddo
  end