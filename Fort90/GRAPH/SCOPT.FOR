	subroutine SCOPT(iptype,nampmark,amark)
c For AUTPLOT -to add/override SC plots queued from SCAN
c
	real*4 amark(10)
	character ans*1
c
c Option to draw amplitude marker lines when in auto-mode
c
	nampmark=0
	call DCASK('Add amplitude marker lines to single channel plots',
     &   'N',ans)
	if(ans.eq.'Y') then
	   print 26,nampmark
26	   format(
     & ' Number of amplitude markers to be drawn on screen [',i2,'] = ')
	   call INPUTi(nampmark)
	   if(nampmark.gt.0) then
		do i=1,nampmark
		   x=amark(i)
		   print 27,i,x
27		   format(
     &		'&Amplitude #',i2,' (pA, with sign) [',f7.2,'] = ')
		   call INPUTr(x)
		   amark(i)=x			!x pA
		enddo
	   endif
	endif
c
	RETURN
	end


