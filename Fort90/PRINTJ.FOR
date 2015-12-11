	subroutine PRINTJ(j,titled,setx,hdisp,setvar,pprt,nj,w,xobs,yobs)
c To print data for set #j in CVFIT
c
	real YOBS(100,10),XOBS(100,10),W(100,10),setx(10)
	integer nj(10)
	character*60 TITLED(10)		!data set names
	logical hdisp,pprt,discprt,setvar
	common/dp/discprt
c
	print 1092,j,titled(j)
c	pause '1'
	if(pprt) write(7,1092) j,titled(j)
      if(discprt) write(8,1092) j,titled(j)
1092	FORMAT('  Data set # ',i3,3x,a60)
	if(setvar) then
	   print 334,setx(j)
	   if(pprt) write(7,334) setx(j)
	   if(discprt) write(8,334) setx(j)
334	   format( ' Set variable= ',g13.6)
	endif
	if(.not.hdisp) then
	   print 39
c	   pause '2'
	   if(pprt) write(7,39)
	   if(discprt) write(8,39)
39	   format(
     & ' Obs #       X value      Y value           s(Y)       weight')
	   do 141 i=1,nj(j)
	   if(w(i,j).gt.1.e-37) then
		sd=sqrt(1.0/w(i,j))
	   else
		sd=1.e36
	   endif
	   print 1091,I,XOBS(I,J),YOBS(I,J),sd,w(i,j)
c	   pause '3'
	   if(pprt)write(7,1091)I,XOBS(I,J),YOBS(I,J),sd,w(i,j)
	   if(discprt)write(8,1091)I,XOBS(I,J),YOBS(I,J),sd,w(i,j)
1091	   FORMAT(I4,3X,4(2X,G13.6))
141	   continue
	else			!for histo disp
	   print 391
	   if(pprt) write(7,391)
	   if(discprt) write(8,391)
391	   format(
     & ' Obs #       X value')
	   do 1131 i=1,nj(j)
	   print 140,I,XOBS(I,J)
	   if(pprt)write(7,140)I,XOBS(I,J)
	   if(discprt)write(8,140)I,XOBS(I,J)
140	   FORMAT(i4,5x,g13.6)
1131	   continue
	endif
	RETURN
	end

