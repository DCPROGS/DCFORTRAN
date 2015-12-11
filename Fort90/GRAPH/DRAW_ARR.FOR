	subroutine draw_arr(ikey,narrow,xe,xb,ye,yb,ixfix,iyfix,
     &xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,rlth,zoomed,iver,ihead)
c========================================================================
	character*30  defolt,cans
	real rlth(100)	!for line thickness
	character ch,cnum1*11
	real*4	xe(10),xb(10),ye(10),yb(10)
	integer*4   icol(100)
	logical logx,logy,sqrty,mono,zoomed
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/logval/logx,logy,sqrty
	COMMON/cols/icol,mono

	ict=11
	icmono=15		!colour for mono (laserjet etc)
	call broken(0)
	if(ikey.eq.0.and.narrow.gt.0) then
	  do 222 i=1,narrow
	   call LINWID(rlth(i+50))
	   if(mono) then 
c		call lincols(15,idev)
		call lincol(icmono)
	   else
		call lincols(icol(50+i),idev)
	   endif
c	   if(mono) then
c	      call lincols(0,idev)
c	   else
c		call lincols(icol(i+50),idev)
c	   endif
	   xbeg=xb(i)
	   xend=xe(i)
 	   ybeg=yb(i)
 	   yend=ye(i)
	   if (zoomed) then
	      call clipping(xabs1,yabs1,xabs2,yabs2,
     &      xbeg,ybeg,xend,yend,iflag)
		if (iflag.eq.0) goto 222
	   endif
66	   continue
c	   if(iver.eq.1100) then
	   if(logx) then
		xbeg=alog10(xbeg)
		xend=alog10(xend)
	   endif
 	   if(logy) then
 		ybeg=alog10(ybeg)
 		yend=alog10(yend)
 	   endif
	   if(sqrty) then
 		ybeg=sqrt(ybeg)
 		yend=sqrt(yend)
         endif
c	   endif
	   call graMOV(xbeg,ybeg)
	   call arrow(xend,yend,ihead,1)
222	  continue
	endif
	if((ikey.eq.6.or.ikey.eq.11).and.narrow.gt.0) then		!delete
2354	   continue
	   if(ikey.eq.11) then
            k=1
		goto 777
	   endif
	   call INTCONV(narrow,cnum1)
	   nc=nblank1(cnum1)
	   iopt=1
	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,
     &'Delete arrow number (1 to '//cnum1(1:nc)//')',defolt,
     & ict,cans)
	   call GETINPi(cans,k)
	   if(k.le.0.or.k.gt.narrow) goto 2354
777	   if(logx) then
		xend=alog10(xe(k))
		xbeg=alog10(xb(k))
	   endif
	   if(logy) then
		yend=alog10(ye(k))
		ybeg=alog10(yb(k))
	   endif
	   if(sqrty) then
		ybeg=sqrt(yb(k))
		yend=sqrt(ye(k))
	   endif
	   call lincols(icol(71),idev)
	   call gramov(xbeg,ybeg)
         call ARROW(xend,yend,ihead,1)
	   if(ikey.eq.11) goto 888
	   j=0
	   do i=1,narrow
	      if(i.ne.k) then	!skip deleted one
		   j=j+1
		   xb(j)=xb(i)
		   yb(j)=yb(i)
		   xe(j)=xe(i)
		   ye(j)=ye(i)
		endif
	   enddo
	   narrow=narrow-1
	   endif
888	   if(ikey.eq.1.or.ikey.eq.11) then
	   narrow=narrow+1			!count arrows
	   if(ikey.eq.11) narrow=1
	   call INTCONV(narrow,cnum1)
	   nc=nblank1(cnum1)
	   call WDIALOG(1,'Arrow #'//cnum1(1:nc)
     &	//': define start and end points with cursors',ict)
	   call CURPOS(vxhi/2,vyhi/2) !start with cursor central
	   call cursor(ikey,x1,y1)
         ch=char(ikey)		!define start of arrow
	   call jSYMBOL(x1,y1,9,csize,4,idev)   !mark bottom left with red diamond

	   call cursor(ikey,x2,y2)
         ch=char(ikey)		!define start of arrow
	   call jSYMBOL(x2,y2,9,csize,4,idev)   !mark bottom left with red diamond
	   i=narrow
	   if(mono) then 
c		call lincols(15,idev)
		call lincol(icmono)
	   else
		call lincols(icol(50+i),idev)
	   endif
         call spagra(x1,y1,xb(i),yb(i))
         call spagra(x2,y2,xe(i),ye(i))

	   if(ixfix.eq.1) xb(i)=xv       ! x specified
	   if(ixfix.eq.1) xe(i)=xv
	   if(iyfix.eq.1) yb(i)=yv       ! y specified
	   if(iyfix.eq.1) ye(i)=yv
c Call to ARROW includes colour (with Hgraph numbering)
	   call gramov(xb(i),yb(i))
         call ARROW(xe(i),ye(i),ihead,1)
	   if(logx) then
		xe(i)=10**xe(i)
		xb(i)=10**xb(i)
	   endif
	   if(logy) then
		ye(i)=10**ye(i)
		yb(i)=10**yb(i)
	   endif
	   if(sqrty) then
		yb(i)=yb(i)**2
		ye(i)=ye(i)**2
	   endif
	   ixfix=0		!clear again
	   iyfix=0		!clear again
	endif

	end

c
