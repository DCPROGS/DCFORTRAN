	subroutine draw_lin(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & csize,idev,xabs1,yabs1,xabs2,yabs2,rlth,
     & xmin,xmax,ymin,ymax,zoomed,iver)
c===================================================================

	character*30  defolt,cans
	real rlth(100)	!for line thickness
	character ch,cnum1*11,ans,UC
	real*4	xle(10),xlb(10),yle(10),ylb(10)
	real*4	xhle(10),xhlb(10),yhline(10)
	real*4	xvline(10),yvle(10),yvlb(10)
	integer ilvtype(10),ilhtype(10)
	integer*1 ivlinrel(10),ihlinrel(10)
	integer*4 icol(100),iltype(10)
	logical pon,logx,logy,sqrty,mono,discprt,relval,delete
      logical slock,zoomed
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi

	common/logval/logx,logy,sqrty
	COMMON/cols/icol,mono
	common/dp/discprt

	pon()=slock()
	ict=11
	if(ikey.eq.0) then
	if(nline.gt.0) then
	   do 222 i=1,nline
	   if(idev.ge.7) then
	      call LINWID(2*rlth(i+60))
	   else
	      call LINWID(rlth(i+60))
	   endif
c	   if(.not.mono)
         call lincols(icol(i+60),idev)	!colour for line(i)
	   call BROKEN(iltype(i))
	   xbeg=xlb(i)
	   xend=xle(i)
 	   ybeg=ylb(i)
 	   yend=yle(i)
c	   if(iver.eq.1100) then
	   continue
	   if(logx) then
	     xb=xlb(i)
	     xe=xle(i)
	     t1=abs(0.1*xb)
	     t2=abs(0.1*xe)
	     if(t1.eq.0) t1=0.001
	     if(t2.eq.0) t2=0.001
555	     if(xb.gt.0) then
              xbeg=alog10(xb)
	     else
		  xb=xb+t1
		  goto 555
	     endif
556	     if(xe.gt.0) then
		  xend=alog10(xe)
	     else
		  xe=xe+t2
		  goto 556
	     endif
	   endif
	   if(logy) then
	     yb=ylb(i)
	     ye=yle(i)
	     t1=abs(0.1*yb)
	     t2=abs(0.1*ye)
	     if(t1.eq.0) t1=0.001
	     if(t2.eq.0) t2=0.001
557 	     if(yb.gt.0) then
		  ybeg=alog10(yb)
	     else
		  yb=yb+t1
		  goto 557
	     endif
559 	     if(ye.gt.0) then
              yend=alog10(ye)
	     else
		  ye=ye+t2
		  goto 559
	     endif
 	   endif
	   if(sqrty) then
	     yb=ylb(i)
	     ye=yle(i)
	     t1=abs(0.1*yb)
	     t2=abs(0.1*ye)
	     if(t1.eq.0) t1=0.001
	     if(t2.eq.0) t2=0.001
560 	     if(yb.gt.0) then
              ybeg=sqrt(yb)
	     else
		  yb=yb+t1
		  goto 560
	     endif
561 	     if(ye.gt.0) then
              yend=sqrt(ye)
	     else
		  ye=ye+t2
		  goto 561
	     endif
         endif
c	   endif
c	   if (zoomed) then
	      call clipping(xabs1,yabs1,xabs2,yabs2,
     &      xbeg,ybeg,xend,yend,iflag)
		if (iflag.eq.0) goto 222
c	   endif
	   call gramov(xbeg,ybeg)
	   call gralin(xend,yend)
222	   continue
	endif
	if(nhline.gt.0) then
	   do i=1,nhline
	     if(idev.ge.7) then
	      call LINWID(2*rlth(i+71))
	     else
	      call LINWID(rlth(i+71))
	     endif
c	     if(.not.mono)
           call lincols(icol(i+71),idev)		!colour for line(i)
	     xbeg=xhlb(i)
	     xend=xhle(i)
	     yhl =yhline(i)
c	   if(iver.eq.1100) then
	     continue
	     if(logx) then
		  xb=xhlb(i)
		  xe=xhle(i)
	        t1=abs(0.1*xb)
	        t2=abs(0.1*xe)
	     if(t1.eq.0) t1=0.001
	     if(t2.eq.0) t2=0.001
666		  if(xb.gt.0) then
			xbeg=alog10(xb)
		  else
		     xb=xb+t1
		     goto 666
		  endif
667		  if(xe.gt.0) then
			xend=alog10(xe)
		  else
		     xe=xe+t2
		     goto 667
		  endif
	     endif
	     if(logy) then
		  y=yhline(i)
	        t=abs(0.1*y)
	     if(t.eq.0) t=0.001
668		  if(y.gt.0) then
			yhl=alog10(y)
		  else
		     y=y+t
		     goto 668
		  endif
	     endif
	     if(sqrty) then
		  y=yhline(i)
	        t=abs(0.1*y)
	     if(t.eq.0) t=0.001
669 		  if(y.gt.0) then
			yhl=sqrt(y)
		  else
		     y=y+t
		     goto 669
		  endif
           endif
c	     endif
c	   if (zoomed) then
	      call clipping(xabs1,yabs1,xabs2,yabs2,
     &      xbeg,yhl,xend,yhl,iflag)
		if (iflag.eq.0) goto 223
c	   endif
	     call BROKEN(ilhtype(i))
	     call graMOV(xbeg,yhl)
	     call graLIN(xend,yhl)
223        continue
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
	     if(idev.ge.7) then
	      call LINWID(2*rlth(i+81))
	     else
	      call LINWID(rlth(i+81))
	     endif
c	     if(.not.mono)
           call lincols(icol(i+81),idev)		!colour for line(i)
	     ybeg=yvlb(i)
	     yend=yvle(i)
	     xvl=xvline(i)
c	   if(iver.eq.1100) then
	     if(logy) then
		if(yvlb(i).gt.0)  ybeg=alog10(yvlb(i))
		if(yvle(i).gt.0)  yend=alog10(yvle(i))
	     endif
	     if(sqrty) then
		  if(yvlb(i).gt.0) ybeg=sqrt(yvlb(i))
		  if(yvle(i).gt.0) yend=sqrt(yvle(i))
	     endif
	     if(logx) then
		  if(xvline(i).gt.0) xvl=alog10(xvline(i))
	     endif
c	     endif
c	   if (zoomed) then
	      call clipping(xabs1,yabs1,xabs2,yabs2,
     &      xvl,ybeg,xvl,yend,iflag)
		if (iflag.eq.0) goto 224
c	   endif
	     call BROKEN(ilvtype(i))
	     call graMOV(xvl,ybeg)
	     call graLIN(xvl,yend)
224        continue
	   enddo
	endif
	else if(ikey.eq.2) then             !add freehand line
	   if(nline.eq.10) then
	     call WDIALOG(1,'Sorry ,no more lines',ict)
	     goto 221
	   endif
	   nline=nline+1			!count lines
	   call INTCONV(nline,cnum1)
	   call WDIALOG(1,'Line #'//CHARNB(cnum1)
     &	//': define start and end points with cursors',ict)
	   call CURPOS(vxhi/2,vyhi/2) !start with cursor central
	   call CURSOR(ikey,x1,y1)
         ch=char(ikey)		!define start of line
	   call jSYMBOL(x1,y1,9,csize,4,idev)   !mark bottom left with red diamond
	   call CURSOR(ikey,x2,y2)
         ch=char(ikey)		!define start of line
	   call jSYMBOL(x2,y2,9,csize,4,idev)   !mark bottom left with red diamond
	   i=nline
	   call spagra(x1,y1,xlb(i),ylb(i))
	   call spagra(x2,y2,xle(i),yle(i))
	   call lincols(icol(61+i),idev)
	   call graMOV(xlb(i),ylb(i))
	   call BROKEN(iltype(i))
	   call graLIN(xle(i),yle(i))
	   if(logx) then
	    if(xle(i).gt.0)	xle(i)=10**xle(i)
	    if(xlb(i).gt.0)	xlb(i)=10**xlb(i)
	   endif
	   if(logy) then
		if(yle(i).gt.0)	yle(i)=10**yle(i)
		if(ylb(i).gt.0)	ylb(i)=10**ylb(i)
	   endif
	     if(sqrty) then
		if(ylb(i).gt.0)	ylb(i)=ylb(i)**2
		if(yle(i).gt.0)	yle(i)=yle(i)**2
	     endif
	else if(ikey.eq.3.or.ikey.eq.4) then     !add horizontal/vert line
	   if(ikey.eq.3) then
	   if(nhline.eq.10) then
	     call WDIALOG(1,'Sorry ,no more horizontal lines',ict)
	     goto 221
	   endif
		nhline=nhline+1
	      call INTCONV(nhline,cnum1)
	   else
	   if(nhline.eq.10) then
	     call WDIALOG(1,'Sorry ,no more vertical lines',ict)
	     goto 221
	   endif
		nvline=nvline+1
	      call INTCONV(nvline,cnum1)
	   endif
	   call WDIALOG(1,'Horizontal line # '//charnb(cnum1)//':',ict)
	   iopt=1
	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,'Specify (1) with cursor (2) numerically;',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
	   call WDIALOG(1,'-Hit ENTER/ESC when done: F1 for help',ict)
	   if(iopt.eq.1) then
	    if(ikey.eq.3) then	!horizontal
		val=0.5*(ymin+ymax)
		eps=0.003*val
		if(sqrty) val=sqrt(val)
		amin=xmin
		amax=xmax
		call SETLINE(.true.,val,eps,6,2,amin,amax,icol(nhline+71),6,
     &	70,icol(71),delete,ik)     !format=F6.2 at present
		if(delete) goto 221	!should not use delete here!
		if(logy) val=10**val
		if(sqrty) val=val**2
		if (logx) then
		   amin=10**amin
		   amax=10**amax
		endif
		yhline(nhline)=val		!record y value
		xhlb(nhline)=amin        !start/end of HLINE
		xhle(nhline)=amax
	      if(pon()) write(7,35) nhline,val
	      if(discprt) write(8,35) nhline,val
35		format(' Horizontal line # ',i3,' added at y = ',g13.6)
	    else if(ikey.eq.4) then	!vertical
		val=0.5*(xmin+xmax)
		eps=0.001*val
		amin=ymin
		amax=ymax
		if(sqrty) then
		amin=sqrt(ymin)
		amax=sqrt(ymax)
		endif
		call SETLINE(.false.,val,eps,6,2,amin,amax,icol(nvline+81),6
     &	,70,icol(71),delete,ik)     !format=F6.2 at present
		if(delete) goto 221
		if(logx) val=10**val
		if (logy) then
		   amin=10**amin
		   amax=10**amax
		endif
		if (sqrty) then
		   amin=amin**2
		   amax=amax**2
		endif
		xvline(nvline)=val		!record x value
		yvlb(nvline)=amin          !start/end of HLINE (or define with cursor?)
		yvle(nvline)=amax
	      if(pon()) write(7,351) nvline,val
	      if(discprt) write(8,351) nvline,val
351		format(' Vertical line # ',i3,' added at x = ',g13.6)
	    endif
	   else if(iopt.eq.2) then	!specify numerically
		a1=xmin
		a2=xmax
		b1=ymin
		b2=ymax
		if(logx) then
		   a1=10**a1
		   a2=10**a2
		else if(logy) then
		   b1=10**b1
		   b2=10**b2
		else if (sqrty) then
		   b1=b1**2
		   b2=b2**2
		endif
	      if(ikey.eq.3) then	!horizontal
		   relval=.false.
		   if(nhline.gt.1) then
			ans='Y'
	   		call DEFOLTa(ans,defolt)
			call QDIALOG(1,' Specify RELATIVE to line 1',
     &		 defolt,ict,cans)
			call GETINPa(cans,ans)
			relval=UC(ans).eq.'Y'
			if(relval) then
			   ihlinrel(nhline)=1
			else
			   ihlinrel(nhline)=0
			endif
		   endif
		   yopt = b1
	   	   call DEFOLTr(yopt,defolt)
		   call QDIALOG(1,' Y value for horizontal line ',
     &	   defolt,ict,cans)
		   call GETINPr(cans,yopt)
		   if(relval) then
			yhline(nhline)=yopt+yhline(1)
			ilhtype(nhline)=2
		   else
			yhline(nhline)=yopt
		   endif
		   xhlb(nhline)=a1    !start/end of HLINE (or define with cursor?)
		   xhle(nhline)=a2
c=new
		   ans='Y'
	   	   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,' Specify X values at ends of line too',
     &		 defolt,ict,cans)
		   call GETINPa(cans,ans)
		   if(ans.eq.'Y') then
			call DEFOLT2r(a1,a2,defolt)
			call QDIALOG(1,'Xlow, Xhigh',defolt,ict,cans)
			call GETINP2r(cans,xhlb(nhline),xhle(nhline))
		   endif
		   xbeg=xhlb(nhline)
		   xend=xhle(nhline)
		   yopt=yhline(nhline)
		   if(logx) then
	            xbeg=alog10(xhlb(nhline))
                  xend=alog10(xhle(nhline))
		   endif
		   if(logy) yopt=alog10(yhline(nhline))
		   if(sqrty) yopt=sqrt(yhline(nhline))
               i=nhline
c		   if(.not.mono)
               call lincols(icol(i+71),idev)		!draw it
		   call BROKEN(ilhtype(i))
		   call graMOV(xbeg,yopt)
		   call graLIN(xend,yopt)
	         if(pon()) write(7,35) nhline,y
	         if(discprt) write(8,35) nhline,y
		   if(relval) then
	            if(pon()) write(7,36)
	            if(discprt) write(8,36)
36			format(' (relative to line 1)')
		   endif
	      else if(ikey.eq.4) then	!vertical
		   relval=.false.
		   if(nvline.gt.1) then
			ans='Y'
	   		call DEFOLTa(ans,defolt)
			call QDIALOG(1,' Specify RELATIVE to line 1',
     &		 defolt,ict,cans)
			call GETINPa(cans,ans)
			relval=UC(ans).eq.'Y'
			if(relval) then
			   ivlinrel(nvline)=1
			else
			   ivlinrel(nvline)=0
			endif
		   endif
		   x=a1
	   	   call DEFOLTr(x,defolt)
		   call QDIALOG(1,' X value for vertical line ',
     &	defolt,ict,cans)
		   call GETINPr(cans,x)
		   if(relval) then
			xvline(nvline)=x + xvline(1)
			ilvtype(nvline)=2
		   else
			xvline(nvline)=x
		   endif
		   yvlb(nvline)=b1    !start/end of VLINE (or define with cursor?)
		   yvle(nvline)=b2
c=new
		   ans='Y'
	   	   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,' Specify Y values at ends of line too',
     &		 defolt,ict,cans)
		   call GETINPa(cans,ans)
		   if(ans.eq.'Y') then
			call DEFOLT2r(b1,b2,defolt)
			call QDIALOG(1,
     &	 'Ylow, Yhigh',
     &   		defolt,ict,cans)
			call GETINP2r(cans,yvlb(nvline),yvle(nvline))
		   endif
c= end new

	            ybeg=(yvlb(nvline))
                  yend=(yvle(nvline))
		      x=(xvline(nvline))
		   if(logy) then
	            ybeg=alog10(yvlb(nvline))
                  yend=alog10(yvle(nvline))
		   endif
		   if(sqrty) then
	            ybeg=sqrt(yvlb(nvline))
                  yend=sqrt(yvle(nvline))
		   endif
		   if(logx) x=alog10(xvline(nvline))
		   i=nvline
c		   if(.not.mono)
		call lincols(icol(i+81),idev)		!draw it
		   call BROKEN(ilvtype(i))
		   call graMOV(x,ybeg)
		   call graLIN(x,yend)
	         if(pon()) write(7,351) nvline,x
	         if(discprt) write(8,351) nvline,x
		   if(relval) then
	            if(pon()) write(7,36)
	            if(discprt) write(8,36)
c36			format(' (relative to line 1)')
		   endif
	      endif
	   endif !option
	endif
221   end
c===================================================================
