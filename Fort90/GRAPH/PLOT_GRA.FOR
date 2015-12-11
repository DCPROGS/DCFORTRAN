	subroutine plot_graph()
c=========================================================
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu
	common/hlp/help		!for QDIALOG
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,iangle,idraw,
     & ifnt,csfac,lth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel


	do 50 j1=1,ncurvd		!plot ncurvd data sets
	jflag=0
	j=icurvd(j1)		!data set to be plotted (col # where data starts)
	if(j.le.10) then
	   j0=j
	else
	   j0=1+mod(j-1,10)	!so j=11-20, 21-30 all -> j0=1-10 etc
	endif
	if(.not.mono) call lincols(icol(j0))	 !col for jth data set: symbols+line+SD
	linetype=ijoin(j)		!line to join data points
	iud=1				!draw line between points
	if(linetype.eq.-1) iud=0	!don't
	nint=0			!points for interpolation
c  Definitions to cope with ndat>2048; define ngr(j)=number of cols of
c Yval(i,j) occupied by each of the data graphs (all =1 in normal case
c where ndat(j)=<2048).  If number of points > 2048 then display only
c every ndelt-th point -must first check how many values lie within the
c current xmin,xmax,ymin,ymax range by call to COUNTDAT.
	if(ndat(j).le.2048.or.allpnt) then
	   ndelt=1
	else
	   ndelt=1
	   call COUNTDAT(j,ndat,ndelt,xval,yval,logx,logy,logity,sqrty,
     &    y0,yinf,ymin,ymax,x0,xmin,xmax,ndimd,NDISP,ndv1)
	   if(ndisp.gt.2048) then	!calc ndelt>1
		ndelt=1 + (ndisp-1)/2048
	   endif
	endif
	if(ndelt.gt.1) then
	   call INTCONV(ndelt,cnum1)
	   if(ndelt.eq.2) then
		cnum1(1:3)='2nd'
	   else if(ndelt.eq.3) then
		cnum1(1:3)='3rd'
	   else
		cnum1=CHARNB(cnum1)//'th'
	   endif
c	   call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	   call WDIALOG(2,'Every '//CHARNB(cnum1)//' point',12)
	endif
c
c NB 09/06/91 11:50am seem to get some problems with line thickness if have
c many points, and solvable if line thickness not changed for every point
c Change is only needed if want BOTH symbols (other than point=0) AND
c data points joined with line.  Mostly both are not needed and if so load
c line thickness here, before the 'do 215' loop for each point
	 tload=.true.					!thickness to be loaded in loop
	 if((iud.eq.1).and.(isym(j).eq.0)) then
	    if(idev.ge.5) then
	      call LINWID(2*lth(j0))
	    else
	      call LINWID(lth(j0))

	    endif
	    tload=.false.					!thickness already loaded
	 else if(isym(j).ne.0.and.ijoin(j).eq.-1) then
	    if(idev.ge.5) then
	      call LINWID(2*lth(27))
	    else
	      call LINWID(lth(27))
	    endif
	    tload=.false.					!thickness already loaded
	 endif
c Bit done now when itrace=2 on entry, to define ytsep() etc for
c multi-page plots
c     If called with itrace=2 then check separation of traces etc
c     and do multipage plots here -then return straight away
	if(itrace.eq.2) then
	   reclen=xval(ndat(1),1)-xval(1,1)	!length of trace
	   mulpage=.true.
	   call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),lth(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),
     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,3,
     &	xlo,xhi,ylo,yhi)
	   itrace=1		!so this bit not done again
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   RETURN
	endif
c
c Put do 215 loop + interp drawing into DRAWDAT, to draw curve # j
	   if(j.eq.1.and.ntrace.gt.1) then
	      call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),lth(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),
     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,2,
     &	xlo,xhi,ylo,yhi)
	   else
		icw=icurvw(j1)	!SD control for curve 1,2,....
	      call DRAWDAT5(j,xval,yval,ndelt,
     &       logity,logx,logy,sqrty,y0,yinf,x0,mono,
     &       xmin,xmax,ymin,ymax,iud,tload,idev,
     &       ndat(j),ijoin(j),icol(j0),isym(j),syms(j),lth(j0),lth(j0),
     &       lth(27),lth(27),lth(30),lth(30),barleng,interp,npint,
     &       Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd,
     &	 weight,kwi,kwj,icw)
	   endif
50	continue		!end of sets loop
end
c========================================================================

