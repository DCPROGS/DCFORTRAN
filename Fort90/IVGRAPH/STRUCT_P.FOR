c=======================================================================

	subroutine struct_par(str,csfac1,nstr,irx,iry,ixjust,iyjust,icols,
     &	iangles,ifnts,parval,xtitle,ytitle,title1,cnumx,cnumy,
     &	cexpx,cexpy,newtext,numbx,numby,ntext,inumx,inumy,logx,logy,
     &	csfac,rx,ry,ixjus,iyjus,icol,iangle,ifnt,idraw,jdraw,iflag,
     &      csize)

c=======================================================================

	integer IXJUS(100),IYJUS(100),idraw(100),jdraw(100)
	integer IXJUSt(100),IYJUSt(100)
	integer iangle(100),icol(100)
	integer iangles(100),icols(100),ifnts(100)
	integer ifnt(30),irx(100),iry(100)
	real 	  csfac(30),csfac1(50)
	real    RX(100),RY(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*75 xtitle,ytitle	!output from LAXES
	character parval*150 		!to hold param values
	character*44 TITLE1
	character*80 newtext(20)		!extra text
	character*150 str(100)		!extra text
	LOGICAL logx,logy
	character*10  getint


	ic0=icol(24)
	icl=icol(23)
	ift0=ifnt(7)
	csf0=csfac(7)
	if(iflag.eq.1) then
	   nl=nblank1(parval)
	   str(1)=parval(1:nl)
	   icols(1)=icol(26)
	   ifnts(1)= ifnt(6)
	   iangles(1)=iangle(6)
	   csfac1(1)=csfac(6)
	   irx(1)=iscrx(rx(6))
	   iry(1)=iscry(ry(6))
	   ixjust(1)=ixjus(6)
	   iyjust(1)=iyjus(6)
	   jdraw(1)=idraw(6)
	   nl=nblank1(xtitle)
	   str(2)=xtitle(1:nl)
	   icols(2)=icol(23)
	   ifnts(2)=ifnt(8)
	   iangles(2)=iangle(8)
	   csfac1(2)=csfac(8)
	   irx(2)=iscrx(rx(8))
	   iry(2)=iscry(ry(8))
	   ixjust(2)=ixjus(8)
	   iyjust(2)=iyjus(8)
	   jdraw(2)=idraw(8)

	   nl=nblank1(ytitle)
	   str(3)=ytitle(1:nl)
	   icols(3)=icol(23)
	   ifnts(3)=ifnt(9)
	   iangles(3)=iangle(9)
	   csfac1(3)=csfac(9)
	   irx(3)=iscrx(rx(9))
	   iry(3)=iscry(ry(9))
	   ixjust(3)=ixjus(9)
	   iyjust(3)=iyjus(9)
	   jdraw(3)=idraw(9)

	   nl=nblank1(title1)
	   str(4)=title1(1:nl)
	   icols(4)=icol(25)
	   ifnts(4)=ifnt(10)
	   iangles(4)=iangle(10)
	   csfac1(4)=csfac(10)
	   irx(4)=iscrx(rx(10))
	   iry(4)=iscry(ry(10))
	   ixjust(4)=ixjus(10)
	   iyjust(4)=iyjus(10)
	   jdraw(4)=idraw(10)

	   do k=1,numbx
	      nl=nblank1(cnumx(k))
	      str(k+4)= cnumx(k)(1:nl)
c=============================================================
c!	to be fixed:
c	the best option is to use JUSTSTR.FOR to draw numbers:this imply to
c	change DRAWNUM  all over in the program;
c     for the moment I draw with IDRAWNUM  in FIXNUMB2) witch draws in the
c	same way as DRAWNUM  , but for F90 version please have a look at JUSTSTR.!
c	because in TEXTMOVE the number+exponent has to be treated as one string!
c	and DRAWNUM  writes 2 strings which can not be concatenated !!!
c     JUSTSTR concatenates cnum and cexp and gives you a resulting string!
c===========================================================================
	   	if(logx.and.inumx.eq.1) then
	    	   sizen=csfac(7)*csize
		   call SETCSIZE(0.8*sizen,indsize)	!this defines ISIZE
	         nl1=NBLANK1(cexpx)
	         str(k+4)=cnumx(k)(1:nl)//char(21)//char(19)
     &         //getint(indsize,nm)(1:nm)//cexpx(k)(1:nl1)
		   call SETCSIZE(sizen,indsize)	!reset
		endif
		icols(k+4)=icol(24)
		ifnts(k+4)=ifnt(7)
		csfac1(k+4)=csfac(7)
	    	irx(k+4)=iscrx(rx(k+30))
		iry(k+4)=iscry(ry(k+30))
		iangles(k+4)=iangle(k+30)
		ixjust(k+4)=ixjus(k+30)
		iyjust(k+4)=iyjus(k+30)
	      jdraw(k+4)=idraw(k+30)
	   enddo
	   do k=1,numby
	      nl=nblank1(cnumy(k))
		str(k+4+numbx)= cnumy(k)(1:nl)
c!	to be fixed:
	   	if(logy.and.inumy.eq.1) then
	    	   sizen=csfac(7)*csize
		   call SETCSIZE(0.8*sizen,indsize)	!this defines ISIZE
	         nl1=NBLANK1(cexpy)
	         str(k+4+numbx)=cnumy(k)(1:nl)//char(21)//char(19)
     &         //getint(indsize,nm)(1:nm)//cexpy(k)(1:nl1)
		   call SETCSIZE(sizen,indsize)	!reset
		endif
		icols(k+4+numbx)=icol(24)
		ifnts(k+4+numbx)=ifnt(7)
		csfac1(k+4+numbx)=csfac(7)
	    	irx(k+4+numbx)=iscrx(rx(k+55))
		iry(k+4+numbx)=iscry(ry(k+55))
		iangles(k+4+numbx)=iangle(k+55)
		ixjust(k+4+numbx)=ixjus(k+55)
		iyjust(k+4+numbx)=iyjus(k+55)
	      jdraw(k+4+numbx)=idraw(k+55)
	   enddo
	   nstr=4+numbx+numby
	   if(ntext.gt.0) then
	      do k=1,ntext
		   str(k+4+numbx+numby)= newtext(k)
		   icols(k+4+numbx+numby)=icol(k+10)
		   ifnts(k+4+numbx+numby)=ifnt(k+10)
		   csfac1(k+4+numbx+numby)=csfac(k+10)
	    	   irx(k+4+numbx+numby)=iscrx(rx(k+10))
		   iry(k+4+numbx+numby)=iscry(ry(k+10))
		   iangles(k+4+numbx+numby)=iangle(k+10)
		   ixjust(k+4+numbx+numby)=ixjus(k+10)
		   iyjust(k+4+numbx+numby)=iyjus(k+10)
	         jdraw(k+4+numbx+numby)=idraw(k+10)
	      enddo
	      nstr=nstr+ntext
	   endif
	else
	 parval=str(1)
	 icol(26)=icols(1)
	 ifnt(6)=ifnts(1)
	 iangle(6)=iangles(1)
	 csfac(6)=csfac1(1)
	 rx(6)=xworld(irx(1))
	 ry(6)=yworld(iry(1))
	 ixjus(6)=ixjust(1)
	 iyjus(6)=iyjust(1)
	 idraw(6)=jdraw(1)
	 if(icols(2).ne.icl) then
	 	icol(23)=icols(2)
	 else
	 	icol(23)=icols(3)
	 endif
	 xtitle=str(2)
	 ifnt(8)=ifnts(2)
	 iangle(8)=iangles(2)
	 csfac(8)=csfac1(2)
	 rx(8)=xworld(irx(2))
	 ry(8)=yworld(iry(2))
	 ixjus(8)=ixjust(2)
	 iyjus(8)=iyjust(2)
	 idraw(8)=jdraw(2)

	 ytitle=str(3)
	 ifnt(9)=ifnts(3)
	 iangle(9)=iangles(3)
	 csfac(9)=csfac1(3)
	 rx(9)=xworld(irx(3))
	 ry(9)=yworld(iry(3))
	 ixjus(9)=ixjust(3)
	 iyjus(9)=iyjust(3)
	 idraw(9)=jdraw(3)

	 title1=str(4)
	 icol(25)=icols(4)
	 ifnt(10)=ifnts(4)
	 iangle(10)=iangles(4)
	 csfac(10)=csfac1(4)
	 rx(10)=xworld(irx(4))
	 ry(10)=yworld(iry(4))
	 ixjus(10)=ixjust(4)
	 iyjus(10)=iyjust(4)
	 idraw(10)=jdraw(4)
	 if(icols(5).ne.ic0) then
		icol(24)=icols(5)
	 else
		icol(24)=icols(5+numbx)
	 endif
	 if(ifnts(5).ne.ift0) then
		ifnt(7)=ifnts(5)
	 else
		ifnt(7)=ifnts(5+numbx)
	 endif
	 if(csfac1(5).ne.csf0) then
		csfac(7)=csfac1(5)
	 else
		csfac(7)=csfac1(5+numbx)
	 endif
	 ifnt(7)=ifnts(5)
	 csfac(7)=csfac1(5)
	 do k=1,numbx
	   	if(.not.logx)  then
		   cnumx(k)=str(k+4)
		endif
	    	rx(k+30)=xworld(irx(k+4))
		ry(k+30)=yworld(iry(k+4))
		iangle(k+30)=iangles(k+4)
		ixjus(k+30)=ixjust(k+4)
		iyjus(k+30)=iyjust(k+4)
	      idraw(k+30)=jdraw(k+4)
	 enddo
	 do k=1,numby
	   	if(.not.logy)  then
		   cnumy(k)=str(k+4+numbx)
		endif
	    	rx(k+55)=xworld(irx(k+4+numbx))
		ry(k+55)=yworld(iry(k+4+numbx))
		iangle(k+55)=iangles(k+4+numbx)
		ixjus(k+55)=ixjust(k+4+numbx)
		iyjus(k+55)=iyjust(k+4+numbx)
	      idraw(k+55)=jdraw(k+4+numbx)
	 enddo
	 if(ntext.gt.0) then
	   j=1
	   do k=1,ntext
	      idraw(k+10)=jdraw(k+4+numbx+numby)
		if(idraw(k+10).ne.0) then
		   newtext(j)=str(k+4+numbx+numby)
		   icol(j+30)=icols(k+4+numbx+numby)
		   ifnt(j+10)=ifnts(k+4+numbx+numby)
		   csfac(j+10)=csfac1(k+4+numbx+numby)
		   rx(j+10)=xworld(irx(k+4+numbx+numby))
		   ry(j+10)=yworld(iry(k+4+numbx+numby))
		   iangle(j+10)=iangles(k+4+numbx+numby)
		   ixjus(j+10)=ixjust(k+4+numbx+numby)
		   iyjus(j+10)=iyjust(k+4+numbx+numby)
		   j=j+1
		endif
	   enddo
	   ntext=j-1
	 endif
	endif
      continue
	end

c=====================================================================
