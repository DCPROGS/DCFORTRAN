	subroutine MARK(infit,indfst,ndisp,ixfprev,ix,icol,ibk,
     &  cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	integer*2 ix(2048),ix1,ix2
	logical cjump,onscreen
c
c To draw 'up to here' marker in SCAN, in colour-icol
c   Modified 01/13/95 03:03pm to also draw the tzero marker if (a) cjump
c is true, and (b) it is on screen.
c
c infit=abs index of last transition fitted -where arrow to be drawn
c indfst=abs index of first point on display
c ixfprev=x position where previous marker drawn (so erased correctly)
c ndisp=number of points on display
c If called with ixprev<0 then draws new arrow ONLY (colour=icol)
c ibk=background colour
c
c At present have
c	yd1=127.	!lowest point on screen (Y pixels) for display of channels
c	yd2=457.	!highest point on screen (Y pixels) for display of channels
c so arrow from 445 to 380 OK for channels opening downwards
c Invisible call to
c	subroutine ARROWiv(ix1,iy1,ix2,iy2,alpha,id,ic)
c in main prog defines arrow head for faster routine:
c	subroutine VARROW(ix,iy1,iy2,ic)
c
c Erase previous mark (if one was drawn)
	if(ixfprev.ge.0) then
c	   call VLINE(ixfprev,150,450,ibk)	 !-erase
c	   call ARROWiv(ixfprev,445,ixfprev,380,0.3,20,ibk)
	   call VARROW(ixfprev,445,380,ibk)
	endif
c
c NB must check not only that infit is on screen, but also that we
c are in the jump (#ijfit) in which last fit was done.
	onscreen=infit.ge.indfst.and.infit.le.indfst+ndisp-1
	if(cjump) onscreen=onscreen.and.njump.eq.njfit
	if(onscreen) then
	   index=infit-indfst+1
	   ix1=ix(index)
c	   call VLINE(ix1,150,450,icol)	 !-mark last trigger point
c	   call ARROWiv(ix1,445,ix1,380,0.3,20,icol)
	   call VARROW(ix1,445,380,icol)
	   ixfprev=ix1
	else
	   ixfprev=-1	!no marker drawn, so nothing to erase next time
	endif
c
	if(cjump) goto 1
	RETURN
c
c Now section for tzero marker
1	continue
c Erase previous mark (if one was drawn)
	if(ix0prev.ge.0) then
	   call VLINE(ix0prev,180,430,ibk)	 !-erase
	   ix2=ix0prev+ijprev
	   if(ix2.le.639) then
	      call HLINE(ix0prev,ix2,430,ibk)
	      call VLINE(ix2,430,415,ibk)
	   else
	      call HLINE(ix0prev,ix2,430,ibk)
	   endif
c==	   call WRSTRING18(ix0prev-8,440,'t=0',ibk,ibk) !surely should be this!!
	   call WRSTRING18(ix0prev-8,440,'t=0',ibk,14)
	endif
	if(intzero.ge.indfst.and.intzero.le.indfst+ndisp-1) then	!on screen
	   ijlen=ifixr(tjlen/float(ndisp))	!jump length in pixels
	   index=intzero-indfst+1
	   ix1=ix(index)
	   call VLINE(ix1,180,430,14)	 !-mark tzero in yellow
	   ix2=ix1+ijlen
	   if(ix2.le.639) then
	      call HLINE(ix1,ix2,430,14)
	      call VLINE(ix2,430,415,14)
	   else
	      call HLINE(ix1,ix2,430,14)
	   endif
c write 't=0'
	   ix2=ix1-8	!posn to start 't=0'
	   if(ix2.lt.0) ix2=0
	   call WRSTRING18(ix2,440,'t=0',14,ibk)
	   ix0prev=ix1
	   ijprev=ijlen
	else
	   ix0prev=-1	!no marker drawn, so nothing to erase next time
	endif
c
	RETURN
	end


