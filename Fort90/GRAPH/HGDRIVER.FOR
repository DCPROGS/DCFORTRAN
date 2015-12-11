	subroutine HGDRIVER(iplot,colplot)
c To find out which plotter driver is installed for Hgraph
c Modif 12/20/93 05:36pm for new version which has title line: skip the
c first line
c Returns:
c iplot=1 for laserjet/deskjet
c iplot=2 for HP550C
c iplot=3 for paintjet
c iplot=4 for NECP67
c iplot=5 for POST/CPOST/HPGL
c (read first 2 lines to identify unambiguously)
	character ch,string*60
	character*20 string1(2)		!for 1st 2 lines
	logical present,colplot
c
	INQUIRE(file='\hgraph\raster.cfg',exist=present,flen=len)
	if(.not.present) then
c	   call BELL(3)
	   print 1
1	   format(' HGRAPH PLOTTER DRIVER (RASTER.CFG) NOT PRESENT')
	else
         OPEN(unit=31,file='\hgraph\raster.cfg',status='OLD',
     &     access='TRANSPARENT')
	   read(31,rec=1) string
	   CLOSE(unit=31)
c       Look for CR,LF (ival=13,10) to separate lines, and
c	  remove spaces from strings (the .CFG files are inconsistent
c	   about whether there is a space each side of = sign)
c Modif 12/20/93 05:36pm for new version which has title line: skip the
c first line
	   ival=0
	   i=0
	   do while (ival.ne.10)
		i=i+1
		ch=string(i:i)
		ival=ichar(ch)
	   enddo
	   i1=i+1	!first character on second line
c
	   j=0
	   n=NBLANK(string)
	   line=1
c	   do i=i,n
	   do i=i1,n
		ch=string(i:i)
		ival=ichar(ch)
		if(ival.eq.10) then
		   j=0
		   line=line+1
		   if(line.gt.2) goto 5		!keep only ist 2 lines for now
		endif
		if(ival.ne.32.and.ival.ne.0.and.
     &		ival.ne.13.and.ival.ne.10) then
		   j=j+1
		   string1(line)(j:j)=string(i:i)
		endif
	   enddo
c
5	   continue
	   iplot=0
	   colplot=.false.
	   if(string1(1)(1:13).eq.'FILE=PCL4.RAS') then
	      if(string1(2)(1:8).eq.'DPIH=180') then
		   iplot=3			!paintjet
		   colplot=.true.
	      else if(string1(2)(1:8).eq.'DPIH=300') then
		   iplot=1			!laserjet/deskjet
		   colplot=.false.
		endif
	   else if(string1(1)(1:13).eq.'FILE=PCL5.RAS') then
		iplot=2			!HP550C
		colplot=.true.
	   else if(string1(1)(1:15).eq.'FILE=EPS24C.RAS') then
		iplot=4			!NECP67
		colplot=.false.
	   else if(string1(1)(1:11).eq.'HORRES=1000') then
		iplot=5			!POST/CPOST/HPGL
		colplot=.false.
	   endif
	endif
c
	RETURN
	end



