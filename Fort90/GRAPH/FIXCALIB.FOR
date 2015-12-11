	subroutine FIXCALIB(titlex,xcalib)
c to change number in title (looks for digits 0-9 and replaces them
c with new length of calibration bar. If no digits found (eg labels
c specified in call to VPLOT as units only (eg titlex='ms', titley='pA')
c then number is inserted before all text
	character titlex*40,cnum*11,cpre*30,cpost*40
	integer*2 mark(40)
c	logical caplock
c
	do 8 i=1,40
8	mark(i)=0
	cpre='                             '
	cpost='                                  '
	n=NBLANK(titlex)
	ilast=0
	nd=0
	do 1 i=1,n
	   j=ichar(titlex(i:i))
	   if((j.ge.48.and.j.le.57).or.j.eq.46) then	!0 to 9 or dec point
		nd=nd+1		!# of digits
		mark(i)=1
		ilast=i
	   endif
1	continue
c Keep bit before numbers in CPRE
	i1=0
	if(nd.eq.0) goto 3	!no digits in label
	do 2 i=1,n
	   if(mark(i).eq.1) goto 3	!jump out
	   i1=i1+1
	   cpre(i1:i1)=titlex(i:i)
2	continue
c
c Keep bit after numbers in CPOST
3	continue
	i2=0
	do 4 i=ilast+1,n
	   i2=i2+1
	   cpost(i2:i2)=titlex(i:i)
4	continue
c
c Format the number as in MAKNUM
	nmax=2		!max number of sig figs after decimal point
	call FIXDEC1(xcalib,m0,n1,nmax)	!get no of digits
	call DCFORMAT(xcalib,m0+n1+3,n1,cnum)
	if=2
	if(m0.eq.0.and.xcalib.lt.0.0) if=1
	ns=NBLANK(cnum)	!do not insert ASCII 0
c next line omits decimal point for 'integers'
	if(n1.eq.0.and.ns.gt.1.and.cnum(ns:ns).eq.'.') ns=ns-1
	cnum=cnum(if:ns)
	nfig=ns-if+1
c
c	ndec=0
c	if(amod(xcalib,1.0).gt.0.001) ndec=1
c	xcalib=ROUND(xcalib,ndec)
c	nfig=1+ifix(alog10(abs(xcalib)+0.00001))
c	if(ndec.eq.1) nfig=nfig+2
c	if(nfig.lt.1) nfig=1
c	call REALTOCH(xcalib,cnum)
c	if(caplock()) then
c	   call LOCATE(5,0)
c	   print 700,xcalib,ndec,nfig,nd,i1,i2,ilast,cpre,cpost,mark
c700	format(' xcalib,ndec,nfig,nd,i1,i2,ilast = ',g13.6,6i5,/,
c     & ' cpre=',a30,' cpost=',a35,/,' mark=',40i2)
c	   pause
c	endif
c  replace numbers
	do 5 i=1,i1
5	   titlex(i:i)=cpre(i:i)
	do 6 i=1,nfig
	   j=i+i1
6	   if(j.le.40) titlex(j:j)=cnum(i:i)
	do 7 i=1,i2
	   j=i+i1+nfig
	   jlast=j
7	   if(j.le.40) titlex(j:j)=cpost(i:i)
	do 9 j=jlast+1,40
9	   titlex(j:j)=' '
c
	RETURN
	end

