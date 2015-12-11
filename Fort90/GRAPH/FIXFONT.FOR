      subroutine FIXFONT(string,isize,ifont)
      character*1 fnt,fnti,sze
      character*80 string,string1
c	logical ctrlF,ctrlS
c In Hgraph there is a problem if we want to redraw text ( in 'str')
c usiing WRITESTRING or JUSTIFYSTRING, if the string contains ^F or
c ^S instructions (inserted e.g.during response to READSTRING) (see
c test prog THGLAB.FOR). If either of these codes occurs then when
c string is redrawn the characters start with a size/font dictated
c by the LAST ^S,^F encountered in the string, rather than with
c (a) the size specified by ISIZE in the call, or (b)font in force
c at time of call.
c This subroutine fixes problem by searching STRING for ^S,^F instructions
c and, if any found, then the string is prefixed with extra ^Sn (n=isize
c in call), and/or with ^Fn (n=ifont in call)
c MODIF: best to put ^S and ^F in front anyway- e.g. if greek next written
c  and then next label is roman (and contains no ^F) it may come out
c in greek when redrawn. Also incorp of ^S and ^F at start means that there
c is no need to set size or font explicitly before printing or redrawing?
c ^F=6; ^S=19
c IFONT must be on 0-11 scale
c ISIZE=1-9
	fnt=char(6)		!font (0-11) (see p57)
	sze=char(19)   	!character size (1-9)
	if(ifont.le.9) fnti=fnt//char(ifont+48)
	if(ifont.eq.10) fnti=fnt//'A'
	if(ifont.eq.11) fnti=fnt//'B'
	string1=string
c	ctrlF=.false.
c	ctrlS=.false.
c
c	do 1 i=1,80
c	j=ichar(string(i:i))
c	if(j.eq.0.or.j.eq.32) goto 2		!skip out if null or blank found
c	if(j.eq.6) ctrlF=.true.
c	if(j.eq.19) ctrlS=.true.
c	if(ctrlF.and.ctrlS) goto 2	!no need to look further
c1	continue
c
c2	continue
c	if(ctrlF) string=fnti//string
c	if(ctrlS) string=sze//char(isize+48)//string
	string=fnti//sze//char(isize+48)//string1
c
	RETURN
	end



