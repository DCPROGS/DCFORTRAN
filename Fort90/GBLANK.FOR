	subroutine GBLANK(string,n,n1,n2)
	character string*(*)
c Looks up to position number n in string. Returns n1,n2=positions
c of the 1st and last nonblank characters (see also LBLANK,TBLANK)
c In original version n1,n2 were undefined if STRING was entirely
c blank. Now returns 0,0 in this case
c
	n1=0
	n2=0
	do 1 i=1,n
	ival=ichar(string(i:i))
	if(ival.eq.0.or.ival.eq.32) goto 1
	n1=i       !first non-blank character found- jump out
	goto 2
1	continue
c
2	continue
c Find LAST non-blank position
	do 4 i=n,1,-1
	ival=ichar(string(i:i))
	if(ival.eq.0.or.ival.eq.32) goto 4
	n2=i
	goto 5
4	continue
C
5	continue
	RETURN
	END


