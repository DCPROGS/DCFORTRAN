	subroutine TBLANK(string,n)
c Like LBLANK but moves text to end of string (rather than beginning)
	character string*(*)
c Looks for leading blanks in STRING and removes them. Looks up to
c position number n; posn of 1st non-blank character=n1
c
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
c
5	continue
	ntext=n2-n1+1
	j=n-ntext		!eg ntext=3, n=10, text goes in 8,9,10
	do 3 i=n1,n2
	j=j+1
3	string(j:j)=string(i:i)
c Fill first part with blanks
	do 6 i=1,n-ntext
6	string(i:i)=char(32)
c
	RETURN
	END


