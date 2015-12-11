	subroutine LBLANK(string,n)
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
c Problem with moving characters n1 to n to start of array is that
c the original characters may not all be overwritten
5	continue
	j=0
	do 3 i=n1,n2
	j=j+1
3	string(j:j)=string(i:i)
c Fill rest with blanks
	ntext=n2-n1+1
	do 6 i=ntext+1,n
6	string(i:i)=char(32)
c
	RETURN
	END


