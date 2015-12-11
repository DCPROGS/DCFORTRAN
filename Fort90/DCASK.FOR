	subroutine DCASK(question,default,answer)
c Universal question asker!
c Default should be 'n','N','y','Y' or ' ' (last means no default specified).
c Question should be, eg, 'Read from disk'
c Modif 10/15/95 11:36am so 'default' and 'answer' can be the same in call
c Modified 01/28/95 09:06am so that
c (1) ^C aborts
c Modified 02/10/94 03:59pm so that
c (1) If no valid default given then accepts only y,Y,n,N
c (2) If valid default given then
c	(a) 'enter' (only) returns default
c	(b) zero/ins key returns opposite of default
c	(c) y,Y,n,N as before
c	(d) no other keys accepted
c (note -UC does not change characters other than a-z)
c
	character*(*) question
c	character(*) question
	character*74 text
	character*1 default,answer,UC,ch,getch,ans
c	integer*2 ktype
	logical enter,zero
c
101	format(a1)
	if(.not.(default.eq.' '.or.UC(default).eq.'N'.or.
     &  UC(default).eq.'Y')) then
	   print 1
1	   format(' ARGUMENT ERROR IN DCASK')
	   default=' '
	endif
c
	n=NBLANK(question)
	if(default.eq.' ') then
	   text=question(1:n)//' ? '
	else
	   text=question(1:n)//' ['//default//'] ? '
	endif
c
	n=NBLANK(text)
	n=n+1		!blank after '?'
	print*,text(1:n)
c	read 101,answer
2	ch=GETCH(ktype)
	ans=UC(ch)
	ival=ICHAR(ch)
	if(ival.eq.3.and.ktype.eq.32) STOP		!^C
	enter=ktype.eq.4.and.ival.eq.13
c 'zero' = 0 or INS hit
	zero=(ktype.eq.3.and.ival.eq.48).or.(ktype.eq.8.and.ival.eq.82)
c
	if(default.eq.' ') then
	   if(ans.ne.'Y'.and.ans.ne.'N') then	!check it is Y/N
		goto 2
	   endif
	else if(UC(default).eq.'Y') then	!return Y for enter, y or Y
	   if(ans.eq.'Y'.or.enter) then
		answer='Y'
	   else if(ans.eq.'N'.or.zero) then
		answer='N'
	   else
		goto 2
	   endif
	else if(UC(default).eq.'N') then
	   if(ans.eq.'N'.or.enter) then
		answer='N'
	   else if(ans.eq.'Y'.or.zero) then
		answer='Y'
	   else
		goto 2
	   endif
	endif
c
	print 3,answer
3	format('& ',a1)
	RETURN
	end
