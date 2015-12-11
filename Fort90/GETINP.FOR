c Set of subroutines to decode the ASCII output from QDIALOG
c (based in INPUTi etc)
c NB CANS must be declared at least char*12 for single input
c and at least char*25 for double input
c	subroutine GETINPi(Cans,n)
c	subroutine GETINP2i(Cans,n2,n2)
c	subroutine GETINPr(Cans,real)
c	subroutine GETINP2r(Cans,real1,real2)
c	subroutine GETINPa(Cans,ans)
c
	subroutine GETINPi(Cans,int)
c (based on INPUTi(int))
	character cans*(*)
	character ch*12,ch1*12
	logical digchar
c
	do i=1,12
	  ch(i:i)=CANS(i:i)
	  ch1(i:i)=char(32)
	enddo
c
c Define ch1 to exclude blanks and non-digit characters
	n=0
	do i=1,12
	   ival=ichar(ch(i:i))
	   if(digchar(ival).and.(ival.ne.32)) then
		n=n+1
		ch1(n:n)=ch(i:i)
	   endif
	enddo		!end of i loop
c if all blank, and/or non-digits, then n=0 so return with input value of INT
	if(n.ne.0) then
	   call CHTOREAL(ch1(1:n),real)
	   int=IFIXR(real)
	endif
	RETURN
	end



	subroutine GETINP2i(Cans,int1,int2)
c Based on INPUT2i(int1,int2)
c To input two integers (with or without decimal point!) separated
c by a comma.
c Modified 01/20/93 11:24pm to distinguish between input of 0 and
c input of <ENTER>.
c (1) Invalid characters and blanks are ignored
c (2) If no valid characters entered (eg ENTER is hit) then the input (default)
c	values of int1,int2 are returned unchanged
c (3) Entry of a single value, followed by comma or not, returns that value
c	as int1, and the default int2
c (4) If the first valid character is a comma, any valid characters following
c	it are returned as int2, with the default value of int1
c
	character cans*(*)
	character ch2*25,ch21*25,ch*12
	logical digchar
c
	do i=1,25
	  ch2(i:i)=CANS(i:i)
	  ch21(i:i)=char(32)
	enddo
	do i=1,12
	  ch(i:i)=char(32)
	enddo
c
c Return with input values of int1, int2 if ENTER hit, so ch2 blank
	if(ch2.eq.'                         ') RETURN
c
c Search for comma, and define CH21 as like CH2 but remove blanks and
c non-digits (but include the comma, if present, in ch21)
	icom=0		!not changed if no comma present
	n=0			!count valid characters
	do i=1,25
	   ival=ichar(ch2(i:i))
	   if((digchar(ival).or.ival.eq.44).and.(ival.ne.32)) then
		n=n+1
		ch21(n:n)=ch2(i:i)
		if(ival.eq.44) icom=n	!comma found at position icom in ch21
	   endif
	enddo		!end of i loop
c
c Blanks excluded from ch21 already, so if ch2 had all blanks before comma
c then ch21 has comma in position 1, and if ch2 had all blanks after comma
c then ch21 has comma in position n.
c If no comma found, or comma is at end (posn n in ch21) then treat
c all characters as int1
	if(icom.eq.0.or.icom.eq.n) then
	   ch=ch21(1:12)
	   if(ch.ne.'            ') then
		call CHTOREAL(ch,real1) !convert unless blank
		int1=IFIXr(real1)
	   endif
c If first character is comma then treat characters 2 to n in ch21 as REAL2
	else if(icom.eq.1) then
	   ch=ch21(2:n)
	   call CHTOREAL(ch,real2)
	   int2=IFIXr(real2)
c If comma is elsewhere, convert to get real1, real2 (unless blank)
	else
	   ch=ch21(1:icom-1)		!exclude comma
	   call CHTOREAL(ch,real1)
	   int1=IFIXr(real1)
	   ch=ch21(icom+1:n)		!exclude comma
	   call CHTOREAL(ch,real2)
	   int2=IFIXr(real2)
	endif
	RETURN
	end


	subroutine GETINPr(Cans,real)
c Based on INPUTr(real)
	character cans*(*)
	character CH*12,ch1*12
	logical digchar
c
	do i=1,12
	  ch(i:i)=cans(i:i)
	  ch1(i:i)=char(32)
	enddo
c
c Define ch1 to exclude blanks and non-digit characters
	n=0
	do i=1,12
	   ival=ichar(ch(i:i))
	   if(digchar(ival).and.(ival.ne.32)) then
		n=n+1
		ch1(n:n)=ch(i:i)
	   endif
	enddo		!end of i loop
c if all blank, and/or non-digits, then n=0 so return with input value of REAL
	if(n.ne.0) call CHTOREAL(ch1(1:n),real)
	RETURN
	end


	subroutine GETINP2r(Cans,real1,real2)
c Based on INPUT2r(real1,real2)
	character cans*(*),UC*1
	character ch2*25,ch21*25,ch*12
	logical digchar
c NB CANS must be declared at least char*25
	do i=1,25
	  ch2(i:i)=cans(i:i)
	  ch21(i:i)=char(32)
	enddo
	do i=1,12
	  ch(i:i)=char(32)
	enddo
c
c Return with input values of real1, real2 if ENTER hit, so ch2 blank
	if(ch2.eq.'                         ') RETURN
c Special catch (for interpolation option in VPLOT). If 'u' is entered
c then both values returned as 1.e35
	if(UC(cans(1:1)).eq.'U') then
	   real1=1.e35
	   real2=1.e35
	   RETURN
	endif
c
c Search for comma, and define CH21 as like CH2 but remove blanks and
c non-digits (but include the comma, if present, in ch21)
	icom=0		!not changed if no comma present
	n=0			!count valid characters
	do i=1,25
	   ival=ichar(ch2(i:i))
	   if((digchar(ival).or.ival.eq.44).and.(ival.ne.32)) then
		n=n+1
		ch21(n:n)=ch2(i:i)
		if(ival.eq.44) icom=n	!comma found at position icom in ch21
	   endif
	enddo		!end of i loop
c
c Blanks excluded from ch21 already, so if ch2 had all blanks before comma
c then ch21 has comma in position 1, and if ch2 had all blanks after comma
c then ch21 has comma in position n.
c If no comma found, or comma is at end (posn n in ch21) then treat
c all characters as REAL1
	if(icom.eq.0.or.icom.eq.n) then
	   ch=ch21(1:12)
	   if(ch.ne.'            ') call CHTOREAL(ch,real1) !convert unless blank
c If first character is comma then treat characters 2 to n in ch21 as REAL2
	else if(icom.eq.1) then
	   ch=ch21(2:n)
	   call CHTOREAL(ch,real2)
c If comma is elsewhere, convert to get real1, real2 (unless blank)
	else
	   ch=ch21(1:icom-1)		!exclude comma
	   call CHTOREAL(ch,real1)
	   ch=ch21(icom+1:n)		!exclude comma
	   call CHTOREAL(ch,real2)
	endif
c
	RETURN
	end

	subroutine GETINPa(Cans,ans)
c (based on DCASK) Returns input 'ans' unchanged if CANS is empty, otherwise
c returns UC(cans(1:1))
	character cans*(*),ans*1,UC*1
c
	n=NBLANK(cans)
	if(n.gt.0) then
	   ans=UC(cans(1:1))
	endif
	RETURN
	end



