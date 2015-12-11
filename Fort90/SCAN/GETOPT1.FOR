	subroutine GETOPT1(ixin1,iyin1,ixup1,iyup1,n1,pchar1,
     & txtwin1,ictxt1,nout1,achar,ibkwin1,inc,inca,ndisp,
     & ibk,end,stopped,backward,ilabel)
c========add 'baseline done' to menu if NOT sdone
c
c To separate menu options done when KBHIT in SCAN (ACHAR=input)
c 'window' option (part screen for expand mode) removed for now
	integer*2 ktype
	integer*2 txtwin1(ixin1:ixup1,iyin1:iyup1)
	character*1	achar,getch
	character*1 pchar1(1:n1)
	logical end,backward,stopped
c
	idest=0	!clear any value left from last call
c
	call MENU_OPTIONS(nout1,achar,pchar1,iret)
	select case(iret)
		case(1)  		!Roll on
			backward=.false.
			inc=inca
			call FILLWIN(16,iyin1,624,iyup1,ibkwin1)
			call FILLWIN(320,iyin1,472,iyup1,3)
			call FILLWIN(16,iyin1,320,iyin1+16,3)
			call FILLWIN(16,iyup1-16,168,iyup1,15)
			call write_text(txtwin1,ixin1,iyin1,ixup1,
     &   				iyup1,ictxt1)
		case(2)		!Increase speed
			if(inca.lt.(ndisp-16)) then
				inca=inca+16
				inc=inca
				if(backward) inc=-inca
			endif
		case(3)
		case(4)
		case(5)           !Go back
			call FILLWIN(16,iyin1,624,iyup1,ibkwin1)
			call FILLWIN(320,iyin1,472,iyup1,3)
			call FILLWIN(16,iyin1,320,iyin1+16,3)
			call FILLWIN(16,iyin1+32,168,iyin1+48,15)
			call write_text(txtwin1,ixin1,iyin1,ixup1,
     &				iyup1,ictxt1)
			end=.false.
			backward=.true.
			inc=-inca
		case(6)        	!Decrease speed
			if(inca.ge.32) then
				inca=inca-16
				inc=inca
				if(backward) inc=-inca
			endif
		case(7)
		case(8)           !Stop
c			goto 25
			stopped=.true.
			idest=25		!wait for new option
			goto 99	!RETURN
		case(9)		!Expand=screen2
			idest=800		!expand mode now at label 800
			goto 99
c			call screen2(ixin2,iyin2,ixup2,iyup2,text2
c     &	    		,TXTWIN2,iFWIN2,iCTXT2,n2,
c     &			IDATA,is,ndisp,inabs,nrlast,nsec,
c     &			iybar1,icbar1,iybar2,icbar2,ibk,
c     &			expfac,isec,pch,0,
c     &			nbuf,novlap)
c		      call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
c			call hline(1,639,iybar1,icbar1)
c			call hline(1,639,iybar2,icbar2)
c			call hline(1,639,iybar3,icbar3)
c			goto 22

		case(10)
		case(11)
		case(12)		!quit
c			goto 8
			idest=8
			goto 99	!RETURN
		case(13)
		case(14)
		case(15)         !Window
c		   	call screen2(ixin2,iyin2,ixup2,iyup2,text2
c     &	     		,TXTWIN2,iFWIN2,iCTXT2,n2,
c     &			IDATA,is,ndisp,inabs,nrlast,nsec,
c     &			iybar1,icbar1,iybar2,icbar2,ibk,
c     &			expfac,isec,pch,ixin2-8,
c    &			nbuf,novlap)
c		      call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
c			call hline(1,639,iybar1,icbar1)
c			call hline(1,639,iybar2,icbar2)
c			call hline(1,639,iybar3,icbar3)
c			goto 22
		case(16)          !help
			call first_help(ibk)
			idest=30
			goto 99	!RETURN
	end select
c
c
99	continue
c Code the idest for simplicity on exit
	if(idest.eq.0) then
	   ilabel=0	  !if no 'goto' specified above, so straight on in main prog
	else if(idest.eq.8) then
	   ilabel=1
	else if(idest.eq.25) then
	   ilabel=2
	else if(idest.eq.30) then
	   ilabel=3
	else if(idest.eq.800) then
	   ilabel=4
	endif
c
	RETURN
	end

