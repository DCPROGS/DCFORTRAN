	subroutine BEETH5()
	integer if(2,10)
	integer C,DF,D,EF,E,F,GF,G,AF,A,BF,B
	common/note/C,DF,D,EF,E,F,GF,G,AF,A,BF,B
c To call Spindrift TUNE (must call DEFNOTE first to define common)
	n=1
	if(1,n)=G		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=0		!=freq
	if(2,n)=3		!=duration
	n=n+1
	if(1,n)=G		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=0		!=freq
	if(2,n)=3		!=duration
	n=n+1
	if(1,n)=G		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=0		!=freq
	if(2,n)=3		!=duration
	n=n+1
	if(1,n)=EF		!=freq
	if(2,n)=50		!=duration
c
	call TUNE(if,n)
c
	RETURN
	end

