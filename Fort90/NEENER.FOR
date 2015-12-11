	subroutine NEENER()
	integer if(2,10)
	integer C,DF,D,EF,E,F,GF,G,AF,A,BF,B,C1
	common/note/C,DF,D,EF,E,F,GF,G,AF,A,BF,B,C1
	logical nobeep
	common/quiet/nobeep

	if(nobeep) return
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
	if(1,n)=E		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=A		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=0		!=freq
	if(2,n)=3		!=duration
	n=n+1
	if(1,n)=G		!=freq
	if(2,n)=32		!=duration
	n=n+1
	if(1,n)=E		!=freq
	if(2,n)=32		!=duration
c
	call TUNE(if,n)
c
	RETURN
	end
