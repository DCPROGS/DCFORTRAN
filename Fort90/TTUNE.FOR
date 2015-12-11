	program TTUNE
	integer C,DF,D,EF,E,F,GF,G,AF,A,BF,B
	common/note/C,DF,D,EF,E,F,GF,G,AF,A,BF,B
	integer if(2,10)
c To test Spindrift TUNE
	call DEFNOTE
c
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
	call TUNE(if,n)
	end

