	subroutine SUBVEC(Vin,A,Vout,kin,kout)
c Analogue of SUBMAT to get submatrix from a 1-dimensional vector, Vin(kin)
c (for vectors declared as (1,10) or (10,1) can use SUBMAT)
C	A=1 integer for the part to be extracted
C 1,2,3,4= codes for A,B,C,D resp. 5=F=(B+C). 6=E=(A+B). 7=T=(B+C+D).
c 8=H=(C+D), 9=G=(A+B+C)
	real*8 Vin(kin),Vout(kout)
	integer A
	COMMON/KBLK/kA,kB,kC,kD
c
	do 1 i=1,kout
1	Vout(i)=0.0d0
c
	kE=kA+kB
	kG=kA+kB+kC
	k=kA+kB+kC+kD
c
	nc=A
	goto (101,102,103,104,105,106,107,108,109),nc	!ASSIGN COLS
101	NC1=1		!A
	NC2=KA
	GOTO 200
102	NC1=KA+1	!B
	NC2=KE
	GOTO 200
103	NC1=KE+1	!C
	NC2=KG
	GOTO 200
104	NC1=KG+1	!D
	NC2=K
	GOTO 200
105	NC1=KA+1	!=F SECTION=(B+C)  (=E for BCHAN2)
	NC2=KG
	GOTO 200
106	NC1=1		!=E SECTION=(A+B)
	NC2=KE
	GOTO 200
107	NC1=KA+1	!=T SECTION=(B+C+D) (=G for BCHAN2)
	NC2=K
	GOTO 200
108	nc1=ka+kb+1	!=H SECTION=(C+D) (=F for BCHAN2)
	nc2=ka+kb+kc+kd
	goto 200
109	nc1=1
	nc2=kG	!section G=A+B+C in C&H 1982
c	goto 200
C
200	continue
	do 300 j=nc1,nc2
300	Vout(j-nc1+1)=Vin(j)
	RETURN
	end


