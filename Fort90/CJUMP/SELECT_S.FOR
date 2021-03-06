	subroutine select_state(ivalve,mask,icount,str1)
c==========================================================================
c    Computer and 1401 link via plug cable(back).
c    output computer  : software control (TRIGGER INTERNALLY!)
c    correspond to
c    (external events): E0    E1     E2    E3    E4   Ext
c    Dig output 1401  : 8 	 9	 10	 11	 12	 13	 14	 15    
c    value            : 1      2      4     8    16    32    64   128
c    pin (front 1401) : 17     4     16     3    15     2    14     1
c    input decoder1/2 : A0     A1     A2   A3    INHIBIT 1/2
c    output decoder1/2: O0 to O25
c==========================================================================
c     COMPUTER         1401 outputs = Decoder1/2 inputs 	Decoder outputs
c---------------------------------------------------------------------------
c				8/A0   9/A1   10/A2  11/A3  INH1/2
c---------------------------------------------------------------------------
c                          0      0        0      0      0   no connection O0
c----------------------------------------------------------------------------
c										  Decoder 1
c----------------------------------------------------------------------------
c   DIGTIM,A,31,1,..	   1      0        0      0      0   	O1
C   DIGTIM,A,31,2,..       0      1        0      0	   0        02
C   DIGTIM,A,31,3,..       1      1        0      0      0        03
C   DIGTIM,A,31,4,..       0      0        1      0      0        04
C......................................................................
C   DIGTIM,A,31,15,..      1      1        1      1      0        15
C----------------------------------------------------------------------------
C										  DECODER 2
C----------------------------------------------------------------------------
C   DIGTIM,A,31,16,..      0      0        0      0       1       16
c   DIGTIM,A,31,17,..      1      0        0      0       1       17
C............................................................................
C   DIGTIM,A,31,25,..      1      0        0      1       1       25
C============================================================================
	character*60 fmt
	character*72 str1

	str1=' '
	istate=ivalve
	fmt='(a10,i2,a1,i2,a1,i6,a1)'
	write(str1,fmt) 'DIGTIM,A,',mask,',',istate,',',icount,';'

	end

C============================================================================
