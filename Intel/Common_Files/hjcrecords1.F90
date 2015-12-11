module hjcrecords

    PARAMETER NUMPOINTS=20480
	PARAMETER nump=100
	PARAMETER NaSETS=20

	type par3d
		logical all
		logical cross
		logical fill
		logical fillbad
		logical mark
		logical posneg
		logical inter
		logical bad(100,100)
		integer ifram
		integer istyle(6)
		integer isup(6)
		real ratio
		real rat
		real radius
		real theta
		real phi
		real z(100,100)
	end type par3d

	type (par3d) g3dpar

	TYPE JUMP
	
		INTEGER NCJUMP
		INTEGER NVJUMP
		REAL T1C(10)
		REAL T2C(10)
		REAL T1V(10)
		REAL T2V(10)
		REAL XOFF1
		REAL Y1V
		REAL Y2V
		REAL Y1C
		REAL Y2C
		LOGICAL IVPLOT

	END TYPE JUMP
	TYPE (JUMP) JUMPS

	TYPE AXES 
		INTEGER ITX
		INTEGER ITY
		INTEGER NTX
		INTEGER NTY
		INTEGER NX1
		INTEGER NY1
		integer numaxi
		integer nwidth
		integer npow
		integer nplace
		REAL XTIC
		REAL YTIC
		REAL XCROSS
		REAL YCROSS
		REAL TLENX
		REAL TLENY
		LOGICAL DOFRAME
		logical calbarx
		logical calbary

	END TYPE AXES
	TYPE (AXES) PARAM_AXIS

	
	TYPE NUMBER
		INTEGER NUMBX
		INTEGER NUMBY
		INTEGER INUMX
		INTEGER INUMY
		INTEGER INTXY
		integer ntext
		END TYPE NUMBER
	TYPE (NUMBER) NUMBERS

	
	TYPE GENERAL_ATTRIBUTES
		INTEGER IFNT0(100)
		INTEGER IFNT(100)
		INTEGER IJUS(100)
		INTEGER IDRAW(250)
		INTEGER ICOL(250)
		INTEGER ITYPE(250)
		REAL SIZETEXT(100)
		REAL ANGLE(100)
		REAL THICK(250)
		REAL RX(100)
		REAL RY(100)
		REAL RXBOX(4,100)
		REAL RYBOX(4,100)
	END TYPE GENERAL_ATTRIBUTES
	TYPE (GENERAL_ATTRIBUTES) ATTRIBUTES

	TYPE LINE
		INTEGER NARROW
		INTEGER NLINE
		INTEGER NHLINE
		INTEGER NVLINE
		REAL XBEG(50)
		REAL YBEG(50)
		REAL XEND(50)
		REAL YEND(50)
	END TYPE LINE
	TYPE (LINE) LINES
	

	TYPE RECORD_ATTRIBUTES
		LOGICAL hdisp
		CHARACTER*200 STR(100)
		integer iplot
		integer iplotype
		INTEGER IXP
		INTEGER IYP
		INTEGER IPOS
		integer ILOG
		INTEGER	NUMSETS
		INTEGER NCURVd
		INTEGER NCURVC
		INTEGER NSFIT
		INTEGER	ICURVd(NaSETS)
		INTEGER	ICURVw(NaSETS)
		INTEGER	jmiss(NaSETS)
		INTEGER NdAt(NaSETS)
		INTEGER NJ(NaSETS)
		INTEGER JUSE(NaSETS)
		integer isym(nasets)
		INTEGER ijoin(nasets)
		INTEGER NCAL(NaSETS)
		integer iline(nasets)
		INTEGER	ICURVC(NaSETS)
		INTEGER NDV1
		INTEGER NDIMD
		INTEGER NDC1
		INTEGER NDIMC
		integer kwi
		integer kwj
		REAL XMIN
		REAL YMIN
		REAL XMAX
		REAL YMAX
		REAL XMIN1
		REAL YMIN1
		REAL XMAX1
		REAL YMAX1
		REAL XMIN10
		REAL YMIN10
		REAL XMAX10
		REAL YMAX10
		REAL WXMIN
		REAL WYMIN
		REAL WXMAX
		REAL WYMAX
		real dxs
		real dys
		REAL X0
		REAL Y0
		REAL YINF
		REAL XVAL(NUMPoints,NaSETS)
		REAL YVAL(NUMPoints,NaSETS)
		real w(numpoints,nasets)
		REAL XCAL(NUMPOINTS,NaSETS)
		REAL YCAL(NUMPOINTS,NaSETS)
		real symsiz(nasets)
		TYPE (AXES) PARAM_AXIS
		TYPE (NUMBER) NUMBERS
		TYPE (LINE) LINES
		TYPE (GENERAL_ATTRIBUTES) ATTRIBUTES
		TYPE (JUMP) JUMPS
		type (par3d) g3dpar
	END TYPE RECORD_ATTRIBUTES
	
	TYPE CALCULATED_CURVE
	     REAL XCAL(NUMPOINTS,NaSETS)
		REAL YCAL(NUMPOINTS,NaSETS)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
		REAL X0
		REAL Y0
		REAL YINF
		INTEGER NCAL(NaSETS)
		INTEGER	ICURVC(NaSETS)
		INTEGER NCURVC
		integer ILOG
	END TYPE CALCULATED_CURVE

end module hjcrecords