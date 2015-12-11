subroutine ind_model(Main,imodel,models,isub)
use gino_f90
use menu_f90
integer isubpanel(20),isub(10,20),iopens(10,20),inum(10,20),istart(10,20)
	PARAMETER  N_STATES=200
	TYPE MODEL
		integer irecm
	    INTEGER N
		integer ka
		integer kb
		integer kstat
		integer kstat0
		integer nsub
		integer kcon
		integer ncon
		integer npar
		logical indmod
		logical chardef
		integer ix
		integer iy
		integer model
		character*80 title_model
		real X(N_STATES)
		real Y(N_STATES)
		INTEGER COLOUR(N_STATES)
		LOGICAL STATUS(N_STATES)
		LOGICAL DRAW(N_STATES)
		CHARACTER*3 NAME(N_STATES)
		character*15 statname(n_states)
		character*10 con_states(20)
		character*10 sub_states(10,20)
		character*10 open_states(10,20)
		character*10 start_states(10,20)
		integer	num_states(10,20)
		integer link(n_states,n_states)
		integer inter_link(n_states)
		character*40 name_link(n_states)
		character*20 ligname(10)
		integer nlig
		integer nbound(n_states,10)
		integer nchan
		real vref
		real*8 dgamma(n_states)
		integer nwidth
		integer nheight
		character*2 charmod(25,40)
		integer ilast
		integer jlast
		integer ic(2,200)
		integer index(n_states)
	END TYPE MODEL

	TYPE (MODEL) MODELS(25)

	iyw=models(imodel)%kstat0*models(imodel)%nsub+models(imodel)%nsub+2

	indwin = gmCreateComplexDialogueBox(Main, 5, 3,8,iyw, GALL, 'Basic States ', &
		gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gmiddle)
!	ind1=gmCreateTextEntry(indwin,1,1,3,1,'Name',10, Gdisplay, &
!						gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
!             			gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
!              			gmVpos=GTOP, gmExpand=GOFF)	
!	ind2=gmCreateTextEntry(indwin,1,1,8,1,'Open',10, Gdisplay, &
!						gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
!              			gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
!              			gmVpos=GTOP, gmExpand=GOFF)	
!	ind3=gmCreateTextEntry(indwin,1,1,10,1,'Start',10, Gdisplay, &
!						gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
!              			gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
!              			gmVpos=GTOP, gmExpand=GOFF)	
!	ind4=gmCreateTextEntry(indwin,1,1,12,1,'Number',10, Gdisplay, &
!						gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
!              			gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
!							gmVpos=GTOP, gmExpand=GOFF)	

		if (models(imodel)%nsub.gt.0) then
				
				do i=1,models(imodel)%nsub
					isubPanel(i)=gmCreatePanel(indwin, 0, (models(imodel)%kstat0+1)*(i-1),&
					8,models(imodel)%kstat0+1, &
              		gmHpos=GLEFT, gmVpos=Gtop, gmtitle='subunit',gmExpand=GOff, gmType=GNOBOUNDARY, &
              		gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
					do j=1,models(imodel)%kstat0
						models(imodel)%sub_states(i,j) = ' '
						isub(i,j)=gmCreateTextEntry(isubPanel(i),1,j,&
							6, 1,models(imodel)%sub_states(i,j), 60, Gedit, &
							gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              				gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              				gmVpos=GTOP, gmExpand=GOFF)
					!	iopen(i,j)=gmCreateTextEntry(isubPanel(i),1,8,&
					!		2, 1,models(imodel)%open_states(i,j), 60, Gedit, &
					!		gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              		!		gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              		!		gmVpos=GTOP, gmExpand=GOFF)
					!	istart(i,j)=gmCreateTextEntry(isubPanel(i),10,j,&
					!		2, 1,models(imodel)%start_states(i,j), 60, Gedit, &
					!		gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              		!		gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              		!		gmVpos=GTOP, gmExpand=GOFF)
					!		val=models(imodel)%num_states(i,j)
					!	inum(i,j)=gmCreateValueEntry(isubPanel(i),12,j,&
					!		2, 1,val,5,0,display=Gedit, &
					!		gmVpos=GTOP, gmExpand=GOFF)
					enddo
				enddo
				
         else
				
				i=1
				isubPanel(i)=gmCreatePanel(indwin, 0, 0,8,models(imodel)%kstat0+1, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle='subunit',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
				do j=1,models(imodel)%kstat0
				models(imodel)%sub_states(i,j)=' '
				isub(i,j) = gmCreateTextEntry(isubPanel(i),1, j, 6, 1,&
				models(imodel)%sub_states(i,j), 60, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
				!iopen(i,j)=gmCreateTextEntry(isubPanel(i),1,8,&
				!			2, 1,models(imodel)%open_states(i,j), 60, Gedit, &
				!			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	!			gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	!			gmVpos=GTOP, gmExpand=GOFF)
				!		istart(i,j)=gmCreateTextEntry(isubPanel(i),10,j,&
				!			2, 1,models(imodel)%start_states(i,j), 60, Gedit, &
				!			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	!			gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	!			gmVpos=GTOP, gmExpand=GOFF)
				!			val=models(imodel)%num_states(i,j)
				!		inum(i,j)=gmCreateValueEntry(isubPanel(i),12,j,&
				!			2, 1,val,5,0,display=Gedit, &
				!			gmVpos=GTOP, gmExpand=GOFF)
				enddo
		endif
		iqb=gmCreatePushButton(indwin,3,0, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=1122) !6505)
call gmdrawwindow(indwin)
end