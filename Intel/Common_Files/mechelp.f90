subroutine mechelp(main,ihelp1,ki,icl)
	use menu_f90
      character*70  helps(50)
	integer ithg(50)
    character*10 ikon
	if(icl.eq.615) then

	select case(ki)
	 CASE(2)
	   helps(1)='Modify model'
	   helps(2)=&
        ' To modify the number of states you must '
		helps(3)=' choose the option "Mechanism" , "Display Old"'
	   helps(4)=&
        ' and "Modify" from the beginning or if you "Open" first the '
		helps(5)=' old model , press the relevant icon on the graph '
		helps(6)=' A new window will appear in which you are allowed to'
		helps(7)=' delete/add states and links'
		helps(8)=' When the icon "States" is pressed the changes are shown' 
		helps(9)= ' automatically keeping the unmodified values'
		helps(10)=' For rates you have to enter all the values from '
		helps(11)=' the beginning.'
	   nhelp=11
	   
	 case(3)
	   helps(1)=' Microscopic reversibility'
	   helps(2)=&
        ' On the "Rates" table press the relevant button'
		helps(3)=' A window will pop up with the old values'
	   helps(4)=&
        ' allowing to enter the changes'
		helps(5)=' The "Rates" table will be upDATEWd automatically'
	   
	   nhelp=5
	 case(4)
	   helps(1)=' State parameters'
	   helps(2)=&
        ' In the "States" table you can modify: names,conductances,'
		
	   helps(3)=&
       ' and ligands bound to each state'  
		helps(4)=' Do not forget to press "Save changes"'
	   nhelp=4
	 case(5)
	   helps(1)=' Rates parameters'
	   helps(2)=&
        ' In the "Rates" table you can modify: names,values,'
		helps(3)=' concentration dependancy and voltage '
	   helps(4)=&
       ' dependency'  
	   
		helps(5)=' Do not forget to press "Save changes"'
	   nhelp=5
	   
	
	end select
	else
    select case(ki)
	 CASE(1)
	   helps(1)='START'
	   helps(2)=&
        ' From the main menu choose "Mechanism" and then '
		helps(3)=' "Create new mechanism" '
	   helps(4)=&
        ' You will be asked first to choose the type of '
		helps(5)=' the mechanism you want to create'
	   nhelp=5
	   
	 case(2)
	   helps(1)=' NORMAL'
	   helps(2)=&
        ' Fill in the new name, number of open states,stut states,'
		helps(3)=' etc and press OK'
	   helps(4)=&
        ' When you finish, a graphic blue window will display'
		helps(5)=' the open states in red and the closed ones in blue'
	   helps(6)=&
	     ' At this stage you can add more states '
	   nhelp=6
	 case(3)
	   helps(1)=' INDEPENDENT'
	   helps(2)=&
        ' Fill in the new name, number of subunits,'
		helps(3)='states in subunits, concerted states,etc '
	   helps(4)=&
       ' After filling  the names of basic states in each subunit,'  
	   helps(5)=' a blue graphic window for each subunit will be displayed'
		helps(6)=&
	     ' At this stage you can add more states '
	   nhelp=5
	 case(4)
	   helps(1)=' Add states'
	   helps(2)=&
        ' For open states press on the icon " O "'
	
	  helps(3)=&
        ' For closed states press on the icon " C "'
	 	helps(4)=' and than click on the blue graphic window'
	   nhelp=4
	   
	 case(5)
	   helps(1)=' Make links between the states'
	   helps(2)=&
      ' Press on the icon " = " '
	   helps(3)=&
      ' and than on the states you want to link'
	  nhelp=3
	  
	  case(6)
	   helps(1)='View/edit states properties'
       helps(2)=&
      ' On the graphic blue window click on the icon "States"'
	   helps(3)=&
      ' which you will find on the right hand corner '
	   helps(4 )=&
      ' A table containing the state names & conductance,'
	  helps(5)=' number of links for each state and '
	  helps(6)='number of ligands bound to each state will appear '
	   helps(7 )=&
      ' After filling all the required fields DO NOT FORGET TO '
	   helps(8)=&
      ' PRESS THE BUTTON TO SAVE THE CHANGES'
	   helps(9)=' ALWAYS FILL THIS TABLE FIRST'

	  nhelp=9
	
	 
	 case(7)

		helps(1)='View/edit rates properties'
       helps(2)=&
      ' On the graphic blue window click on the icon "Rates"'
	   helps(3)=&
      ' which you will find on the right corner '
	   helps(4 )=&
      ' A tabel containing the rates properties will appear '
	  
	   helps(5 )=&
      ' After filling all the required fields DO NOT FORGET TO'
	   helps(6)=&
      ' PRESS THE BUTTON TO SAVE THE CHANGES'
	  nhelp=6

	
	 case(8)
	   helps(1)='Finalize the independent model '
	   helps(2)=' On the menu bar press on the icon " i "'
       helps(3)=' A blue window displaying the independent model'
	   helps(4)=' with the link between states will be displayed'
	   helps(5)=' For the state and rates properties follow the'
	   helps(6)=' instructions mentioned above'
	   nhelp=6
	   
	 case(9)
	 	helps(1)='Save'
       helps(2)=&
      ' On the graphic blue window click on the icon with the' 
	   helps(3)=&
      ' floppy disk sign which you will find on the right hand corner '
	  helps(4)=' A text file with the mechanism characteristics '
	  helps(5)=' will be generated automatically '
	  nhelp=5
       
	 case(10)
	   	helps(1)='Print'
       helps(2)=&
      ' On the graphic blue window click on the icon with the '
	   helps(3)=&
      ' printer sign which you will find on the right hand corner '
	  nhelp=3
       
	 nhelp=3
	end select

	endif
	ndhelp=nhelp+4
	if(ndhelp.gt.25) ndhelp=25
	ihelp1 = gmCreateComplexDialogueBox(Main, 14,2, 16, &
     ndhelp,gall, 'Curve Fitting Help ', gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
	ihelPanel=gmCreatePanel(ihelp1, 0, 0, 16, ndhelp-1, &
     gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON,gmType=GNOBOUNDARY,& 
     gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)

      	    call widmin(14,1)
	        call widmax(14,1)
	        CALL TEXATR(0,0,2,-1)
		    call texent(ihelpanel,1,1,helps(1),70,1,ithg(1))
	        CALL TEXATR(0,0,0,-1)
		    call texsiz(85)
		    do i=2,nhelp
		     call texent(ihelpanel,1,1+i,helps(i),70,1,ithg(I))
		    enddo
           
		    call texsiz(100)
		  
	!	call gFlushGraphics()
call gmdrawwindow(ihelp1)
	end

