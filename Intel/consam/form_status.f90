subroutine form_status(iall,itape,nchan,Edit2_2,Edit2_3,Edit2_3a,Edit2_4,Edit2_5,&
Edit2_7,Edit2_8,Edit2_12,Edit2_13,Edit2_13a,Edit2_14,Edit2_15,Edit2_17,Edit2_18,toggle1_9,&
toggle1_8)
use menu_f90
integer :: Edit2_2 
integer :: Edit2_3a 
integer :: Edit2_3 
integer :: Edit2_4 
integer :: Edit2_5
integer :: Edit2_6 
integer :: Edit2_7 
integer :: Edit2_8 
integer :: Edit2_9 

integer :: Edit2_12 
integer :: Edit2_13a 
integer :: Edit2_13 
integer :: Edit2_14 
integer :: Edit2_15
integer :: Edit2_16 
integer :: Edit2_17 
integer :: Edit2_18
integer toggle1_8(5),toggle1_9(3) 
		   if(itape.eq.0) then	
		   	call gmSetToggleSwitch(Toggle1_8(3),GOn)
			call gmSetToggleSwitch(Toggle1_8(4),GOFF)
		   if(iall.eq.100) then
		   call gmSetToggleSwitch(Toggle1_9(1),GON)
		   	call gmSetToggleSwitch(Toggle1_9(2),GOFF)
			call gmSetToggleSwitch(Toggle1_9(3),GOFF)
		   if(nchan.eq.2) then
		   call gmSetToggleSwitch(Toggle1_8(2),GOn)
			call gmSetToggleSwitch(Toggle1_8(1),GOFF)
		   
		    call gmSetWidgetStatus(Edit2_2,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		
		   else
		   call gmSetToggleSwitch(Toggle1_8(1),GOn)
			call gmSetToggleSwitch(Toggle1_8(2),GOFF)
		   
			call gmSetWidgetStatus(Edit2_2,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		  
		    endif
		   else if(iall.eq.0) then
		   call gmSetToggleSwitch(Toggle1_9(2),GON)
		   	call gmSetToggleSwitch(Toggle1_9(1),GOFF)
			call gmSetToggleSwitch(Toggle1_9(3),GOFF)
		   if(nchan.eq.2) then
		   call gmSetToggleSwitch(Toggle1_8(2),GOn)
			call gmSetToggleSwitch(Toggle1_8(1),GOFF)
		   
		    call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		   else
		   call gmSetToggleSwitch(Toggle1_8(1),GOn)
			call gmSetToggleSwitch(Toggle1_8(2),GOFF)
		   
			call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		  
		    endif
		  
	
		   else  if(iall.eq.-1) then
		   call gmSetToggleSwitch(Toggle1_9(3),GON)
		   	call gmSetToggleSwitch(Toggle1_9(1),GOFF)
			call gmSetToggleSwitch(Toggle1_9(2),GOFF)
		   if(nchan.eq.2) then
		   call gmSetToggleSwitch(Toggle1_8(2),GOn)
			call gmSetToggleSwitch(Toggle1_8(1),GOFF)
		   
		    call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		   else
		   call gmSetToggleSwitch(Toggle1_8(1),GOn)
			call gmSetToggleSwitch(Toggle1_8(2),GOFF)
		   
			call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		  
		    endif
		  
		   endif
		   else
		   call gmSetToggleSwitch(Toggle1_8(3),GOff)
			call gmSetToggleSwitch(Toggle1_8(4),GOn)
		   
		   if(iall.eq.100) then
		   call gmSetToggleSwitch(Toggle1_9(1),GON)
		   	call gmSetToggleSwitch(Toggle1_9(2),GOFF)
			call gmSetToggleSwitch(Toggle1_9(3),GOFF)
		   if(nchan.eq.2) then
		   call gmSetToggleSwitch(Toggle1_8(2),GOn)
			call gmSetToggleSwitch(Toggle1_8(1),GOFF)
		   
		    call gmSetWidgetStatus(Edit2_2,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GSELECTABLE)
		   else
			call gmSetToggleSwitch(Toggle1_8(1),GOn)
			call gmSetToggleSwitch(Toggle1_8(2),GOFF)
		   
			call gmSetWidgetStatus(Edit2_2,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		  
		    endif
		   else if(iall.eq.0) then
		   call gmSetToggleSwitch(Toggle1_9(2),GON)
		   	call gmSetToggleSwitch(Toggle1_9(1),GOFF)
			call gmSetToggleSwitch(Toggle1_9(3),GOFF)
		   if(nchan.eq.2) then
		   call gmSetToggleSwitch(Toggle1_8(2),GOn)
			call gmSetToggleSwitch(Toggle1_8(1),GOFF)
		   
		    call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GSELECTABLE)
		   else
		   call gmSetToggleSwitch(Toggle1_8(1),GOn)
			call gmSetToggleSwitch(Toggle1_8(2),GOFF)
		   
			call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,gSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		  
		    endif
		  
	
		   else  if(iall.eq.-1) then
		   call gmSetToggleSwitch(Toggle1_9(3),GON)
		   	call gmSetToggleSwitch(Toggle1_9(1),GOFF)
			call gmSetToggleSwitch(Toggle1_9(2),GOFF)
		   if(nchan.eq.2) then
		   call gmSetToggleSwitch(Toggle1_8(2),GOn)
			call gmSetToggleSwitch(Toggle1_8(1),GOFF)
		   
		    call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GSELECTABLE)
		   else
		   call gmSetToggleSwitch(Toggle1_8(1),GOn)
			call gmSetToggleSwitch(Toggle1_8(2),GOFF)
		   
			call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_3a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_4,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_7,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_13a,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_14,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_15,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_17,GunSELECTABLE)
			call gmSetWidgetStatus(Edit2_18,GunSELECTABLE)
		  
		    endif
		  
		   endif
		   endif
end