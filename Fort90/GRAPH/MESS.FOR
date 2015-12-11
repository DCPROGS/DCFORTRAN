	call gino
      call vga
        call errswi(-1)	! ERRSWI(-1) switches the display of errors off
        call movto2(0.,0.)
        call linto2(500.,20.)
        call errchk
        call movto2(20.,20.)
        call chastr('hello*&')
        call errchk
	  pause
        call devsus
        stop
        end
c
        subroutine errchk
        integer ilist(12)
        call errenq(ilist,1,icount) ! ERRENQ allows you to enquire the most recent error/warning number.
        write(*,5)ilist(1)
        if(ilist(1).eq.-33)write(*,10)
5       format('          Error number = ',i4)
10      format('          This is an error 33')
        return
        end
