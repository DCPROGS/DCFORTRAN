Subroutine GetCommandLineArguments( plotfile_name,nchar,nargs)
! ===================================================
!subroutine as sent by Rainer Assent, with argument added for number of characters
!in the array plotfile_name in the calling program

    use gino_f90
    implicit none
    character(len=*),intent(out):: plotfile_name
!   character(len=nchar),intent(out):: plotfile_name

    integer:: nargs,nchar
    character(len=128):: args(2)


    nargs = 2
    call gEnqSysArgs(nargs,args)


    plotfile_name = ' '
    if (nargs > 1) plotfile_name = args(2)


  End Subroutine GetCommandLineArguments