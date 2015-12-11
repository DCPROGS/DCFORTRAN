************************************************************************
**
**  File : SAMPLE.FOR
**    This program demonstrates the Fortran to assembly interface by 
**    calling routines which are defined in the file EXAMPLE.ASM.
**
**  Compatible products   : Lahey F77L-EM/32, Lahey F77L.
**
**  How to create and run : EXAMPLE.ASM must be assembled separately  
**                          using a supported assembler 
**                          (see EXAMPLE.ASM).
**
**                          F77L3 SAMPLE
**                          386LINK SAMPLE EXAMPLE
**                          SAMPLE
**
**----------------------------------------------------------------------
**
**  This file is intended for use with the Lahey/Phar Lap 
**  386|DOS-Extender and use with any other DOS extender is not 
**  supported.
**
**  You have the same rights to the object form of this program as you
**  do to the object code of the library.  You may not distribute this
**  source, even if modified by you.
**
**  This code is subject to change without any prior notice in any 
**  future release of this language system.
**
**  Unpublished - rights reserved under the Copyright Laws of the United
**  States.  Use, duplication, or disclosure by the Government is 
**  subject to restrictions as set forth in subparagraph (c)(1)(ii) of 
**  the Rights in Technical Data and Computer Software clause at 
**  252.227-7013.
**
**                  Lahey Computer Systems, Inc.
**                           PO Box 6091
**                    Incline Village, NV 89450
**                       voice: 702-831-2500
**                         fax: 702-831-8123
**                         bbs: 702-831-8023
**                   e-mail: uunet!lahey!support
**
**         Copyright(c) 1991-1992, Lahey Computer Systems, Inc.
************************************************************************

      program sample
      implicit none

      common /foo/a,b
      real a(10),b,x,y
      integer*2 i2var /17/
      integer*4 i4var /5/,i,j,k,addint,row
      integer*2 i2array(4)/1,2,3,4/
      integer*1 outchar
      double precision dvar
      complex xvar /(3.,-4.)/
      logical*4 lvar4, example
      character cvar*8/'aB X12YZ'/

      call system("cls")
      print*,"After the pause statement this upper"
      print*,"left corner will be over written by"
      print*,"direct video writes."
      pause
      
** video_write is a subroutine which prints out the text in video mode
** in column 0 of the specified row.
      row = 0
      call video_write("This text is being written                ",row)
      row = 1
      call video_write("by direct video writes.                   ",row)
      row = 2
      call video_write("                                          ",row)
      row = 3
      call video_write("                                          ",row)
      row = 4
      call video_write("          Hello World!                    ",row)
      row = 5
      call video_write("                                          ",row)
      pause      

** example is a logical funtion which returns .TRUE. if it has not been
** called before and .FALSE. if it has previously been called.  It
** modifies or sets several of the variables.
      lvar4 = example( i2var, i4var, i2array, i2array(3), dvar,
     &  xvar )
      print*, "Should be: T, is", lvar4
      print*, "Should be: 22, is", i4var
      print*, "Should be: 22.0000000000000, is", dvar
      print*, "Should be:", (3.,-4)**2, ", is", xvar

** Makes a character string all lower case.
      call make_lower( cvar )
      print*, "Should be: 'ab x12yz'", ", is '", cvar, "'"

      print*, "The character 'P' should follow:"
      outchar = ichar ('P')
** Outputs byte to screen using a DOS call.
      call io_out( outchar )

      b = 37.
      x = 1.
** Adds two real numbers.
      call adder(x,y)
      print*, "Should be: 48.0000, is", y

      i = 3
      j = 4
** Function to add two integers.
      k = addint(i,j)
      print*,  "Should be: 7, is",k

      print *, "Three Z's should follow, printed with a BIOS call:"
      outchar =  ichar ('Z')
** Prints specified number of characters using a BIOS call.
      call bios_out (outchar, 3)

      end
      	

      	
            
