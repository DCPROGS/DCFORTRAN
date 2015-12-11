
. .
!     Last change:  JCT  22 Sep 100   11:06 am
      Program RichTextEditor
!***********************************************
! Copyright Bradly Associates Limited
!        GINOMENU Version 5.0
!***********************************************
USE MENU_F90

    INTEGER :: master_window, menu_bar, toolbar, style_toolbar, status_bar

    INTEGER :: file_entry,          file_popup
    INTEGER :: edit_entry,          edit_popup
    INTEGER :: justification_entry, justification_popup
    INTEGER :: indentation_entry,   indentation_popup
    INTEGER :: effects_entry,       effects_popup
    INTEGER :: window_entry,        window_popup
    INTEGER :: help_entry,          help_popup

    INTEGER :: new_document, open_document, save_document, save_as, print_document
    INTEGER :: separator, exit_program
    INTEGER :: undo_last_change
    INTEGER :: left_justify,centre_justify,right_justify
    INTEGER :: left_indent,first_line_indent,right_indent
    INTEGER :: normal_text,bullet_text
    INTEGER :: cascade_windows,tile_windows
    INTEGER :: about_help

    INTEGER :: colour_button, print_button
    INTEGER :: left_button, centre_button, right_button
    INTEGER :: cut_button, copy_button, paste_button
    INTEGER :: bold_button, italic_button, underline_button
    INTEGER :: typeface_combo, size_combo

    INTEGER :: document, document_text

    INTEGER :: callback, message_status

    CHARACTER*80 :: font,text,text2,filename,dirnam,filter
    INTEGER :: ipans(4)
    INTEGER :: ichild(20)

    INTEGER :: current_window, current_child

    REAL :: RED=0.0,GREEN=0.0,BLUE=0.0

    type (GACTION) :: actlist

    DATA ipans/100,170,250,-1/

    call gOpenGino
    call gGuiwin
    call gmInitializeMenu
    call gmSetGuiGridMode(GOFF)

! ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
! ³Create Master Window with Menubar ³
! ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      master_window = gmCreateMDIMasterWindow(50,50,425,275,GALL,'GINOMENU Rich Text Editor',&
                                              gmIconFormat = GDLLICON,&
                                              gmIconFile = 'gee')

      menu_bar = gmCreateMenuBar(master_window)
!---------------------------------------------------------------------------------
      file_entry = gmCreateMenuEntry(menu_bar,'File',&
                                     gmAccel = ICHAR('F'))

        file_popup = gmCreatePullDownMenu(file_entry)

          new_document =   gmCreateMenuEntry(file_popup,'New Document',&
                                             gmAccel = ICHAR('N'),&
                                             gmcallback = -50,&
                                             gmHelp = 'Create New Text Document')
          open_document =  gmCreateMenuEntry(file_popup,'Open',&
                                             gmAccel = ICHAR('O'),&
                                             gmcallback = -30,&
                                             gmHelp = 'Open Text Document from file')
          save_document =  gmCreateMenuEntry(file_popup,'Save',&
                                             gmAccel = ICHAR('S'),&
                                             gmcallback = -40,&
                                             gmHelp = 'Save Text Document to File')
          save_as =        gmCreateMenuEntry(file_popup,'Save As...',&
                                             gmAccel = ICHAR('A'),&
                                             gmcallback = -45,&
                                             gmHelp = 'Save Text Document as a new File')
          print_document = gmCreateMenuEntry(file_popup,'Print...',&
                                             gmAccel = ICHAR('P'),&
                                             gmcallback = 620,&
                                             gmHelp = 'Print Text Document')

          separator =      gmCreateMenuSeparator(file_popup)


          exit_program   = gmCreateMenuEntry(file_popup,'Exit',&
                                             gmAccel = ICHAR('x'),&
                                             gmcallback = -1,&
                                             gmHelp = 'Exit Text Editor')

!------------------------------------------------------------------------------------
      edit_entry   = gmCreateMenuEntry(menu_bar,'Edit',&
                                       gmAccel = ICHAR('E'))

        edit_popup = gmCreatePullDownMenu(edit_entry)

          undo_last_change   = gmCreateMenuEntry(edit_popup,'Undo',&
                                                 gmAccel = ICHAR('U'),&
                                                 gmcallback = 320,&
                                                 gmHelp = 'Undo the last change')
!------------------------------------------------------------------------------------
      justification_entry   = gmCreateMenuEntry(menu_bar,'Justification',&
                                       gmAccel = ICHAR('J'))

        justification_popup = gmCreatePullDownMenu(justification_entry)

          left_justify =   gmCreateMenuEntry(justification_popup,'Left',&
                                             gmAccel = ICHAR('L'),&
                                             gmcallback = 300,&
                                             gmHelp = 'Left Justify Paragraph')
          centre_justify = gmCreateMenuEntry(justification_popup,'Centre',&
                                             gmAccel = ICHAR('C'),&
                                             gmcallback = 301,&
                                             gmHelp = 'Centre Justify Paragraph')
          right_justify =  gmCreateMenuEntry(justification_popup,'Right',&
                                             gmAccel = ICHAR('R'),&
                                             gmcallback = 302,&
                                             gmHelp = 'Right Justify Paragraph')
!-----------------------------------------------------------------------------------
      indentation_entry   = gmCreateMenuEntry(menu_bar,'Indentation',&
                                              gmAccel = ICHAR('I'))

        indentation_popup = gmCreatePopupMenu(indentation_entry,0,0,GNONE,' ')

          left_indent =       gmCreateMenuEntry(indentation_popup,'Left Indent',&
                                                gmAccel = ICHAR('L'),&
                                                gmcallback = 380,&
                                                gmHelp = 'Left Indent Entire Paragraph')
          first_line_indent = gmCreateMenuEntry(indentation_popup,'First Line Indent',&
                                                gmAccel = ICHAR('F'),&
                                                gmcallback = 381,&
                                                gmHelp = 'Left Indent First Line of Paragraph')
          right_indent =      gmCreateMenuEntry(indentation_popup,'Right Indent',&
                                                gmAccel = ICHAR('R'),&
                                                gmcallback = 382,&
                                                gmHelp = 'Right Indent Paragraph')
!----------------------------------------------------------------------------------
      effects_entry   = gmCreateMenuEntry(menu_bar,'Effects',&
                                              gmAccel = ICHAR('E'))

        effects_popup = gmCreatePullDownMenu(effects_entry)

          normal_text = gmCreateMenuEntry(effects_popup,'Normal Text',&
                                          gmAccel = ICHAR('N'),&
                                          gmcallback = 350,&
                                          gmHelp = 'Normal non-bulleted text')
          bullet_text = gmCreateMenuEntry(effects_popup,'Bullet',&
                                          gmAccel = ICHAR('B'),&
                                          gmcallback = 351,&
                                          gmHelp = 'Bulleted Text')
!----------------------------------------------------------------------------------
      window_entry   = gmCreateMenuEntry(menu_bar,'Windows',&
                                          gmAccel = ICHAR('W'))

        window_popup = gmCreatePullDownMenu(window_entry)

          cascade_windows = gmCreateMenuEntry(window_popup,'Cascade',&
                                              gmAccel = ICHAR('C'),&
                                              gmcallback = 700,&
                                              gmHelp = 'Cascade all document windows')
          tile_windows =    gmCreateMenuEntry(window_popup,'Tile',&
                                              gmAccel = ICHAR('T'),&
                                              gmcallback = 710,&
                                              gmHelp = 'Tile all document windows')
!----------------------------------------------------------------------------------
      help_entry   = gmCreateMenuEntry(menu_bar,'Help',&
                                       gmAccel = ICHAR('H'))

        help_popup = gmCreatePullDownMenu(help_entry)

          about_help = gmCreateMenuEntry(help_popup,'About',&
                                         gmAccel = ICHAR('A'),&
                                         gmcallback = 720,&
                                         gmHelp = 'About the application')

! ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
! ³ Create Font Control ToolBox Panel ³
! ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      toolbar = gmCreateToolBar(master_window,GTOP,22,&
                                gmTitle = 'Text Format Toolbar')
      typeface_combo = gmCreateComboBox(toolbar,0,0,150,550,GFONTS,GDISPLAY,&
                                        gmSort = GSORTED,&
                                        gmcallback = 210)
      size_combo = gmCreateComboBox(toolbar,0,0,50,550,GNONE,GEDIT,&
                                    gmcallback = 220)

      call gmSetListEntry(size_combo,GADD, gmString = '  6', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = '  8', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 10', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 12', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 14', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 16', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 18', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 24', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 32', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 48', gmEntry = 0)
      call gmSetListEntry(size_combo,GADD, gmString = ' 72', gmEntry = 0)

      separator = gmCreateToolBarSeparator(toolbar)

      colour_button = gmCreateToolBarButton(toolbar,GICONBUTTON,'msml1020',&
                                                 gmHFlag = GBUBBLEANDSTATUSBAR,&
                                                 gmHelp = 'Colour Selector',&
                                                 gmcallback = 600)
      print_button =       gmCreateToolBarButton(toolbar,GICONBUTTON,'mstd1007',&
                                                 gmHFlag = GBUBBLEANDSTATUSBAR,&
                                                 gmHelp = 'Print Document',&
                                                 gmcallback = 620)

      separator = gmCreateToolBarSeparator(toolbar)

      cut_button =   gmCreateToolBarButton(toolbar,GICONBUTTON,'mstd1004',&
                                           gmHFlag = GBUBBLEANDSTATUSBAR,&
                                           gmHelp = 'Cut to Clipboard',&
                                           gmcallback = 500)
      copy_button =  gmCreateToolBarButton(toolbar,GICONBUTTON,'mstd1005',&
                                           gmHFlag = GBUBBLEANDSTATUSBAR,&
                                           gmHelp = 'Copy to Clipboard',&
                                           gmcallback = 501)
      paste_button = gmCreateToolBarButton(toolbar,GICONBUTTON,'mstd1006',&
                                           gmHFlag = GBUBBLEANDSTATUSBAR,&
                                           gmHelp = 'Paste from Clipboard',&
                                           gmcallback = 502)

      style_toolbar = gmCreateToolBar(master_window,GTOP,22,&
                                      gmTitle = 'Text Style Toolbar')

      left_button =   gmCreateToolBarButton(style_toolbar,GICONBUTTON,'left',&
                                            gmHFlag = GBUBBLEANDSTATUSBAR,&
                                            gmHelp = 'Left Justify Paragraph',&
                                            gmcallback = 300)
      centre_button = gmCreateToolBarButton(style_toolbar,GICONBUTTON,'centre',&
                                            gmHFlag = GBUBBLEANDSTATUSBAR,&
                                            gmHelp = 'Centre Justify Paragraph',&
                                            gmcallback = 301)
      right_button =  gmCreateToolBarButton(style_toolbar,GICONBUTTON,'right',&
                                            gmHFlag = GBUBBLEANDSTATUSBAR,&
                                            gmHelp = 'Right Justify Paragraph',&
                                            gmcallback = 302)

      separator = gmCreateToolBarSeparator(toolbar)

      bold_button =      gmCreateToolBarButton(style_toolbar,GICONBUTTON,'bold',&
                                               gmHFlag = GBUBBLEANDSTATUSBAR,&
                                               gmHelp = 'Bold Selected Text',&
                                               gmcallback = 303)
      italic_button =    gmCreateToolBarButton(style_toolbar,GICONBUTTON,'italic',&
                                               gmHFlag = GBUBBLEANDSTATUSBAR,&
                                               gmHelp = 'Italicize Selected Text',&
                                               gmcallback = 304)
      underline_button = gmCreateToolBarButton(style_toolbar,GICONBUTTON,'under',&
                                               gmHFlag = GBUBBLEANDSTATUSBAR,&
                                               gmHelp = 'Underline Selected Text',&
                                               gmcallback = 305)

      status_bar = gmCreateStatusBar(master_window,&
                                     gmPanes = 4,&
                                     gmSizes = IPANS)


! ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
! ³ Manage window and enter Action Loop ³
! ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      call gmManage

! ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
! ³ Create blank Text Area  ³
! ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      document = gmCreateMDIComplexDialogueBox(master_window,0,0,200,190,1,'* New Document',&
                                               gmQuitMode = GLEAVE,&
                                               gmIconFormat = GDLLICON,&
                                               gmIconFile = 'MBIG1083',&
                                               gmcallback = -2,&
                                               gmSelect = -3)

      document_text = gmCreateTextEntry(document,0,0,100,100,CHAR(0),1024,GEDIT,&
                                        gmEntryType = GRICHEDIT,&
                                        gmHpos = GCENTRE,&
                                        gmVpos = GMIDDLE,&
                                        gmExpand = GON,&
                                        gmSelect = 400,&
                                        gmChange = 400)
      call gmDrawWindow(document)
      icall = 0
      current_child = document_text
      current_window = document
      call gmDefineTimercallback(1000,800)

      do
        callback = gmAction(callback)

        select case (callback)
 
        case (-2)

          call gmEnqActionState(actlist)
          ident = actlist%ident
          if (actlist%status.eq.2) then
            call gmEnqWidgetProp(ident,gmTitle = text)
            message_status = gmDisplayMessageBox('Window Close',&
                                                 'Do you want to save the file||'//  &
                                                 TEXT//'||before closing this window?',&
                                                 GQUESTION, GYESNO)

            select case (message_status)

            case (6)

              IWIDS = 1
              call gmReturnChildList(ident,iwids,ichild)
              IF(TEXT(1:1) .eq. '*')then  !New Document
                FILTER = '*.rtf|Rich Text Format|*.tex|Text File'
                call gmFileBrowser(filename,dirnam,filter,gmBrowseType = GOUTPUT)
                if (filename(1:1).ne.' ') then
                  ilen = gTrueLen(dirnam)
                  text = dirnam(1:ilen)//'\'//filename
                  isstat = gmSaveTextToFile(ichild(1),text)
                  call gmEraseWidget(ident)
                ENDIF
              else
                ISSTAT = -1  ! Overwrite File
                isstat = gmSaveTextToFile(ichild(1),text)
                call gmEraseWidget(ident)
              ENDIF

            case (3)

              call gmEraseWidget(ident)

            end select
          endif

        case (-3)

          call gmEnqActionState(actlist)
          ident = actlist%ident
          iwids = 1
          call gmReturnChildList(ident,iwids,ichild)
          current_child = ichild(1)
          current_window = ident
          call gmEnqWidgetProp(ident,gmTitle = text)
          call gmSetStatusBarText(status_bar,3,text)

        case (-30)

          filter = '*.rtf|Rich Text Format|*.tex|Text File'
          call gmFileBrowser(filename,dirnam,filter,gmBrowseType = GINPUT)
          if (filename(1:1).NE.' ') then
            ilen = gTrueLen(dirnam)
            text = dirnam(1:ilen)//'\'//filename
            document = gmCreateMDIComplexDialogueBox(master_window,0,0,200,190,1,text,&
                                                     gmQuitMode = GLEAVE,&
                                                     gmIconFormat = GDLLICON,&
                                                     gmIconFile = 'MBIG1083',&
                                                     gmcallback = -2,&
                                                     gmSelect = -3)

            document_text = gmCreateTextEntry(document,0,0,100,100,CHAR(0),32768,GEDIT,&
                                              gmEntryType = GRICHEDIT,&
                                              gmHpos = GCENTRE,&
                                              gmVpos = GMIDDLE,&
                                              gmExpand = GON,&
                                              gmSelect = 400,&
                                              gmChange = 400)
            istat = gmGetTextFromFile(document_text,filename)
            if (istat.eq.0) call gmEraseWidget(document)
            current_child = document_text
          end if

        case (-40)

          call gmEnqWidgetProp(current_window, gmTitle = text)
          if (text(1:1).eq.'*') then
            filter = '*.rtf|Rich Text Format|*.tex|Text File|*.*|All Files'
            call gmFileBrowser(filename,dirnam,filter,gmBrowseType = GOUTPUT)
            if (filename(1:1).ne.' ') then
              istat = gmSaveTextToFile(current_child,filename)
              ilen = gTrueLen(dirnam)
              text = dirnam(1:ilen)//'\'//filename
              call gmSetWidgetProp(current_window, gmTitle = text)
            end if
          else
            filename = text
            istat = -1
            istat = gmSaveTextToFile(current_child,filename)
          endif

        case (-45)
   
          filter = '*.rtf|Rich Text Format|*.tex|Text File|*.*|All Files'
          call gmFileBrowser(filename,dirnam,filter,gmBrowseType = GOUTPUT)
          if (filename(1:1).ne.' ') then
            istat = gmSaveTextToFile(current_child,filename)
            ilen = gTrueLen(dirnam)
            text = dirnam(1:ilen)//'\'//filename
            call gmSetWidgetProp(current_window, gmTitle = text)
          endif
        case (-50)

         document = gmCreateMDIComplexDialogueBox(master_window,0,0,400,300,GALL,'* New Document',&
                                                   gmSelect = -3,&
                                                   gmcallback = -2,&
                                                   gmIconFormat = GDLLICON,&
                                                   gmIconFile = 'mbig1083')
         document_text = gmCreateTextEntry(document,0,0,100,100,' ',1024,GEDIT,&
                                           gmHpos = GCENTRE,&
                                           gmEntryType = GRICHEDIT,&
                                           gmVpos = GMIDDLE,&
                                           gmExpand = GON,&
                                           gmSelect = 400,&
                                           gmChange = 400)

! ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
! ³ Check to see which font is selected ³
! ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

        case (210)

          istat = gmEnqListEntry(typeface_combo,0,font)
          call gmSetFontAttribs(current_child, gmFontFace = font)
          call gmSetActiveWidget(current_child)

        case (220)

          istat = gmEnqListEntry(size_combo,0,text)
          read(text,FMT='(I3)')ifs
          call gmSetFontAttribs(current_child, gmPointSize = ifs)
          call gmSetActiveWidget(current_child)

        case (300)

          call gmSetParagraphProp(current_child, gmJustify = GLEFT)
          call gmSetWidgetStatus(left_justify,  GCHECKED)
          call gmSetWidgetStatus(centre_justify,GSELECTABLE)
          call gmSetWidgetStatus(right_justify ,GSELECTABLE)
          call gmSetWidgetStatus(left_button,   GCHECKED)
          call gmSetWidgetStatus(centre_button, GSELECTABLE)
          call gmSetWidgetStatus(right_button,  GSELECTABLE)

        case (301)

          call gmSetParagraphProp(current_child, gmJustify = GCENTRE)
          call gmSetWidgetStatus(left_justify,  GSELECTABLE)
          call gmSetWidgetStatus(centre_justify,GCHECKED)
          call gmSetWidgetStatus(right_justify ,GSELECTABLE)
          call gmSetWidgetStatus(left_button,   GSELECTABLE)
          call gmSetWidgetStatus(centre_button, GCHECKED)
          call gmSetWidgetStatus(right_button,  GSELECTABLE)

        case (302)

          call gmSetParagraphProp(current_child, gmJustify = GRIGHT)
          call gmSetWidgetStatus(left_justify,  GSELECTABLE)
          call gmSetWidgetStatus(centre_justify,GSELECTABLE)
          call gmSetWidgetStatus(right_justify ,GCHECKED)
          call gmSetWidgetStatus(left_button,   GSELECTABLE)
          call gmSetWidgetStatus(centre_button, GSELECTABLE)
          call gmSetWidgetStatus(right_button,  GCHECKED)

        case (303)

          call gmEnqFontAttribs(current_child,gmBold = ival)
          ival = iabs(ival-1)
          call gmSetFontAttribs(current_child,gmBold = ival)
          call gmSetWidgetStatus(bold_button,ival+1)

        case (304)

          call gmEnqFontAttribs(current_child,gmItalic = ival)
          ival = iabs(ival-1)
          call gmSetFontAttribs(current_child,gmItalic = ival)
          call gmSetWidgetStatus(italic_button,ival+1)

        case (305)

          call gmEnqFontAttribs(current_child,gmUnderline = ival)
          ival = iabs(ival-1)
          call gmSetFontAttribs(current_child,gmUnderline = ival)
          call gmSetWidgetStatus(underline_button,ival+1)

        case (320)

          istat = gmTextUndoAction(current_child,GUNDO)
          istat = gmTextUndoAction(current_child,GCANUNDO)
          if (istat.eq.0) then
            call gmSetWidgetStatus(undo_last_change,GUNSELECTABLE)
          else
            call gmSetWidgetStatus(undo_last_change,GSELECTABLE)
          endif

        case (350)

          call gmSetParagraphProp(current_child, gmBullet = 0)
          call gmSetWidgetStatus(normal_text,GCHECKED)
          call gmSetWidgetStatus(bullet_text,GSELECTABLE)

        case (351)

          call gmSetParagraphProp(current_child, gmBullet = 1)
          call gmSetWidgetStatus(normal_text,GSELECTABLE)
          call gmSetWidgetStatus(bullet_text,GCHECKED)

        case (380)

          call gmEnqParagraphProp(current_child,gmIndentBody = ival)
          if (ival.ne.0)then
            call gmSetWidgetStatus(left_indent,GSELECTABLE)
            call gmSetParagraphProp(current_child, gmIndentBody = 0)
          else
            call gmSetWidgetStatus(left_indent,GCHECKED)
            call gmSetParagraphProp(current_child,gmIndentBody = -500)
          ENDIF

        case (381)

          call gmEnqParagraphProp(current_child, gmIndentAll = ival)
          if (ival.NE.0)then
            call gmSetWidgetStatus(first_line_indent,GSELECTABLE)
            call gmSetParagraphProp(current_child, gmIndentAll = 0)
          else
            call gmSetWidgetStatus(first_line_indent,GCHECKED)
            call gmSetParagraphProp(current_child, gmIndentAll = 500)
          ENDIF

        case (382)

          call gmEnqParagraphProp(current_child, gmIndentRight = ival)
          if (ival.ne.0)then
            call gmSetWidgetStatus(right_indent,GSELECTABLE)
            call gmSetParagraphProp(current_child, gmIndentRight = 0)
          else
            call gmSetWidgetStatus(right_indent,GCHECKED)
            call gmSetParagraphProp(current_child, gmIndentRight = 500)
          ENDIF

        case (400)

          call gmEnqActionState(actlist)
          current_child = actlist%ident
          call gmEnqSelectedTextRange(current_child,imin,imax)

          if(imin.eq.imax)then
            call gmSetWidgetStatus(cut_button,GUNSELECTABLE)
            call gmSetWidgetStatus(copy_button,GUNSELECTABLE)
          else
            call gmSetWidgetStatus(cut_button,GSELECTABLE)
            call gmSetWidgetStatus(copy_button,GSELECTABLE)
          ENDIF

          ival = gmEnqTextPosition(current_child, GLINENUMBER, gmItem = -1)
          WRITE(TEXT,FMT='(A5,I4)')'LINE:',ival
          call gmSetStatusBarText(status_bar,1,text)

          ival = gmEnqTextPosition(current_child, GFIRSTCHAR, gmItem = -1)
          WRITE(TEXT,FMT='(A7,I4)')'COLUMN:',IMIN-ival
          call gmSetStatusBarText(status_bar,2,text)

          call gmEnqFontAttribs(current_child, gmFontFace = font, gmPointSize = isize)
          call gmSetTextSetting(typeface_combo,font)
          WRITE(TEXT,FMT='(I3)')ISIZE
          call gmSetTextSetting(size_combo,text)
          istat =  gmEnqListEntry(size_combo,0,text2)
          if (text.ne.text2) then
            call gmSetListEntry(size_combo,GADD, gmString = text,gmEntry = 0)
            call gmSetTextSetting(size_combo,text)
          end if
  !  Update Justification Tick Marks
          call gmSetWidgetStatus(left_justify,  GSELECTABLE)
          call gmSetWidgetStatus(centre_justify,GSELECTABLE)
          call gmSetWidgetStatus(right_justify, GSELECTABLE)
          call gmSetWidgetStatus(left_button,   GSELECTABLE)
          call gmSetWidgetStatus(centre_button, GSELECTABLE)
          call gmSetWidgetStatus(right_button,  GSELECTABLE)
          call gmEnqParagraphProp(current_child, gmJustify = ival)
          IF(ival.eq.-1)call gmSetWidgetStatus(left_justify,  GCHECKED)
          IF(ival.eq. 0)call gmSetWidgetStatus(centre_justify,GCHECKED)
          IF(ival.eq. 1)call gmSetWidgetStatus(right_justify, GCHECKED)
          IF(ival.eq.-1)call gmSetWidgetStatus(left_button,   GCHECKED)
          IF(ival.eq. 0)call gmSetWidgetStatus(centre_button, GCHECKED)
          IF(ival.eq. 1)call gmSetWidgetStatus(right_button,  GCHECKED)
  !  Update Character Style Buttons
          call gmEnqFontAttribs(current_child, gmBold = ival)
          if (ival.ne.0) then
            call gmSetWidgetStatus(bold_button,GCHECKED)
          else
            call gmSetWidgetStatus(bold_button,GSELECTABLE)
          end if
          call gmEnqFontAttribs(current_child, gmItalic = ival)
          if (ival.ne.0) then
            call gmSetWidgetStatus(italic_button,GCHECKED)
          else
            call gmSetWidgetStatus(italic_button,GSELECTABLE)
          end if
          call gmEnqFontAttribs(current_child, gmUnderline = ival)
          if (ival.ne.0) then
            call gmSetWidgetStatus(underline_button,GCHECKED)
          else
            call gmSetWidgetStatus(underline_button,GSELECTABLE)
          end if
  !  Update Bullet Effects Tick Marks
          call gmSetWidgetStatus(normal_text, GSELECTABLE)
          call gmSetWidgetStatus(bullet_text, GSELECTABLE)
          call gmEnqParagraphProp(current_child, gmBullet = ival)
          if (ival.eq. 0)call gmSetWidgetStatus(normal_text,GCHECKED)
          if (ival.eq. 1)call gmSetWidgetStatus(bullet_text,GCHECKED)
  !  Update Indent Flags
          call gmSetWidgetStatus(left_indent,      GSELECTABLE)
          call gmSetWidgetStatus(first_line_indent,GSELECTABLE)
          call gmSetWidgetStatus(right_indent,     GSELECTABLE)
          call gmEnqParagraphProp(current_child, gmIndentBody = ival)
          if (ival.NE.0)call gmSetWidgetStatus(left_indent,GCHECKED)
          call gmEnqParagraphProp(current_child, gmIndentAll = ival)
          if (ival.NE.0)call gmSetWidgetStatus(first_line_indent,GCHECKED)
          call gmEnqParagraphProp(current_child, gmIndentRight = ival)
          if (ival.NE.0)call gmSetWidgetStatus(right_indent,GCHECKED)
  !  Update Undo Information
          call gmSetActiveWidget(current_child)
          istat = gmTextUndoAction(current_child,GCANUNDO)
          if (istat.eq.0) then
            call gmSetWidgetStatus(undo_last_change,GUNSELECTABLE)
          else
            call gmSetWidgetStatus(undo_last_change,GSELECTABLE)
          end if

        case (500)

          call gmCopyTextToClipboard(current_child,GCUT)
          call gmSetActiveWidget(current_child)

        case (501)

          call gmCopyTextToClipboard(current_child,GCOPY)
          call gmSetActiveWidget(current_child)

        case (502)

          call gmCopyTextToClipboard(current_child,GPASTE)
          call gmSetActiveWidget(current_child)

        case (600)

          call gmColourControl(RED,GREEN,BLUE)
          call gDefineRGB(20,RED,GREEN,BLUE)
          call gmSetFontAttribs(current_child,gmTextCol = 20)
          call gmSetActiveWidget(current_child)

        case (620)

          call gmPrintTextWidget(current_child,GOPENPRINTERDIALOGUE,GSERIF,12)
          call gmSetActiveWidget(current_child)

        case (700)

          call gmMDIAction(master_window,GCASCADE)

        case (710)

          call gmMDIAction(master_window,GTILE)

        case (720)

          call gmDisplayInfoBox('About','GINOMENU Rich Text Editor Version 1.0')

        case (800)

          istat = gmEnqClipboardStatus(GTEXT)
          if (istat.eq.0) then
            call gmSetWidgetStatus(paste_button,GUNSELECTABLE)
          else
            call gmSetWidgetStatus(paste_button,GSELECTABLE)
          end if

        case (-1)

          call gmCloseMenu
          exit

        end select
      end do
      stop
      end

