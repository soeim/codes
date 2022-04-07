program main

!*****************************************************************************80
!
!! MAIN is the main program for F90SPLIT.
!
!  Discussion:
!
!    F90SPLIT splits the modules of a FORTRAN file into separate files.
!
!    A "module" is a blockdata, function, module, program, subroutine,
!    recursive function or recursive subroutine program subunit.
!
!    The command
!
!      f90split extract.f90
!
!    processes the file EXTRACT.F90 line by line.  Each program subunit
!    that is found is written to a separate file whose name is derived
!    from the name of the program subunit.  If the program subunit does
!    not have a name, a default name is assigned.
!
!    The program should be able to split multiple files with a
!    single command, as in:
!
!      f90split *.f90
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  character ( len = 255 ) input_file
  integer ( kind = 4 ) numarg

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'F90SPLIT:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Split a FORTRAN90 program, so that each'
  write ( *, '(a)' ) '  unit is in its own file.'
!
!  Count the number of command line arguments.
!
  numarg = iargc ( )

  if ( numarg < 1 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F90SPLIT:'
    write ( *, '(a)' ) '  What is the name of the input file?'
    read ( *, '(a)' ) input_file

    if ( input_file == ' ' ) then
      stop
    end if

    numarg = 1
    call handle ( input_file )

  else

    do iarg = 1, numarg

      call getarg ( iarg, input_file )

      call handle ( input_file )

    end do

  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  Number of files handled = ', numarg
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'F90SPLIT:'
  write ( *, '(a)' ) '  Normal end of execution.'

  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine handle ( input_file )

!*****************************************************************************80
!
!! HANDLE handles one file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_FILE, the name of the file to
!    be split up.
!
  implicit none

  integer ( kind = 4 ) duplicate_num
  character ( len = 255 ) extension
  logical f90_line_is_end
  integer ( kind = 4 ) i
  character ( len = 255 ) input_file
  integer ( kind = 4 ) input_unit
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) j
  character ( len = 255 ) line
  integer ( kind = 4 ) line_length
  integer ( kind = 4 ) line_length_loc
  integer ( kind = 4 ) line_length_max
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) module_num
  logical no_name
  character ( len = 255 ) no_name_file
  integer ( kind = 4 ) no_name_line_last
  integer ( kind = 4 ) no_name_line_num
  logical no_name_open
  integer ( kind = 4 ) no_name_unit
  logical output_exists
  character ( len = 255 ) output_file
  logical output_open
  integer ( kind = 4 ) output_unit

  duplicate_num = 0
!
!  Pick off the extension of the input file.
!
  call file_ext ( input_file, i, j )

  if ( i == 0 ) then
    extension = '.f'
  else if ( 1 < i ) then
    extension = input_file(i-1:j)
  else
    extension = ' '
  end if
!
!  Open the file.
!
  call get_unit ( input_unit )

  open ( unit = input_unit, file = input_file, status = 'old', &
    iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F90SPLIT - Fatal error!'
    write ( *, '(a)' ) &
      '  Could not open the input file "' // trim ( input_file ) // '".'
    stop
  end if

  line_num = 0
  no_name_line_last = -1
  no_name_line_num = 0
  module_num = 0

  output_open = .false.

  no_name_open = .false.
  no_name = .true.
  no_name_file = 'no_name.f90'

  line_length_max = -1;
  line_length_loc = -1;

  do

    read ( input_unit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      exit
    end if

    line_num = line_num + 1

    line_length = len_trim ( line )
    if ( line_length_max < line_length ) then
      line_length_max = line_length
      line_length_loc = line_num
    end if
!
!  If we don't have a module name, then it's not clear what to do.
!  My vote is to discard the information for now.
!
!  It's important to check whether the next line marks the beginning of
!  a named module.
!
    if ( no_name ) then

      call f90_line_is_begin ( line, output_file )

      if ( output_file /= ' ' ) then

        no_name = .false.

        module_num = module_num + 1

        call s_cat ( output_file, extension, output_file )

        write ( *, '(a)' ) trim ( output_file )

        call get_unit ( output_unit )

        open ( unit = output_unit, file = output_file, &
          status = 'replace', iostat = ios )

        output_open = .true.

      end if

    end if
!
!  If an output file is not currently open...
!
    if ( .not. output_open ) then

      call f90_line_is_begin ( line, output_file )

      if ( output_file == ' ' ) then

        no_name = .true.

        if ( .not. no_name_open ) then
          write ( *, '(a)' ) trim ( no_name_file )
          call get_unit ( no_name_unit )
          open ( unit = no_name_unit, file = no_name_file, status = 'replace', &
            iostat = ios )
          no_name_open = .true.
        end if

      else

        module_num = module_num + 1

        no_name = .false.
        call s_cat ( output_file, extension, output_file )
        call s_low ( output_file )
        write ( *, '(a)' ) trim ( output_file )
!
!  Check for duplicates
!
        inquire ( file = output_file, exist = output_exists )

        if ( output_exists ) then
          duplicate_num = duplicate_num + 1
          write ( *, '(a)' ) '  Duplicate module = "' &
            // trim ( output_file ) // '".'
        end if

        call get_unit ( output_unit )

        open ( unit = output_unit, file = output_file, status = 'replace', &
          iostat = ios )

        output_open = .true.

      end if

    end if
!
!  Write the line.
!
    if ( output_open ) then
      write ( output_unit, '(a)' ) trim ( line )
    else
      write ( no_name_unit, '(a)' ) trim ( line )
      no_name_line_last = line_num
      no_name_line_num = no_name_line_num + 1
    end if

    if ( f90_line_is_end ( line ) ) then
      close ( unit = output_unit )
      output_open = .false.
      no_name = .false.
    end if

  end do
!
!  Close the NO_NAME file, and delete it.
!  Rationale:
!
!    1) I don't write main programs without a PROGRAM statement.
!    2) I don't stick blank or comment lines between routines.
!    3) The stupid ALPHA fortran compiler will FAIL if given
!       a file to compile that contains only blanks and comments!
!
  if ( no_name_open ) then
    close ( unit = no_name_unit, status = 'delete' )
    no_name_open = .false.
  end if

  close ( unit = input_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'F90SPLIT:'
  write ( *, '(a)' ) '  Reached end of ' // trim ( input_file )
  write ( *, '(a,i8)' ) '  Lines read:              ', line_num
  write ( *, '(a,i8)' ) '  Longest line length:     ', line_length_max
  write ( *, '(a,i8)' ) '  Longest line location:   ', line_length_loc
  write ( *, '(a,i8)' ) '  Named modules created:   ', module_num
  write ( *, '(a,i8)' ) '  Lines sent to NO_NAME:   ', no_name_line_num
  if ( 0 < no_name_line_num ) then
    write ( *, '(a,i8)' ) '  Last NO_NAME line:       ', no_name_line_last
  end if
  write ( *, '(a,i8)' ) '  Duplicate modules found: ', duplicate_num

  return
end
subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none

  character c
  integer ( kind = 4 ) itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
subroutine ch_low ( ch )

!*****************************************************************************80
!
!! CH_LOW lowercases a single character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantee the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character CH, the character to be lowercased.
!
  implicit none

  character              ch
  integer   ( kind = 4 ) i

  i = iachar ( ch )

  if ( 65 <= i .and. i <= 90 ) then
    ch = achar ( i + 32 )
  end if

  return
end
subroutine digit_to_ch ( digit, c )

!*****************************************************************************80
!
!! DIGIT_TO_CH returns the character representation of a decimal digit.
!
!  Example:
!
!    DIGIT   C
!    -----  ---
!      0    '0'
!      1    '1'
!    ...    ...
!      9    '9'
!     17    '*'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIGIT, the digit value between 0 and 9.
!
!    Output, character C, the corresponding character, or '*' if DIGIT
!    was illegal.
!
  implicit none

  character              c
  integer   ( kind = 4 ) digit

  if ( 0 <= digit .and. digit <= 9 ) then

    c = char ( digit + 48 )

  else

    c = '*'

  end if

  return
end
subroutine f90_line_is_begin ( line, name )

!*****************************************************************************80
!
!! F90_LINE_IS_BEGIN determines if a line begins a FORTRAN90 routine.
!
!  Discussion:
!
!    This routine will NOT properly handle complicated function
!    statements such as:
!
!      integer ( kind = 4 )*2 function fred ( a, b )
!      recursive real function bob ( c )
!
!    For that matter, if you are so bold as to have a variable whose
!    name is "PROGRAM", "FUNCTION" or a similar "keyword", then this
!    routine will incorrectly treat lines such as:
!
!      function = function + 1
!
!    The routine will also fail if the initial line of the module
!    extends over more than one line:
!
!      recursive double precision fun&
!      ction naomi ( x )
!
!    or if you use some nonstandard keyword such as
!
!      parallel function magoo ( y )
!
!    14 December 2002: This routine was, for convenience and style,
!    lowercasing the line and hence the output name.  I now find that
!    I want to preserve case, so I modified the routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a line of text.
!
!    Output, character ( len = * ) NAME, the name of the module, if this
!    line begins a module, and ' ' otherwise.
!
  implicit none

  integer ( kind = 4 ) i
  character ( len = * ) line
  character ( len = 255 ) line2
  character ( len = * ) name
  logical s_eqi

  name = ' '

  line2 = line
  call s_blank_delete ( line2 )

  if ( s_eqi ( line2(1:9), 'blockdata' ) ) then
    if ( line2(10:) == ' ' ) then
      name = 'blockdata'
    else
      call s_before_ss_copy ( line2(10:), '(', name )
    end if
  else if ( s_eqi ( line2(1:17), 'characterfunction' ) ) then
    call s_before_ss_copy ( line2(18:), '(', name )
  else if ( s_eqi ( line2(1:15), 'complexfunction' ) ) then
    call s_before_ss_copy ( line2(16:), '(', name )
  else if ( s_eqi ( line2(1:23), 'doubleprecisionfunction' ) ) then
    call s_before_ss_copy ( line2(24:), '(', name )
  else if ( s_eqi ( line2(1:8), 'function' ) ) then
    call s_before_ss_copy ( line2(9:), '(', name )
  else if ( s_eqi ( line2(1:15), 'integerfunction' ) ) then
    call s_before_ss_copy ( line2(16:), '(', name )
  else if ( s_eqi ( line2(1:15), 'logicalfunction' ) ) then
    call s_before_ss_copy ( line2(16:), '(', name )
  else if ( s_eqi ( line2(1:6), 'module' ) ) then
    call s_before_ss_copy ( line2(7:), '(', name )
  else if ( s_eqi ( line2(1:7), 'program' ) ) then
    call s_before_ss_copy ( line2(8:), '(', name )
  else if ( s_eqi ( line2(1:12), 'realfunction' ) ) then
    call s_before_ss_copy ( line2(13:), '(', name )
  else if ( s_eqi ( line2(1:17), 'recursivefunction' ) ) then
    call s_before_ss_copy ( line2(18:), '(', name )
  else if ( s_eqi ( line2(1:10), 'subroutine' ) ) then
    call s_before_ss_copy ( line2(11:), '(', name )
  else if ( s_eqi ( line2(1:19), 'recursivesubroutine' ) ) then
    call s_before_ss_copy ( line2(20:), '(', name )
  end if
!
!  In some "clever" cases, people write the name of the routine
!  on one line, continue with an ampersand, and the rest of the
!  routine follows.
!
!  I really should be reading the logical line, not the literal
!  line, but for now, let me just chop off trailing ampersands.
!
  i = index ( name, '&' )

  if ( i /= 0 ) then
    name(i:i) = ' '
  end if

  return
end
function f90_line_is_end ( line )

!*****************************************************************************80
!
!! F90_LINE_IS_END determines if a line ends a FORTRAN90 module.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a line of text.
!
!    Output, logical F90_LINE_IS_END, TRUE if the line ends a module.
!
  implicit none

  logical f90_line_is_end
  character ( len = * ) line
  character ( len = 255 ) line2

  f90_line_is_end = .false.

  line2 = line

  call s_low ( line2 )

  call s_blank_delete ( line2 )

  if ( &
    line2       == 'end' .or. &
    line2(1:12) == 'endblockdata' .or. &
    line2(1:11) == 'endfunction' .or. &
    line2(1:9)  == 'endmodule' .or. &
    line2(1:10) == 'endprogram' .or. &
    line2(1:13) == 'endsubroutine' .or. &
    line2(1:4)  == 'end!' ) then

    f90_line_is_end = .true.

  end if

  return
end
subroutine file_ext ( file_name, i, j )

!*****************************************************************************80
!
!! FILE_EXT determines the "extension" of a file name.
!
!  Discussion:
!
!    The "extension" of a filename is the string of characters
!    that appears after the LAST period in the name.  A file
!    with no period, or with a period as the last character
!    in the name, has a "null" extension.
!
!    Blanks are unusual in filenames.  This routine ignores all
!    trailing blanks, but will treat initial or internal blanks
!    as regular characters acceptable in a file name.
!
!  Example:
!
!    FILE_NAME  I  J
!
!    bob.for    5  7
!    N.B.C.D    7  7
!    Naomi.     0  0
!    Arthur     0  0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 February 2000
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, a file name to be examined.
!
!    Output, integer ( kind = 4 ) I, J, the indices of the first and last 
!    characters in the file extension.
!
!    If at least one period occurs in the filename, and at least one
!    nonblank character follows that period, then I will be the index
!    of the first character after the period, and J the index of the
!    last nonblank character after the period.  The extension is
!    therefore equal to FILE_NAME(I:J).
!
!    Otherwise, I and J will be returned as 0, indicating that the file
!    has no extension.
!
  implicit none

  character ( len = * ) file_name
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) s_index_last

  i = s_index_last ( file_name, '.' )

  if ( i /= 0 ) then

    j = len_trim ( file_name )

    if ( i == j ) then
      i = 0
      j = 0
    else
      i = i + 1
    end if

  else

    j = 0

  end if

  return
end
subroutine get_unit ( iunit )

!************************************************