subroutine ch_cap ( ch )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
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
!    Input/output, character CH, the character to capitalize.
!
  implicit none

  character ch
  integer ( kind = 4 ) itemp

  itemp = iachar ( ch )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    ch = achar ( itemp - 32 )
  end if

  return
end
function ch_eqi ( c1, c2 )

!*****************************************************************************80
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!  Example:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical ( kind = 4 ) CH_EQI, the result of the comparison.
!
  implicit none

  logical ( kind = 4 ) ch_eqi
  character c1
  character c1_cap
  character c2
  character c2_cap

  c1_cap = c1
  c2_cap = c2

  call ch_cap ( c1_cap )
  call ch_cap ( c2_cap )

  if ( c1_cap == c2_cap ) then
    ch_eqi = .true.
  else
    ch_eqi = .false.
  end if

  return
end
subroutine ch_to_digit ( c, digit )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
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
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding integer value.
!    If C was 'illegal', then DIGIT is -1.
!
  implicit none

  character c
  integer ( kind = 4 ) digit

  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

    digit = ichar ( c ) - 48

  else if ( c == ' ' ) then

    digit = 0

  else

    digit = -1

  end if

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine gmsh_data_read ( gmsh_filename, node_dim, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! GMSH_DATA_READ reads data from a GMSH file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) GMSH_FILENAME, the GMSH filename.
!
!    Input, integer ( kind = 4 ) NODE_DIM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Output, real ( kind = 8 ) NODE_X(NODE_DIM,NODE_NUM), the node coordinates.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Output, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the nodes that make up each element.
!

  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  integer ( kind = 4 ) node_dim
  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) element_node(element_order,element_num)
  character ( len = * ) gmsh_filename
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_dummy
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) input
  integer ( kind = 4 ) input_stat
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) length
  integer ( kind = 4 ) level
  real ( kind = 8 ) node_x(node_dim,node_num)
  real ( kind = 8 ), parameter :: r8_big = 1.0D+30
  logical s_begin
  character ( len = 255 ) text
  real ( kind = 8 ) x
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) y
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min
  real ( kind = 8 ) z
  real ( kind = 8 ) z_max
  real ( kind = 8 ) z_min

  call get_unit ( input )

  open ( unit = input, file = gmsh_filename, status = 'old', &
    iostat = input_stat )

  if ( input_stat /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
    write ( *, '(a)' ) '  Could not open input file "' // &
      trim ( gmsh_filename ) // '"'
    stop 1
  end if

  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( input_stat /= 0 ) then
      write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Error while seeking node coordinates.'
      stop 1
    end if

    if ( level == 0 ) then
      if ( s_begin ( text(1:6), '$Nodes' ) ) then
        level = 1
        j = 0
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, i4_dummy, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:9), '$EndNodes' ) ) then
        exit
      else
        j = j + 1
        call s_to_i4 ( text, indx, ierror, length )
        text = text(length+1:)
        do i = 1, node_dim
          call s_to_r8 ( text, x, ierror, length )
          text = text(length+1:)
          node_x(i,j) = x
        end do      
      end if
    end if

  end do
!
!  Now read element information.
!
  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( input_stat /= 0 ) then
      write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
      write ( *, '(a)' ) '  Error while seeking element connectivity.'
      stop 1
    end if

    if ( level == 0 ) then
      if ( s_begin ( text(1:9), '$Elements' ) ) then
        level = 1
        j = 0
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, i4_dummy, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:12), '$EndElements' ) ) then
        exit
      else
        j = j + 1
        k = 0
        do k = 1, 5
          call s_to_i4 ( text, i4_dummy, ierror, length )
          text = text(length+1:)
        end do
        do i = 1, element_order
          call s_to_i4 ( text, k, ierror, length )
          text = text(length+1:)
          element_node(i,j) = k
        end do
      end if
    end if

  end do

  close ( unit = input )

  return
end
subroutine gmsh_size_read ( gmsh_filename, node_num, node_dim, element_num, &
  element_order )

!*****************************************************************************80
!
!! GMSH_SIZE_READ reads sizes from a GMSH file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) GMSH_FILENAME, the GMSH filename.
!
!    Output, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Output, integer ( kind = 4 ) NODE_DIM, the spatial dimension.
!
!    Output, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Output, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  character ( len = * ) gmsh_filename
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) input
  integer ( kind = 4 ) input_stat
  integer ( kind = 4 ) k
  integer ( kind = 4 ) length
  integer ( kind = 4 ) level
  integer ( kind = 4 ) node_dim
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), parameter :: r8_big = 1.0D+30
  logical s_begin
  character ( len = 255 ) text
  real ( kind = 8 ) x
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) y
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min
  real ( kind = 8 ) z
  real ( kind = 8 ) z_max
  real ( kind = 8 ) z_min

  node_num = 0
  node_dim = 0

  x_max = - r8_big
  x_min = + r8_big
  y_max = - r8_big
  y_min = + r8_big
  z_max = - r8_big
  z_min = + r8_big

  call get_unit ( input )

  open ( unit = input, file = gmsh_filename, status = 'old', &
    iostat = input_stat )

  if ( input_stat /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GMSH_SIZE_READ - Fatal error!'
    write ( *, '(a)' ) '  Could not open input file "' // &
      trim ( gmsh_filename ) // '"'
    stop 1
  end if

  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( level == 0 ) then
      if ( s_begin ( text(1:6), '$Nodes' ) ) then
        level = 1
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, node_num, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:9), '$EndNodes' ) ) then
        exit
      else
        call s_to_i4 ( text, indx, ierror, length )
        text = text(length+1:)
        call s_to_r8 ( text, x, ierror, length )
        x_min = min ( x_min, x )
        x_max = max ( x_max, x )
        text = text(length+1:)
!
!  Need to check that we actually were able to read an R8 here.
!
        call s_to_r8 ( text, y, ierror, length )
        y_min = min ( y_min, y )
        y_max = max ( y_max, y )
        text = text(length+1:)
        call s_to_r8 ( text, z, ierror, length )
        text = text(length+1:)
        z_min = min ( z_min, z )
        z_max = max ( z_max, z )
      end if
    end if

  end do
!
!  Make a very simple guess as to the dimensionality of the data.
!
  node_dim = 3
  if ( z_max == z_min ) then
    node_dim = 2
    if ( y_max == y_min ) then
      node_dim = 1
    end if
  end if
!
!  Now read element information.
!
  level = 0

  do

    read ( input, '(a)', iostat = input_stat ) text

    if ( level == 0 ) then
      if ( s_begin ( text(1:9), '$Elements' ) ) then
        level = 1
      end if
    else if ( level == 1 ) then
      call s_to_i4 ( text, element_num, ierror, length )
      level = 2
    else if ( level == 2 ) then
      if ( s_begin ( text(1:12), '$EndElements' ) ) then
        exit
      else
        k = 0
        do 
          call s_to_i4 ( text, indx, ierror, length )
          text = text(length+1:)
          if ( ierror /= 0 ) then
            exit
          end if
          k = k + 1
        end do
        element_order = k - 5
        exit
      end if
    end if

  end do

  close ( unit = input )

  return
end
subroutine gmsh_mesh1d_write ( gmsh_filename, m, node_num, node_x, &
  element_order, element_num, element_node )

!*****************************************************************************80
!
!! GMSH_MESH1D_WRITE writes 1D mesh data as a Gmsh file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Christophe Geuzaine, Jean-Francois Remacle,
!    Gmsh: a three-dimensional finite element mesh generator with
!    built-in pre- and post-processing facilities,
!    International Journal for Numerical Methods in Engineering,
!    Volume 79, Number 11, pages 1309-1331, 2009.
!
!  Parameters:
!
!    Input, character * ( * ) GMSH_FILENAME, the name of the Gmsh file.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_X(M,NODE_NUM), the node coordinates.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the nodes that make up each element.
!
  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order
  integer ( kind = 4 ) m
  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) element
  integer ( kind = 4 ) element_node(element_order,element_num)
  integer ( kind = 4 ) element_type
  character * ( * ) gmsh_filename
  integer ( kind = 4 ) gmsh_unit
  integer ( kind = 4 ) node
  real ( kind = 8 ) node_x(m,node_num)
  integer ( kind = 4 ) tag_num
  integer ( kind = 4 ) tag1
!
!  Enforce 1-based indexing of nodes.
!
  call mesh_base_one ( node_num, element_order, element_num, element_node )
!
!  Open the file.
!
  call get_unit ( gmsh_unit )

  open ( unit = gmsh_unit, file = gmsh_filename, status = 'replace' )
!
!  Write the data.
!
  write ( gmsh_unit, '(a)' ) '$MeshFormat'
  write ( gmsh_unit, '(a)' ) '2.2 0 8'
  write ( gmsh_unit, '(a)' ) '$EndMeshFormat'

  write ( gmsh_unit, '(a)' ) '$Nodes'
  write ( gmsh_unit, '(i6)' ) node_num
  do node = 1, node_num
    write ( gmsh_unit, '(i6,2x,g14.6,a)' ) &
      node, node_x(1:m,node), '  0.0  0.0'
  end do
  write ( gmsh_unit, '(a)' ) '$EndNodes'

  element_type = 1

  tag_num = 2
  tag1 = 0
  write ( gmsh_unit, '(a)' ) '$Elements'
  write ( gmsh_unit, '(i6)' ) element_num
  do element = 1, element_num
    write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,2(2x,i6))' ) &
      element, element_type, tag_num, tag1, element, &
      element_node(1:element_order,element)
  end do
  write ( gmsh_unit, '(a)' ) '$EndElements'

  close ( unit = gmsh_unit )

  return
end
subroutine gmsh_mesh2d_element_data_example ( element_num, element_order, &
  element_node )

!*****************************************************************************80
!
!! GMSH_MESH2D_ELEMENT_DATA_EXAMPLE returns element data for the 2D example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of elements.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the elements.
!
!    Output, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
!    the indices of the nodes that make up each element.
!
  implicit none

  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  integer ( kind = 4 ) element_node(element_order,element_num)
  integer ( kind = 4 ), dimension ( 3, 24 ) :: element_node_save = &
  reshape ( (/ &
    1,  2,  6, &
    7,  6,  2, &
    2,  3,  7, &
   8,  7,  3, &
    3,  4,  8, &
    9,  8,  4, &
    4,  5,  9, &
   10,  9,  5, &
    6,  7, 11, &
   12, 11,  7, &
    7,  8, 12, &
   13, 12,  8, &
    8,  9, 13, &
   14, 13,  9, &
    9, 10, 14, &
   15, 14, 10, &
   11, 12, 16, &
   17, 16, 12, &
   12, 13, 17, &
   18, 17, 13, &
   16, 17, 19, &
   20, 19, 17, &
   17, 18, 20, &
   21, 20, 18 /), (/ 3, 24 /) )

  call i4mat_copy ( element_order, element_num, element_node_save, &
    element_node )

  return
end
subroutine gmsh_mesh2d_element_size_example ( element_num, element_order )

!*****************************************************************************80
!
!! G