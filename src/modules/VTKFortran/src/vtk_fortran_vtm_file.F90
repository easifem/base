!< VTM file class.
! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE VTK_FORTRAN_VTM_FILE
USE BEFOR64
USE PENF
USE VTK_FORTRAN_VTK_FILE_XML_WRITER_ABSTRACT
USE VTK_FORTRAN_VTK_FILE_XML_WRITER_ASCII_LOCAL
IMPLICIT NONE
PRIVATE
PUBLIC :: VTM_FILE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

type :: vtm_file
  class(xml_writer_abstract), allocatable, public :: xml_writer
    !! XML writer.
  contains
    procedure, pass(self) :: initialize
      !! Initialize file.
    procedure, pass(self) :: finalize
      !! Finalize file.
    generic :: write_block => write_block_array, write_block_string
      !! Write one block dataset.
    procedure, pass(self), private :: write_block_array
      !! Write one block dataset (array input).
    procedure, pass(self), private :: write_block_string
      !! Write one block dataset (string input).
endtype vtm_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

contains
  ! public methods
  function initialize(self, filename) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize file (writer).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout) :: self     !< VTM file.
  character(*),    intent(in)    :: filename !< File name of output VTM file.
  integer(I4P)                   :: error    !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  if (allocated(self%xml_writer)) deallocate(self%xml_writer)
  allocate(xml_writer_ascii_local :: self%xml_writer)
  error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology='vtkMultiBlockDataSet')
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize

  function finalize(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize file (writer).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout) :: self  !< VTM file.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = 1
  if (allocated(self%xml_writer)) error = self%xml_writer%finalize()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize

  ! private methods
  function write_block_array(self, filenames, names, name) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write one block dataset (array input).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], name='my_block')
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], &
  !<                         names=['block-bar', 'block-foo', 'block-baz'], name='my_block')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self          !< VTM file.
  character(*),    intent(in)           :: filenames(1:) !< File names of VTK files grouped into current block.
  character(*),    intent(in), optional :: names(1:)     !< Auxiliary names attributed to each files.
  character(*),    intent(in), optional :: name          !< Block name
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = self%xml_writer%write_parallel_open_block(name=name)
  error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
  error = self%xml_writer%write_parallel_close_block()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_block_array

  function write_block_string(self, filenames, names, name) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write one block dataset (string input).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', name='my_block')
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', names='block-bar block-foo block-baz', name='my_block')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self      !< VTM file.
  character(*),    intent(in)           :: filenames !< File names of VTK files grouped into current block.
  character(*),    intent(in), optional :: names     !< Auxiliary names attributed to each files.
  character(*),    intent(in), optional :: name      !< Block name
  integer(I4P)                          :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = self%xml_writer%write_parallel_open_block(name=name)
  error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
  error = self%xml_writer%write_parallel_close_block()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_block_string
ENDMODULE VTK_FORTRAN_VTM_FILE
