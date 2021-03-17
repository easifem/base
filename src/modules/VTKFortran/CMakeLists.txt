# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 16/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/vtk_fortran.f90
  ${src_path}/vtk_fortran_vtk_file.f90
  ${src_path}/vtk_fortran_vtm_file.f90
  ${src_path}/vtk_fortran_pvtk_file.f90
  ${src_path}/vtk_fortran_parameters.f90
  ${src_path}/vtk_fortran_dataarray_encoder.f90
  ${src_path}/vtk_fortran_vtk_file_xml_writer_abstract.f90
  ${src_path}/vtk_fortran_vtk_file_xml_writer_appended.f90
  ${src_path}/vtk_fortran_vtk_file_xml_writer_ascii_local.f90
  ${src_path}/vtk_fortran_vtk_file_xml_writer_binary_local.f90
)