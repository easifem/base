# This program is a part of EASIFEM library Expandable And Scalable
# Infrastructure for Finite Element Methods htttps://www.easifem.com Vikas
# Sharma, Ph.D., vickysharma0812@gmail.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(${PROJECT_NAME} PRIVATE ${src_path}/TriangleInterface.F90)

add_library(easifemTriangle STATIC
  ${src_path}/triangle.c
  ${src_path}/report.c
)

target_link_libraries(${PROJECT_NAME} PUBLIC easifemTriangle)

list(APPEND TRIANGLE_COMPILE_DEF "-DTRILIBRARY" "-DANSI_DECLARATORS" 
"-DNO_TIMER")

if(USE_Real64)
  list(APPEND TRIANGLE_COMPILE_DEF "-DUSE_Real64")
endif()

if(LINUX)
  list(APPEND TRIANGLE_COMPILE_DEF "-DLINUX")

elseif(WIN32)
  list(APPEND TRIANGLE_COMPILE_DEF "-DCPU86")

endif()

target_compile_definitions(easifemTriangle PRIVATE ${TRIANGLE_COMPILE_DEF})

list(APPEND TRIANGLE_COMPILE_OPTIONS "-O3")

target_compile_options(easifemTriangle PRIVATE ${TRIANGLE_COMPILE_OPTIONS})

# target properties
set_target_properties(
  easifemTriangle
  PROPERTIES POSITION_INDEPENDENT_CODE 1
             SOVERSION ${VERSION_MAJOR}
             # OUTPUT_NAME ${PROJECT_NAME}
             LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
             MACOSX_RPATH ON
             WINDOWS_EXPORT_ALL_SYMBOLS ON
             LINKER_LANGUAGE C )

list(APPEND C_PROJECTS "easifemTriangle")
