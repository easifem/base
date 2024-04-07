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

# TARGET PROPerties
set_target_properties(
  ${PROJECT_NAME}
  PROPERTIES POSITION_INDEPENDENT_CODE 1
             SOVERSION ${VERSION_MAJOR}
             OUTPUT_NAME ${PROJECT_NAME}
             LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
             MACOSX_RPATH ON
             WINDOWS_EXPORT_ALL_SYMBOLS ON)

# TARGET PROPerties
set_target_properties(
  easifemSystemMethodC
  PROPERTIES POSITION_INDEPENDENT_CODE 1
             SOVERSION ${VERSION_MAJOR}
             OUTPUT_NAME ${PROJECT_NAME}
             LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
             MACOSX_RPATH ON
             WINDOWS_EXPORT_ALL_SYMBOLS ON)
