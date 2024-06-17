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

if(USE_LAPACK95)

  find_package(LAPACK95 REQUIRED)

  if(LAPACK95_FOUND)
    message(STATUS "[INFO] :: FOUND LAPACK95")
    target_link_libraries(${PROJECT_NAME} PUBLIC LAPACK95::LAPACK95)
    list(APPEND TARGET_COMPILE_DEF "-DUSE_LAPACK95")

  else()
    message(ERROR "[ERROR] :: NOT FOUND LAPACK95")

  endif()

endif()
