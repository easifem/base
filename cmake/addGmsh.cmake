# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
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

option(USE_GMSH_SDK OFF)
if(USE_GMSH_SDK)

  message(STATUS "USING GMSH SDK")
  list(APPEND TARGET_COMPILE_DEF "-DUSE_GMSH_SDK")

  find_library(GMSH_LIBRARIES NAMES gmsh gmsh.4.13.0 gmsh.4.13 REQUIRED)

  target_link_libraries(${PROJECT_NAME} PUBLIC ${GMSH_LIBRARIES})
  message(STATUS "GMSH_LIBRARIES : ${GMSH_LIBRARIES}")

else()

  message(STATUS "NOT USING GMSH SDK")

endif()
