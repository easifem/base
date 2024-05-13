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

option(USE_LIS OFF)
if(USE_LIS)

  list(APPEND TARGET_COMPILE_DEF "-DUSE_LIS")

  find_library(LIS_LIBRARIES NAME lis REQUIRED)
  find_path(LIS_INCLUDE_DIR NAMES lis_config.h lisf.h lis.h)

  target_link_libraries(${PROJECT_NAME} PUBLIC ${LIS_LIBRARIES})
  message(STATUS "LIS_LIBRARIES : ${LIS_LIBRARIES}")

  target_include_directories(${PROJECT_NAME} PUBLIC ${LIS_INCLUDE_DIR})
  message(STATUS "LIS_INCLUDE_DIR : ${LIS_INCLUDE_DIR}")

else()

  message(STATUS "NOT USING LIS LIBRARIES")

endif()
