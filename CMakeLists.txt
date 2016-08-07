##
##  CMakeFile for RTschedule - rtschedule.sf.net
##  Marc van der Sluys, 2016-08-07
##
##  To compile, from the directory that contains this file, do:
##    $ mkdir build; cd build
##    $ cmake ..
##    $ make
##


cmake_minimum_required( VERSION 2.6 FATAL_ERROR )

# Set build type. Do this *before* we set the project name:
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE Release CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE )
endif( NOT CMAKE_BUILD_TYPE )
set( CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal" )


# Project name and language:
project( RTschedule Fortran )


# Search in the CMake/ directory for CMake modules:
list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake )


# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Set directories:
# Place the products in their directories:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/mod" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}" )



# Find dependencies:
# libSUFR:
find_package( LibSUFR REQUIRED )
set( INCLUDE_FLAGS "-I${LibSUFR_INCLUDES}" )  # will be transferred to CompilerFlags

# PGPlot:
find_package( PLplot )
set( INCLUDE_FLAGS "${INCLUDE_FLAGS} -I${PLplot_INCLUDES}" )  # will be transferred to CompilerFlags





# Set source files:
include( FileList )

# Set FORTRAN compiler flags:
include( CompilerFlags_Fortran )




# Code-version generator:
# This implies that the file code_version.f90 is removed by 'make clean'!
#if( CREATE_VERSION )
#  
#  set_source_files_properties( ${CMAKE_SOURCE_DIR}/src/code_version.f90 PROPERTIES GENERATED 1 )  # Tell CMake this source file won't be available until build time.
#  
#  add_custom_command(
#    OUTPUT ${CMAKE_SOURCE_DIR}/src/code_version.f90
#    COMMAND cd $(CMAKE_SOURCE_DIR)
#    COMMAND . ${CMAKE_SOURCE_DIR}/code_version.sh $(CMAKE_SOURCE_DIR) src/code_version.f90  yes  ${Fortran_COMPILER_NAME} ${OPT_FLAGS}
#    )
#  
#endif( CREATE_VERSION )


# Use PLplot if found, otherwise use PGPLOT:
if( PLplot_FOUND )
  set( Plot_LIBRARIES ${PLplot_LIBRARIES} )
  message( STATUS "Linking against PLplot, including support for graphical output" )
else( PLplot_FOUND )
  set( Plot_LIBRARIES ${PGPLOT_LIBRARIES} )
  message( STATUS "PLplot libraries not found, compiling without support for graphical output" )
endif( PLplot_FOUND )


# Build, link and install RTschedule executable:
add_executable( rtschedule ${RTschedule_SRC_FILES} ${Plot_SRC_FILES} )
target_link_libraries( rtschedule ${LibSUFR_LIBRARIES} ${Plot_LIBRARIES} )
set_target_properties( rtschedule PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )
install( TARGETS "rtschedule" RUNTIME DESTINATION "bin" )


# Place the executable in the project's root directory:
set( EXECUTABLE_OUTPUT_PATH ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} )


#add_test( RTscheduleruns RTschedule )

