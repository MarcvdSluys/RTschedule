set( RTschedule_SRC_FILES
  src/rtschedule.f90
  src/make_schedule.f90
  )


# Source files specific to PLplot:
if( PLplot_FOUND )
  set( Plot_SRC_FILES
    src/plotting.f90
    )
else( PLplot_FOUND )
  set( Plot_SRC_FILES
    src/no_plotting.f90
    )
endif( PLplot_FOUND )

