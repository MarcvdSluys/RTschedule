!***********************************************************************************************************************************
program schedule
  !use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit, syntax_quit
  use SUFR_dummy, only: dumStr
  
  implicit none
  integer, parameter :: nLines=99
  integer :: status,ip,ln, ti(nLines),ci(nLines),di(nLines),pi(nLines)
  !real(double) :: dat(nLines)
  character :: inFile*(99), name(nLines)
  
  call set_SUFR_constants()
  
  if(command_argument_count().ne.1) call syntax_quit('<input file name>', 0, 'Simple scheduling tool for LLF')
  call get_command_argument(1, inFile)
  
  call find_free_io_unit(ip)
  open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(inFile),iostat=status)
  if(status.ne.0) call file_open_error_quit(trim(inFile), 1, 1)  ! 1: input file, 1: status: not ok
  
  ! Read file header:
  do ln=1,1
     read(ip,'(A)') dumStr
  end do
  
  ! Read file body:
  do ln=1,nLines
     read(ip,*,iostat=status) name(ln), ti(ln),ci(ln),di(ln),pi(ln)
     if(status.lt.0) exit
     if(status.gt.0) call file_read_error_quit(trim(inFile), ln, 0)
     write(*,*) name(ln), ti(ln),ci(ln),di(ln),pi(ln)
  end do  ! ln
  close(ip)
end program schedule
!***********************************************************************************************************************************
