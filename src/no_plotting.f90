!  Copyright (c) 2016  Marc van der Sluys - han.vandersluys.nl
!   
!  This file is part of the RT Schedule package, 
!  see: http://rtschedule.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.


!***********************************************************************************************************************************
!> \brief  Dummy routine to compile the code without linking to PLplot

subroutine plot_scheduler(sched, np,time, name,ti,pi,di, ccs,run, fileBaseName)
  
  implicit none
  integer, intent(in) :: np,time, ti(np),pi(np),di(np), ccs(np,time),run(time)
  character, intent(in) :: sched*(9), name(np)*(9), fileBaseName*(99)
  integer :: tmpInt
  character :: tmpStr*(99)
  
  ! Avoid 'unused variable' compiler warnings:
  tmpInt = np
  tmpInt = time
  
  tmpInt = ti(1)
  tmpInt = pi(1)
  tmpInt = di(1)
  
  tmpInt = ccs(1,1)
  tmpInt = run(1)
  
  tmpStr = trim(sched)
  tmpStr = trim(name(1))
  tmpStr = trim(fileBaseName)
  
end subroutine plot_scheduler
!***********************************************************************************************************************************

