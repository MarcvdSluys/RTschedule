!> \file rtschedule.f90  Main routines for RT Schedule



!  Copyright (c) 2016-2019  Marc van der Sluys - han.vandersluys.nl
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
!> \brief  Module to share plot settings
module settings
  use SUFR_kinds, only: double
  implicit none
  save
  
  integer :: scaleType, plotSize, optTS,optTS0, majFr, opTex, printTable,asciiSchedule,plotSchedule,saveLaTeX
  real(double) :: fontSize
  character :: schedType*(19), plotType*(19)
  logical :: colour
  
end module settings
!***********************************************************************************************************************************


!***********************************************************************************************************************************
program schedule
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: syntax_quit, find_free_io_unit, file_open_error_quit
  use settings, only: schedType, optTS, optTS0, saveLaTeX
  
  implicit none
  integer, parameter :: nProcMax=19  ! Maximum number of processes to expect
  integer :: nFiles,iFile, np, time, ti(nProcMax),ci(nProcMax),di(nProcMax),pi(nProcMax), iSch, opTex, ioStat
  real(double) :: load
  character :: inFile*(199), scheds(4)*(9), name(nProcMax)*(9), fileBaseName*(99), ioMsg*(199)
  
  ! Initialise libSUFR:
  call set_SUFR_constants()
  
  ! See whether a file name was passed on the command line:
  nFiles = command_argument_count()
  if(nFiles.lt.1) call syntax_quit('<input file name>', 0, 'Simple realtime-scheduling tool for RMS, EDF and '// &
       'LST/LLF  -  rtschedule.sf.net')
  
  do iFile=1,nFiles
     call get_command_argument(iFile, inFile)
     
     ! Read the input file:
     call read_input_file(trim(inFile), nProcMax, name, ti,ci,di,pi, np,time, fileBaseName)
     
     
     if(saveLaTeX.ge.1) then  ! Open a .tex file to save LaTeX output
        write(*,'(A)') '  Saving LaTeX output as '//trim(fileBaseName)//'.tex'
        call find_free_io_unit(opTex)
        open(opTex, file=trim(fileBaseName)//'.tex', status='replace', iostat=ioStat, ioMsg=ioMsg)
        if(ioStat.ne.0) call file_open_error_quit(trim(fileBaseName)//'.tex', 0, ioStat, trim(ioMsg))
     end if
     
     ! Print some basic data about the system:
     call print_system_data(np,time, name,ti,ci,di,pi, load, trim(fileBaseName), opTex, 1)  ! Trial = 1
  
     if(optTS.ne.1) then
        optTS0 = optTS
        call rescale_task_list(np,time, ti,ci,di,pi)
        call print_system_data(np,time, name,ti,ci,di,pi, load, trim(fileBaseName), opTex, 2)  ! Trial = 2
        optTS = optTS0
     end if
  
     
     scheds = [character(len=9) :: 'RMS', 'EDF', 'LST', 'LLF']
     
     if(trim(schedType).eq.'ALL') then  ! Create all schedules (but not LST+LLF):
        do iSch=1,3
           call make_schedule(scheds(iSch), np,time, name, ti,ci,di,pi, load, fileBaseName,opTex)
        end do
     else  ! Create the specified schedule:
        call make_schedule(schedType, np,time, name, ti,ci,di,pi, load, fileBaseName,opTex)
     end if
     
     close(opTex)
     write(*,*)
  end do  ! iFile=1,nFiles
  
end program schedule
!***********************************************************************************************************************************


!***********************************************************************************************************************************
subroutine read_input_file(inFile, nProcMax, name, ti,ci,di,pi, np,time, fileBaseName)
  use SUFR_system, only: find_free_io_unit, file_open_error_quit, file_read_error_quit, file_read_end_error
  use SUFR_dummy, only: dumStr
  use settings, only: schedType, plotType, scaleType, plotSize, fontSize, colour, printTable,asciiSchedule,plotSchedule,saveLaTeX
  
  implicit none
  character, intent(in) :: inFile*(*)
  integer, intent(in) :: nProcMax
  integer, intent(out) :: ti(nProcMax),ci(nProcMax),di(nProcMax),pi(nProcMax), np,time
  character, intent(out) :: name(nProcMax)*(9), fileBaseName*(99)
  integer :: ip,ln, status, lastDot
  
  ! Open the input file:
  call find_free_io_unit(ip)
  open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(inFile),iostat=status)
  if(status.ne.0) call file_open_error_quit(trim(inFile), 1, 1)  ! 1: input file, 1: status: not ok
  
  ! Read file header:
  read(ip,'(A)') dumStr
  
  
  ! Read file body - plot settings:
  write(*,*)
  read(ip,*, iostat=status) dumStr, plotType
  if(status.ne.0) call file_read_end_error(trim(inFile), 3, status, 1, 1, message='expected variable: plotType')
  
  read(ip,*,iostat=status) dumStr, colour
  if(status.ne.0) call file_read_end_error(trim(inFile), 4, status, 1, 1, message='expected variable: colour')
  
  read(ip,*,iostat=status) dumStr, scaleType
  if(status.ne.0) call file_read_end_error(trim(inFile), 5, status, 1, 1, message='expected variable: scaleType')
  
  read(ip,*,iostat=status) dumStr, plotSize
  if(status.ne.0) call file_read_end_error(trim(inFile), 6, status, 1, 1, message='expected variable: plotSize')
  
  read(ip,*,iostat=status) dumStr, fontSize
  if(status.ne.0) call file_read_end_error(trim(inFile), 7, status, 1, 1, message='expected variable: fontSize')
  
  
  ! Read file body - scheduler settings:
  read(ip,*,iostat=status) dumStr, schedType
  if(status.ne.0) call file_read_end_error(trim(inFile), 9, status, 1, 1, message='expected variable: schedType')
  read(ip,*,iostat=status) dumStr, time
  if(status.ne.0) call file_read_end_error(trim(inFile), 10, status, 1, 1, message='expected variable: time')
  
  
  ! Read file body - output settings:
  read(ip,*,iostat=status) dumStr, printTable
  if(status.ne.0) call file_read_end_error(trim(inFile), 9, status, 1, 1, message='expected variable: printTable')
  read(ip,*,iostat=status) dumStr, asciiSchedule
  if(status.ne.0) call file_read_end_error(trim(inFile), 10, status, 1, 1, message='expected variable: asciiSchedule')
  read(ip,*,iostat=status) dumStr, plotSchedule
  if(status.ne.0) call file_read_end_error(trim(inFile), 10, status, 1, 1, message='expected variable: plotSchedule')
  read(ip,*,iostat=status) dumStr, saveLaTeX
  if(status.ne.0) call file_read_end_error(trim(inFile), 10, status, 1, 1, message='expected variable: saveLaTeX')
  
  
  ! Read task-list header:
  read(ip,'(A)') dumStr
  read(ip,'(A)') dumStr
  
  ! Read task list:
  do ln=1,nProcMax
     read(ip,*,iostat=status) name(ln), ti(ln),ci(ln),di(ln),pi(ln)
     if(status.lt.0) exit
     if(status.gt.0) call file_read_error_quit(trim(inFile), 12+ln, 0, ioMsg='expected row with task properties')
  end do  ! ln
  
  ! Close the file:
  close(ip)
  np = ln - 1  ! Number of processes/tasks found
  
  ! Get the base of the file name:
  fileBaseName = trim(inFile)
  lastDot = index(inFile,'.', back=.true.)  ! Position of the last dot in the file name
  if(lastDot.ne.0) fileBaseName = trim(inFile(1:lastDot-1))  ! Reomve file-name extension
  
end subroutine read_input_file
!***********************************************************************************************************************************
  



!***********************************************************************************************************************************
!> \brief  Print some basic data about the system

subroutine print_system_data(np,time, name,ti,ci,di,pi, load, fileBaseName, opTex, trial)
  use SUFR_kinds, only: double
  use SUFR_text, only: d2s
  use SUFR_numerics, only: gcd,lcm
  use SUFR_text, only: replace_substring
  use settings, only: optTS, majFr, saveLaTeX, optTS0
  
  implicit none
  integer, intent(in) :: np, ti(np),ci(np),di(np),pi(np), opTex, trial
  integer, intent(inout) :: time
  character, intent(in) :: name(np)*(9), fileBaseName*(*)
  real(double), intent(out) :: load
  integer :: pr
  real(double) :: frac
  character :: lFileBaseName*(len(fileBaseName)*2)
  
  ! Print task list:
  write(*,'(A)') '  **************************************************************************************************************'
  write(*,'(A,T110,A)') '  ***   GENERAL TASK DATA', '***'
  write(*,'(A)') '  **************************************************************************************************************'
  write(*,*)
  
  if(saveLaTeX.ge.1) then  ! Save LaTeX output
     
     ! Replace '_' with '\_'.  Must be done in two steps, since replacement string contains match string
     lFileBaseName = fileBaseName
     call replace_substring(lFileBaseName, '_', '&')
     call replace_substring(lFileBaseName, '&', '\_')
     
     if(trial.eq.1) then
        write(opTex,'(A)') '\section{RTschedule results for '//trim(lFileBaseName)//'}'
        write(opTex,'(A)') ''
        write(opTex,'(A)') '\subsection{General info}'
     end if
     write(opTex,'(A)') '\begin{table}[ht!]'
     write(opTex,'(A)') '  \centering'
     if(trial.eq.1) then
        write(opTex,'(A)') '  \caption{'
        write(opTex,'(A)') '    Task list for '//trim(lFileBaseName)//'.'
        write(opTex,'(A)') '    \label{tab:'//trim(fileBaseName)//'}'
        write(opTex,'(A)') '  }'
     else
        write(opTex,'(A)') '  \caption{'
        write(opTex,'(A,I0,A)') '    Task list for '//trim(lFileBaseName)//' with a timeslice of ',optTS0,' time units.'
        write(opTex,'(A)') '    \label{tab:'//trim(fileBaseName)//'_newTS}'
        write(opTex,'(A)') '  }'
     end if
     write(opTex,'(A)') '  \vspace*{1em}'
     write(opTex,'(A)') '  \begin{tabular}{l|rrrr}'
     write(opTex,'(4x,5(A5,A))') 'Task', '  & ', '$t_i$', '  & ','$c_i$', '  & ','$d_i$', '  & ','$p_i$', '  \\ \hline'
  end if
  write(*,*)
  write(*,'(2x,A)') 'Task list:'
  write(*,'(2x,9A5)') 'Name', 'ti','ci','di','pi'
  do pr=1,np
     write(*,'(2x,A5, 9I5)') trim(name(pr)), ti(pr),ci(pr),di(pr),pi(pr)
     if(saveLaTeX.ge.1) write(opTex,'(4x,A5,A, 4(I5,A))') trim(name(pr)), '  & ', ti(pr),'  & ',ci(pr),'  & ',di(pr),'  & ',pi(pr),'  \\'
  end do
  if(saveLaTeX.ge.1) then
     write(opTex,'(A)') '  \end{tabular}'
     write(opTex,'(A)') '\end{table}'
     write(opTex,'(A)') ''
  end if
  write(*,*)
  
  ! Print scheduling time:
  majFr = lcm(pi(1:np))
  if(time.le.0) then  ! Use major frame
     time = majFr
     write(*,'(A)') '  Using a major frame (hyperperiod) as the scheduling time.'
  end if
  write(*,'(2x,2(I0,A))') np, ' lines (processes) read; scheduling for ', time,' time units.'
  write(*,*)
  
  
  ! Print system load:
  if(trial.eq.1) then
     write(*,'(/,A)', advance='no') '  System load: '
     if(saveLaTeX.ge.1) then
        write(opTex,'(/,A)', advance='no') 'The system load for the task list in Table~\ref{tab:'//trim(fileBaseName)//'} is '
        if(trial.gt.1) write(opTex,'(A)', advance='no') 'still '
     end if
     load = 0.d0
     do pr=1,np
        frac = dble(ci(pr))/dble(pi(pr))
        load = load + frac
        write(*,'(A)', advance='no') d2s(frac,4)
        if(pr.lt.np) write(*,'(A)', advance='no') ' + '
        if(saveLaTeX.ge.1) then
           write(opTex,'(A)', advance='no') d2s(frac,4)
           if(pr.lt.np) write(opTex,'(A)', advance='no') ' + '
        end if
     end do
     write(*,'(A)') ' = '//d2s(load,4)
     if(saveLaTeX.ge.1) write(opTex,'(A)') ' = '//d2s(load,4)//'.'
     if(load.gt.1.d0) then
        write(*,'(A)') '  The task list can NOT be scheduled indefinately... :-('
        if(saveLaTeX.ge.1) write(opTex,'(A)') 'Hence, the task list can \textbf{not} be scheduled indefinately.'
     else
        write(*,'(A)') '  The task list CAN BE SCHEDULED in principle (is feasible) :-)'
        if(saveLaTeX.ge.1) write(opTex,'(A)') 'Hence, the task list can be scheduled in principle (\emph{i.e.}, it is feasible).'
     end if
     write(*,*)
  end if
  
  optTS = gcd(ci(1:np))
  write(*,*)
  write(*,'(A,I0,A)') '  Optimal timeslice: ', optTS, ' time units'
  write(*,'(A,I0,A)') '  Major frame: ', majFr, ' time units'
  write(*,'(A,I0,A)') '  Minor frame: ', gcd(pi(1:np)), ' time units'
  if(saveLaTeX.ge.1) then
     if(trial.eq.1) then
        write(opTex,'(A,I0,A)') 'The optimal timeslice is ', optTS, ' time units, '
        write(opTex,'(A,I0,A)') 'the major frame ', majFr, ' time units and '
        write(opTex,'(A,I0,A)') 'the minor frame ', gcd(pi(1:np)), ' time units.'
     else
        write(opTex,'(A,I0,A)') '\textbf{We now use a timeslice of ', optTS0, ' time units.}'
        write(opTex,'(A,I0,A)') 'The major frame has changed to ', majFr, ' timeslices and '
        write(opTex,'(A,I0,A)') 'the minor frame to ', gcd(pi(1:np)), ' timeslices.'
     end if
     write(opTex,*)
  end if
  write(*,*)
  
end subroutine print_system_data
!***********************************************************************************************************************************



!***********************************************************************************************************************************
subroutine rescale_task_list(np,time, ti,ci,di,pi)
  use SUFR_numerics, only: dne
  use settings, only: optTS
  
  implicit none
  integer, intent(in) :: np
  integer, intent(inout) :: time, ti(np),ci(np),di(np),pi(np)
  integer :: ip
  logical :: allOK
  
  write(*,*)
  write(*,'(A)') '  Scaling times and periods with the optimal timeslice.'
  write(*,'(A,I0,A)') '  This only gives the correct result if *all* numbers are divisible by ', optTS, ' - verifying...'
  
  allOK = .true.
  if(time/optTS .ne. nint(dble(time)/dble(optTS))) allOK = .false.
  if( dne( dble(time/optTS), dble(time)/dble(optTS), 0.1d0) ) allOK = .false.
  
  ip = 1
  do while(allOK .and. ip.le.np)
     if(ti(ip)/optTS .ne. nint(dble(ti(ip))/dble(optTS))) allOK = .false.
     if(ci(ip)/optTS .ne. nint(dble(ci(ip))/dble(optTS))) allOK = .false.
     if(di(ip)/optTS .ne. nint(dble(di(ip))/dble(optTS))) allOK = .false.
     if(pi(ip)/optTS .ne. nint(dble(pi(ip))/dble(optTS))) allOK = .false.
     
     if( dne( dble(ti(ip)/optTS), dble(ti(ip))/dble(optTS), 0.1d0) ) allOK = .false.
     if( dne( dble(ci(ip)/optTS), dble(ci(ip))/dble(optTS), 0.1d0) ) allOK = .false.
     if( dne( dble(di(ip)/optTS), dble(di(ip))/dble(optTS), 0.1d0) ) allOK = .false.
     if( dne( dble(pi(ip)/optTS), dble(pi(ip))/dble(optTS), 0.1d0) ) allOK = .false.
     ip = ip + 1
  end do
  
  
  if(allOK) then
     write(*,'(A,I0,A)') '  All numbers are divisible by ', optTS, ' - rescaling the task list, and recomputing general '// &
          'task data...'
     time = time/optTS
     ti(1:np) = ti(1:np)/optTS
     ci(1:np) = ci(1:np)/optTS
     di(1:np) = di(1:np)/optTS
     pi(1:np) = pi(1:np)/optTS
  else
     write(*,'(A,I0,A)') '  Not all numbers are divisible by ', optTS, ' - NOT rescaling the task list.'
  end if
  
  write(*,'(/)')
end subroutine rescale_task_list
!***********************************************************************************************************************************

