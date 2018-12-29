!  Copyright (c) 2016-2017  Marc van der Sluys - han.vandersluys.nl
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
!> \brief  Plot a graphical schedule

subroutine plot_schedule(sched, np,time, name,ti,pi,di, ccs,run,laxs, fileBaseName,opTex)
  use SUFR_kinds, only: double
  use SUFR_system, only: quit_program_error
  use plplot, only: plsdev, plsfnam, plbox, plmtex,plfill,plptex, plpoin
  use plplot, only: plspage,plinit,plbop,plvpor,plwind,plwidth,pllsty,plschr,plcol0,plssym,plend
  use settings, only: plotType, colour, optTS, fontSize, saveLaTeX
  
  implicit none
  integer, intent(in) :: np,time, ti(np),pi(np),di(np), ccs(np,-1:time),run(-1:time),laxs(np,0:time), opTex
  character, intent(in) :: sched*(9), name(np)*(9), fileBaseName*(99)
  integer :: it, pr, xSize,ySize
  real(double) :: xMarg1,xMarg2,yMarg1,yMarg2
  character :: tmpStr*(9), plFileName*(99)
  
  
  call pl_square_grid(time,np, xSize,ySize, xMarg1,xMarg2, yMarg1,yMarg2, fontSize)  ! time x np boxes
  
  
  select case(trim(plotType))
  case('eps','pdf','png','svg')  ! All valid types
     plFileName = trim(fileBaseName)//'_schedule_'//trim(sched)//'.'//trim(plotType)  ! Add to the base name of the input file
     call plsdev(trim(plotType)//'cairo')                                             ! Set plotting device, use cairo
  case default
     call quit_program_error('Plot type unknown: '//trim(plotType)//' - use one of png, pdf, svg', 1)
  end select
  call plsfnam(trim(plFileName))                  ! Set file name
  
  
  call plspage(0.d0,0.d0, xSize,ySize, 0,0)       ! Set page size: dpi, size, offset (px/mm)
  call plmycolours()                              ! White bg, proper colours
  
  call plinit()                                   ! Initialise environment; Call after plsdev(), plssub(), plspage()
  call plbop()                                    ! Begin a new page
  call plvpor(xMarg1,xMarg2, yMarg1,yMarg2)       ! Set view port in plot window
  call plwind(0.d0,dble(time), dble(np),0.d0)     ! Set view port in world coordinates
  
  call plwidth(1.d0*fontSize)                     ! Normal line width
  call pllsty(1)                                  ! Full lines
  call plschr(5.d0, fontSize)
  
  do it=1,time
     
     ! Fill a square:
     call plcol0(10)                              ! Grey squares
     if(run(it).ne.0) then
        call plfill( dble([it-1,it-1,it,it]), dble([run(it),run(it)-1,run(it)-1,run(it)]) )
     end if
     
     ! Print remaining cpu time:
     if(it.gt.1) then                         ! Can't have a task switch in timeslice 0-1
        pr = run(it-1)                        ! Task running before the switch
        if(run(it).ne.pr .and. pr.ne.0) then  ! There was a task switch
           if(ccs(pr,it-1).gt.1) then         ! CPU time left for the old task in the previous timeslice
              !                                 if(ccs(pr,it).gt.0) won't work if di == pi and laxity=0
              
              call plcol0(1)                              ! Black text
              write(tmpStr,'(I0,A)') ccs(pr,it)*optTS, '>'
              call plptex(dble(it-1), dble(pr-0.5d0), 1.d0,0.d0, 1.d0, trim(tmpStr))
              
           end if
        end if
     end if
     
  end do  ! it
  
  
  call plcol0(1)                              ! Black box
  if(optTS.ne.1) call plwind(0.d0,dble(time*optTS), dble(np),0.d0)  ! Set view port in world coordinates
  call plbox('BCGHNT', 5.d0*optTS, 5,  'BCG', 1.d0, 0)              ! Plot box
  if(optTS.ne.1) call plwind(0.d0,dble(time), dble(np),0.d0)        ! Set view port in world coordinates
  
  
  if(colour) call plcol0(2)                        ! Red arrows, dots and crosses
  call plwidth(2.0d0*fontSize)                    ! Thicker lines
  do it=0,time
     
     do pr=1,np
        
        ! Mark event (up arrow, as in SimSo - len 0.25, angle 25d):
        if( mod( ti(pr)-it + pi(pr)*1000, pi(pr)).eq.0 )  call plarrow(2,  dble([it,it]), dble([pr,pr-1]), 0.25d0*fontSize, 25.d0)
        
        ! Mark deadline (down arrow, as in SimSo):
        if( mod( ti(pr)+di(pr)-it + pi(pr)*1000, pi(pr)).eq.0 )  then
           call plarrow( 2, dble([it,it]), dble([pr-1,pr]), 0.25d0*fontSize, 25.d0)  ! len = 0.25, angle 25d
           ! Mark missed deadlines:
           if(it.gt.0) then
              if( (pr.eq.run(it) .and. ccs(pr,it).gt.1) .or. (pr.ne.run(it) .and. ccs(pr,it).gt.0) ) then  ! pr misses deadl @t=it
                 call plssym(10.d0, fontSize)  ! Huge symbols
                 call plpoin(dble([it]), dble([pr])-0.5d0, 5)  ! Cross
                 call plssym(5.d0, fontSize)  ! Default symbol size
              end if
           end if
           
        end if

        ! Print laxity in plot for LST/LLF:
        call plschr(3.d0, fontSize)  ! Small font
        if(sched.eq.'LST'.or.sched.eq.'LLF') then
           write(tmpStr,'(I0)') laxs(pr,it)
           if(ccs(pr,it+1).eq.0) tmpStr = '-'  ! Job is done
           call plptex(dble(it)+0.25d0, dble(pr-0.85d0), 1.d0,0.d0, 1.d0, trim(tmpStr))
        end if

        ! Print current CPU time:
        !write(tmpStr,'(I0)') ccs(pr,it)
        !if(ccs(pr,it).eq.0) tmpStr = '-'  ! Job is done
        !call plptex(dble(it)-0.1d0, dble(pr-0.9d0), 1.d0,0.d0, 1.d0, trim(tmpStr))
        
        call plschr(5.d0, fontSize)  ! Default font size
     end do  ! pr
     
     ! Mark a completed task:
     if(it.gt.1) then                         ! Can't have a task switch in timeslice 0-1
        pr = run(it-1)                        ! Task running before the switch
        if(run(it).ne.pr .and. pr.ne.0) then  ! There must be a task switch
           if(ccs(pr,it-1).le.1) then         ! Less than 1 time unit left for old task in the previous timeslice
              !                                 if(ccs(pr,it).le.0) won't work if di == pi and laxity=0
              call plssym(7.d0, fontSize)     ! Larger symbols
              call plpoin([dble(it-1)], [dble(pr)], 17)
              call plssym(5.d0, fontSize)     ! Default symbol size
           end if
        end if
        if(it.eq.time .and. ccs(run(it),it).eq.1) then  ! End of the schedule, and the running task has 1 TS left
           call plssym(7.d0, fontSize)     ! Larger symbols
           call plpoin([dble(it)], [dble(pr)], 17)
           call plssym(5.d0, fontSize)     ! Default symbol size
        end if
     end if
     
  end do  ! it
  
  call plwidth(1.d0*fontSize)                      ! Normal line width
  
  
  
  
  ! Print axis labels:
  call plcol0(1)                                   ! Black text
  call plmtex('B', 3.5d0, 0.5d0,0.5d0, 'time')     ! Plot label for horizontal axis
  call plmtex('L', 3.5d0, 0.5d0,0.5d0, 'task')     ! Plot label for vertical axis
  
  
  ! Print the task names:
  do pr=1,np
     call plmtex('LV', 1.5d0, (dble(np-pr)+0.5d0)/dble(np), 0.5d0, trim(name(pr)))  ! Plot label for vertical axis
  end do
  
  call plend()                                ! Finish plot

  write(*,'(A)') '  Graphical schedule saved as '//trim(plFileName)
  if(saveLaTeX.ge.1) then
     write(opTex,'(A)') ''
     write(opTex,'(A)') '\begin{figure}[ht!]'
     write(opTex,'(A)') '  \centering'
     write(opTex,'(A)') '  \pgfimage[interpolate=true,width=0.9\textwidth]{'//trim(plFileName)//'}'
     write(opTex,'(A)') '  \caption{Schedule for '//trim(sched)//'.}'
     write(opTex,'(A)') '\end{figure}'
     write(opTex,'(A)') ''
  end if
  
end subroutine plot_schedule
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Compute the image and margin size for a PLplot plot with given plot-box size and margins in pixels
!!

subroutine pl_square_grid(nx,ny, xSize,ySize, xMarg1,xMarg2, yMarg1,yMarg2, fontSize)
  use SUFR_system, only: quit_program_error
  use plplot, only: plflt
  use settings, only: scaleType, plotSize
  
  implicit none
  integer, intent(in) :: nx,ny
  integer, intent(out) :: xSize,ySize
  real(plflt), intent(in) :: fontSize
  real(plflt), intent(out) :: xMarg1,xMarg2, yMarg1,yMarg2
  integer :: marg1,marg2, sumMargs, xPlBox
  real(plflt) :: rat
  
  marg1 = 65  ! Left/bottom margin - needs space for plot labels. 65 reasonable for 1000px wide, plschr(5.d0)
  marg2 = 15  ! Right/top margin - needs space for large number labels sticking out. 15 "
  sumMargs = nint( (marg1 + marg2) * fontSize )  ! All scaled margins
  
  select case(scaleType)
  case(1)
     xSize = plotSize
     
     xPlBox = xSize - sumMargs  ! Size of the plot box (excluding margins) in pixels
     rat = dble(ny)/dble(nx)
     ySize = nint(xPlBox * rat) + sumMargs
     
  case(2)
     xSize = nx * plotSize + sumMargs  ! Image size in pixels
     ySize = ny * plotSize + sumMargs 
     
  case default
     call quit_program_error('pl_square_grid():  Unknown scale type', 1)
  end select
  
  
  ! Compute the relative margins:
  xMarg1 =        dble(marg1)/dble(xSize) * fontSize  ! Fractional margins for plvpor()
  xMarg2 = 1.d0 - dble(marg2)/dble(xSize) * fontSize
  yMarg1 =        dble(marg1)/dble(ySize) * fontSize  ! Fractional margins for plvpor()
  yMarg2 = 1.d0 - dble(marg2)/dble(ySize) * fontSize
  
end subroutine pl_square_grid
!***********************************************************************************************************************************
  

!***********************************************************************************************************************************
!> \brief  Create a white background and define my colours in PLplot

subroutine plmycolours()
  use PLplot, only: plscol0
  implicit none
  
  call plscol0(0,  255,255,255)  ! Default BG, white
  call plscol0(1,  0,0,0)        ! Default foreground, black
  call plscol0(2,  255,0,0)      ! Red
  call plscol0(3,  0,191,0)      ! Green for white bg
  call plscol0(4,  0,0,255)      ! Blue
  call plscol0(5,  0,191,191)    ! Magenta -> darker for white bg
  call plscol0(6,  191,0,191)    ! Cyan -> darker
  call plscol0(7,  255,127,0)    ! Orange
  call plscol0(8,  63,255,63)    ! Light green
  call plscol0(9,  127,63,0)     ! Brown
  call plscol0(10, 150,150,150)  ! Light grey for white bg (170)
  call plscol0(11, 84,84,84)     ! Dark grey for white bg
  call plscol0(12, 255,127,127)  ! Pink/salmon
  call plscol0(13, 150,0,0)      ! Dark red
  call plscol0(14, 255,0,255)    ! Cyan
  call plscol0(15, 255,255,0)    ! Yellow
  
end subroutine plmycolours
!***********************************************************************************************************************************


!***********************************************************************************************************************************
subroutine plarrow(Narr, Xarr,Yarr, arrowLen, angle)
  use SUFR_constants, only: d2r !,r2d
  use plplot, only: plflt, plline, plfill, plpsty
  
  implicit none
  integer, intent(in) :: Narr
  real(kind=plflt), intent(in) :: Xarr(Narr),Yarr(Narr), arrowLen,angle
  real(kind=plflt) :: dX,dY, dX1,dX2, dY1,dY2, alpha,alpha1,alpha2, lArrowLen
  
  
  call plline(Xarr, Yarr)        ! Plot line
  
  dX = Xarr(Narr) - Xarr(Narr-1)
  dY = Yarr(Narr) - Yarr(Narr-1)
  
  alpha = atan2(dY,dX)
  alpha1 = alpha - angle*d2r
  alpha2 = alpha + angle*d2r
  
  lArrowLen = min(arrowLen, 0.45d0)
  
  dX1 = -lArrowLen * cos(alpha1)
  dY1 = -lArrowLen * sin(alpha1)
  dX2 = -lArrowLen * cos(alpha2)
  dY2 = -lArrowLen * sin(alpha2)
  
  !call plline([Xarr(Narr), Xarr(Narr)+dX1], [Yarr(Narr), Yarr(Narr)+dY1])        ! Plot first head line
  !call plline([Xarr(Narr), Xarr(Narr)+dX2], [Yarr(Narr), Yarr(Narr)+dY2])        ! Plot second head line
  !
  !call plline([Xarr(Narr)+dX1, Xarr(Narr)+dX2], [Yarr(Narr)+dY1, Yarr(Narr)+dY2])        ! Plot third line for triangular head
  
  call plpsty(0)                                                                                           ! Filled arrow head
  call plfill([Xarr(Narr), Xarr(Narr)+dX1, Xarr(Narr)+dX2], [Yarr(Narr), Yarr(Narr)+dY1, Yarr(Narr)+dY2])  ! Plot line
  
  
end subroutine plarrow
!***********************************************************************************************************************************
