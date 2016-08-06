# RT Schedule: a real-time schedule generator for educational purposes#

RT Schedule is a realtime-schedule generator to create and present simple and idealised RT schedules from task lists.  The purpose is to compute the basic properties of a task set and create a schedule using the rate monotonic (RM), earliest deadline first (EDF) and least laxity first (LLF) algorithms.  The program uses simple, idealised systems, since students must be able to schedule them manually.  RT Schedule computes the data and generates clear graphs for lecture notes and exams.

The program is written in Fortran, needs the [libSUFR](http://libsufr.sourceforge.net) package and uses [PLplot](http://plplot.sourceforge.net/) to generate graphics.  RT Scheduler can be run from the command line, and requires an input file.  An example input file example.dat is provided.  The output looks as follows:

    
	$ ./schedule example.dat
	
	
      **************************************************************************************************************
      ***   GENERAL TASK DATA                                                                                    ***
      **************************************************************************************************************
    
      Task list:
       Name   ti   ci   di   pi
         T1    0    1    5    5
         T2    0    2   10   10
         T3    0    5   15   15
    
      3 lines (processes) read; scheduling for 30 time units.
    
    
      System load: 0.2000 + 0.2000 + 0.3333 = 0.7333
      The task list is SCHEDULABLE in principle (feasible) :-)
    
    
      Optimal timeslice: 1 time units
      Major frame: 30 time units
      Minor frame: 5 time units
    
    
    
      **************************************************************************************************************
      ***   RM SCHEDULER                                                                                         ***
      **************************************************************************************************************
    
      RM priorities:
       Name   ti   ci   di   pi prio
         T1    0    1    5    5    1
         T2    0    2   10   10    2
         T3    0    5   15   15    3
    
      RM schedulability test for n=3:  SUM Ci/Pi <= n (2^(1/n) - 1):
        0.733 <= 0.780, so task set is GUARANTEED to be schedulable with RM.
      0-1          _1_  e      2   e      5   e     run: 1
      1-2           0         _2_         5         run: 2  switch (task T1 done)
      2-3           0         _1_         5         run: 2
      3-4           0          0         _5_        run: 3  switch (task T2 done)
      4-5           0          0         _4_        run: 3
      5-6          _1_  e      0          3         run: 1  switch (task T3: 3>)
      6-7           0          0         _3_        run: 3  switch (task T1 done)
      7-8           0          0         _2_        run: 3
      8-9           0          0         _1_        run: 3
      9-10          0          0          0         run: 0  switch (task T3 done)
      10-11        _1_  e      2   e      0         run: 1  switch
      11-12         0         _2_         0         run: 2  switch (task T1 done)
      12-13         0         _1_         0         run: 2
      13-14         0          0          0         run: 0  switch (task T2 done)
      14-15         0          0          0         run: 0
      15-16        _1_  e      0          5   e     run: 1  switch
      16-17         0          0         _5_        run: 3  switch (task T1 done)
      17-18         0          0         _4_        run: 3
      18-19         0          0         _3_        run: 3
      19-20         0          0         _2_        run: 3
      20-21        _1_  e      2   e      1         run: 1  switch (task T3: 1>)
      21-22         0         _2_         1         run: 2  switch (task T1 done)
      22-23         0         _1_         1         run: 2
      23-24         0          0         _1_        run: 3  switch (task T2 done)
      24-25         0          0          0         run: 0  switch (task T3 done)
      25-26        _1_  e      0          0         run: 1  switch
      26-27         0          0          0         run: 0  switch (task T1 done)
      27-28         0          0          0         run: 0
      28-29         0          0          0         run: 0
      29-30         0          0          0         run: 0
    
      RM ASCII schedule:
      T1   #    #    #    #    #    #    
      T2    ##        ##        ##       
      T3      ## ###       ####   #      
       t  0    5   10   15   20   25   30
    
      RM ASCII schedule:
      T1   #        e#        e#        e#        e#        e#        e
      T2     # #              e  # #              e  # #              e
      T3         # #   # # #            e  # # # #       #            e
       t  0         5        10        15        20        25        30
    
      Graphical schedule saved as example_schedule_RM.png
    
      No deadlines were missed: the system can be scheduled for 30 time units.
      16 task switches (1.76 time units per run).
    
