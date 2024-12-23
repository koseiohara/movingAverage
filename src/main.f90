program main

    use globals   , only : nx, ny, nz, filterlen, even_center, tnum, &
                         & input_fname, output_fname               , &
                         & variable, datetime_init, options        , &
                         & xmin, ymin, xstep, ystep, zlevels, tstep
    use namelist  , only : read_nml, minuteTaker
    use mkctl     , only : auto_ctl
    use write_vars, only : ctl_vars
    use movave    , only : movingAverage

    implicit none

    real(4) :: start_time
    real(4) :: end_time

    character(256) :: title
    character(8)   :: tstep_char

    write(*,'(A)') 'START PROCESS'

    call cpu_time(start_time)

    call read_nml()

    call minuteTaker()

    call movingAverage()

    write(title,'(I0,A)') filterlen, '-records Moving Average of ' // trim(variable) // ' in t-direction'
    if (mod(filterlen,2) == 0) then
        title = trim(title) //  ' with ' // trim(even_center) // ' type filter'
    endif
    title = trim(title) // ': Generated from ' // trim(input_fname)
    write(tstep_char,'(I0,"hr")') tstep
    call auto_ctl(bin       =output_fname , &  !! IN
                & title     =title        , &  !! IN
                & options   =options      , &  !! IN
                & xnum      =nx           , &  !! IN
                & xmin      =xmin         , &  !! IN
                & xstep     =xstep        , &  !! IN
                & ynum      =ny           , &  !! IN
                & ymin      =ymin         , &  !! IN
                & ystep     =ystep        , &  !! IN
                & znum      =nz           , &  !! IN
                & zlevels   =zlevels      , &  !! IN
                & tnum      =tnum         , &  !! IN
                & tini      =datetime_init, &  !! IN
                & tstep     =tstep_char   , &  !! IN
                & write_vars=ctl_vars       )  !! IN

    call cpu_time(end_time)

    write(*,'(a,f0.3,a)') 'Execution Time : ', end_time - start_time, ' s'

end program main

