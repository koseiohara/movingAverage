program main

    use namelist, only : read_nml, minuteTaker
    use movave  , only : movingAverage

    implicit none

    real(4) :: start_time
    real(4) :: end_time

    call cpu_time(start_time)

    call read_nml()

    call minuteTaker()

    call movingAverage()

    call cpu_time(end_time)

    write(*,'(a,f0.3,a)') 'Execution Time : ', end_time - start_time, ' s'

end program main

