module write_vars

    use CaseConverter, only : to_upper
    use globals      , only : variable
    
    implicit none

    private
    public :: ctl_vars

    contains


    subroutine ctl_vars(unit, znum)
        integer, intent(in) :: unit
        integer, intent(in) :: znum

        character(32) :: variable_uppere

        call to_upper(variable      , &  !! IN
                    & variable_upper  )  !! OUT

        write(unit,'(A)')          'VARS 1'
        write(unit,'(A,x,I0,x,A)') trim(variable), znum, '99 ' // trim(variable_upper)
        write(unit,'(A)')          'ENDVARS'

    end subroutine ctl_vars


end module write_vars

