module movave_filter

    use globals, only : kp, filterlen, filtername

    implicit none

    private
    public :: filter, get_edgeCorrector

    contains


    subroutine filter(weight)
        real(kp), intent(out) :: weight(filterlen)

        if (filtername == 'simple') then
            weight(1:filterlen) = simpleFilter()
        endif

    end subroutine filter


    ! Filter with no weight
    function simpleFilter() result(output)
        real(kp) :: output(filterlen)

        output(1:filterlen) = 1._kp / real(filterlen, kind=kp)

    end function simpleFilter


    subroutine get_edgeCorrector(filter, startIndex, endIndex, corrector)
        real(kp), intent(in)  :: filter(filterlen)
        integer , intent(in)  :: startIndex
        integer , intent(in)  :: endIndex
        real(kp), intent(out) :: corrector

        corrector = 1._kp

    end subroutine get_edgeCorrector



end module movave_filter

