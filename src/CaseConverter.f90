module CaseConverter

    implicit none

    private
    public :: to_upper, to_lower

    contains


    subroutine to_upper(lower, upper)
        character(*), intent(in)  :: lower
        character(*), intent(out) :: upper

        integer, parameter :: code_min = iachar('a')
        integer, parameter :: code_max = iachar('z')
        integer, parameter :: offset = iachar('A') - iachar('a')

        integer :: lower_len
        integer :: i

        lower_len = len(trim(lower))

        if (lower_len > len(upper)) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'to_upper()'
            write(0,'(A)') 'Input string is longer than the output'
            ERROR STOP
        endif

        upper = ''
        do i = 1, lower_len
            if (iachar(lower(i:i)) >= code_min .AND. iachar(lower(i:i)) <= code_max) then
                upper(i:i) = achar(iachar(lower(i:i)) + offset)
            else
                upper(i:i) = lower(i:i)
            endif
        enddo

    end subroutine to_upper


    subroutine to_lower(upper, lower)
        character(*), intent(in)  :: upper
        character(*), intent(out) :: lower

        integer, parameter :: code_min = iachar('A')
        integer, parameter :: code_max = iachar('Z')
        integer, parameter :: offset = iachar('a') - iachar('A')

        integer :: upper_len
        integer :: i

        upper_len = len(trim(upper))

        if (upper_len > len(lower)) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'to_lower()'
            write(0,'(A)') 'Input string is longer than the output'
            ERROR STOP
        endif

        lower = ''
        do i = 1, len(trim(upper))
            if (iachar(upper(i:i)) >= code_min .AND. iachar(upper(i:i)) <= code_max) then
                lower(i:i) = achar(iachar(upper(i:i)) + offset)
            else
                lower(i:i) = upper(i:i)
            endif
        enddo

    end subroutine to_lower


end module CaseConverter

