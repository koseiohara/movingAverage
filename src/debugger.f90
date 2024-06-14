module debugger 

    use fileio, only : finfo

    implicit none

    private
    public :: debug_open, debug_close, debug_write, debug_linebreak

    contains


    subroutine debug_open(unit, fname)
        integer     , intent(out) :: unit
        character(*), intent(in)  :: fname
        integer :: stat

        open(NEWUNIT=unit , &
           & FILE   =fname, &
           & ACTION ='WRITE', &
           & IOSTAT =stat     )

        if (stat/=0) then
            write(*,*)
            write(*,'(a)')    'OpenFileError ------------------------------------------'
            write(*,'(a)')    '|   Failed to open file'
            write(*,'(a)')    '|'
            write(*,'(a)')    '|   File Name : ' // trim(fname)
            write(*,'(a,i0)') '|   Unit      : ', unit
            write(*,'(a)')    '|   ACTION    : WRITE'
            write(*,'(a)')    '--------------------------------------------------------'

            ERROR STOP
        endif

    end subroutine debug_open


    subroutine debug_close(unit)
        integer, intent(in) :: unit

        close(unit)

    end subroutine debug_close


    subroutine debug_write(unit, output)
        integer, intent(in) :: unit
        integer, intent(in) :: output

        write(unit,'(i7)',advance='NO') output

    end subroutine debug_write


    subroutine debug_linebreak(unit)
        integer, intent(in) :: unit

        write(unit,*)

    end subroutine debug_linebreak


end module debugger

