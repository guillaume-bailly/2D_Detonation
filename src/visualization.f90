module visualization_module
    implicit none
    contains

    subroutine write_results(rho, u, v, e, nx, ny, filename)
        ! Write results to a CSV file
        integer, intent(in) :: nx, ny
        real, intent(in) :: rho(nx, ny), u(nx, ny), v(nx, ny), e(nx, ny)
        character(len=*), intent(in) :: filename
        integer :: i, j
        open(unit=10, file=filename, status="replace")
        do j = 1, ny
            do i = 1, nx
                write(10, '(4F12.5)') rho(i, j), u(i, j), v(i, j), e(i, j)
            end do
            write(10, *)  ! Add newline for each row
        end do
        close(10)
    end subroutine write_results
end module visualization_module
