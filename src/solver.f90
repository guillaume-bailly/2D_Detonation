module solver_module
    implicit none
    real, parameter :: gamma = 1.4  ! Ratio of specific heats (ideal gas)

    contains

    subroutine initialize_flow(rho, u, p, e, nx, ny)
        integer, intent(in) :: nx, ny
        real, intent(out) :: rho(nx, ny), u(nx, ny), p(nx, ny), e(nx, ny)

        ! Initialize uniform flow with a density discontinuity
        rho(:, :) = 1.0
        rho(1:nx/2, :) = 2.0  ! Higher density region
        u(:, :) = 0.0
        p(:, :) = 1.0
        e(:, :) = p(:, :) / ((gamma - 1.0) * rho(:, :))  ! Compute internal energy
    end subroutine initialize_flow

    subroutine compute_step(rho, u, e, p, nx, ny, dx, dt)
        integer, intent(in) :: nx, ny
        real, intent(in) :: dx, dt
        real, intent(inout) :: rho(nx, ny), u(nx, ny), e(nx, ny), p(nx, ny)

        integer :: i, j
        real :: lambda_max
        real :: rhoL, rhoR, uL, uR, pL, pR, eL, eR
        real, dimension(nx, ny) :: rho_new, u_new, v_new, e_new

        ! Loop over cells in x-direction (1D version)
        do j = 2, ny-1
            do i = 2, nx-1
                ! Left and right states
                rhoL = rho(i-1, j); uL = u(i-1, j); pL = p(i-1, j)
                rhoR = rho(i, j);   uR = u(i, j); pR = p(i, j)

                lambda_max = max(abs(uL) + sqrt(gamma * pL / rhoL), abs(uR) + sqrt(gamma * pR / rhoR))

                ! Compute numerical flux
                rho_new(i, j) = rho(i, j) - dt / dx * (rhoR * uR - rhoL * uL)
                u_new(i, j)   = u(i, j) - dt / dx * (uR * rhoR * uR + pR - (uL * rhoL * uL + pL))
                e_new(i, j)   = e(i, j) - dt / dx * ((eR + pR) * uR - (eL + pL) * uL)
            end do
        end do
        ! Update solution
        rho(:, :) = rho_new(:, :)
        u(:, :) = u_new(:, :)
        e(:, :) = e_new(:, :)

        ! Update pressure
        p(:, :) = (gamma - 1.0) * rho(:, :) * e(:, :)
    end subroutine compute_step
end module solver_module
