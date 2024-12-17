program detonation_simulation
    use solver_module
    use io_module
    implicit none

    ! Simulation parameters
    integer, parameter :: nx = 100, ny = 100
    real, parameter :: dx = 0.01, dy = 0.01, dt = 1.0e-6, t_max = 0.001
    real :: t
    real, dimension(nx, ny) :: rho, u, v, e   ! Density, velocity, energy

    ! Initialization
    call initialize_flow(rho, u, v, e, nx, ny)

    ! Computation
    t = 0.0
    do while (t < t_max)
        call apply_boundary_conditions(rho, u, v, e, nx, ny)
        call compute_step(rho, u, v, e, nx, ny, dx, dy, dt)
        t = t + dt
    end do

    call write_results(rho, u, v, e, nx, ny, "test1")
end program detonation_simulation
