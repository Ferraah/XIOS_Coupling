PROGRAM basic_couple
    USE mod_wait
    USE xios
    IMPLICIT NONE
    INCLUDE "mpif.h"

    INTEGER :: ierr
    INTEGER :: rank, size

    ! Map the process rank to the role
    LOGICAL :: is_server = .FALSE.
    LOGICAL :: is_client_atm = .FALSE.
    LOGICAL :: is_client_ocn = .FALSE.

    ! Xios variables
    TYPE(xios_duration)  :: dtime
    TYPE(xios_context) :: ctx_hdl

    ! Parameters to load in the context
    CHARACTER(LEN=255) :: duration = "20s"
    CHARACTER(LEN=255) :: timestep = "1s"

    INTEGER :: provided

    CALL MPI_INIT_THREAD(MPI_THREAD_MULTIPLE, provided, ierr)
    IF (provided < MPI_THREAD_MULTIPLE) THEN
        PRINT *, "The MPI library does not provide the required level of thread support."
        CALL MPI_FINALIZE(ierr)
        STOP
    END IF
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)


    ! FOR THIS TEST: 3 processes: 1 server, 2 clients
    IF (size /= 3) THEN
        PRINT *, "This program must be run with 3 processes. Currently, there are ", size, " processes."
        CALL MPI_FINALIZE(ierr)
        STOP
    END IF

    ! Assign roles to the processes
    IF (rank == 2) THEN
        is_server = .TRUE.
    ELSE IF (rank == 0) THEN
        is_client_ocn = .TRUE.
    ELSE IF (rank == 1) THEN
        is_client_atm = .TRUE.
    END IF


    if(is_server) THEN
        CALL xios_init_server()
    ELSE
        IF (is_client_ocn) THEN
            write (*,*) "I am client ocn with rank ", rank
            CALL model("ocn")
        ELSE IF (is_client_atm) THEN
            write (*,*) "I am client atm with rank ", rank
            CALL model("atm")
        END IF
        print *, "Model ", rank, " is done"
        CALL xios_finalize()
    ENDIF


    CALL MPI_FINALIZE(ierr)

CONTAINS
    SUBROUTINE model(model_id)
    IMPLICIT NONE
        CHARACTER(LEN=*) :: model_id
        TYPE(xios_context) :: ctx
        INTEGER :: local_comm
        INTEGER :: curr_timestep
        TYPE(xios_date) :: x_start_date, x_end_date, x_curr_date
        TYPE(xios_duration) :: x_timestep
        INTEGER :: ierr

        DOUBLE PRECISION, POINTER :: field_send(:,:)
        DOUBLE PRECISION, POINTER :: field_recv(:,:)

        ! Initializing the model but local_com is not used in this example (?)
        CALL xios_initialize(model_id, return_comm = local_comm)

        write(*,*) "Model ", model_id, " is init  with rank ", rank

        CALL xios_context_initialize(model_id, local_comm)

        CALL xios_get_handle(model_id, ctx)
        CALL xios_set_current_context(ctx)

        CALL xios_get_start_date(x_start_date)

        x_timestep = xios_duration_convert_from_string(timestep)
        CALL xios_set_timestep(x_timestep)

        x_end_date = x_start_date + xios_duration_convert_from_string(duration)

        CALL xios_close_context_definition()

        ALLOCATE(field_send(10,10))
        ALLOCATE(field_recv(10,10))

        x_curr_date = x_start_date
        curr_timestep = 1

        print *, "Model ",model_id, " is starting"
        
        CALL xios_update_calendar(1)
        
        ! Iterate for the duration of the simulation
        DO WHILE (x_curr_date <= x_end_date)

            ! Communicate timestep increment to XIOS
            CALL xios_update_calendar(curr_timestep)

            field_send = curr_timestep ! Assigning the field values to the current timestep for testing

            ! Send data to the other model
            IF(rank == 0) THEN
                IF (curr_timestep > 1) THEN
                    CALL xios_send_field("field2D", field_send)
                    print *, "Model ", model_id, " is sending data at timestep ", curr_timestep
                END IF

            ! Receive data from the other model
            ELSE IF(rank == 1) THEN

                ! If we are at the first timestep, receive the restart data from file
                IF (curr_timestep == 1) THEN

                    CALL xios_recv_field("field2D_restart", field_recv)
                    print *, "Received data: ", field_recv(1,1), " at timestep ", curr_timestep

                ! If we are at a multiple of 4, receive data from the other model
                ELSE IF (modulo(curr_timestep, 4) == 1) THEN

                    CALL xios_recv_field("field2D_oce_to_atm", field_recv)
                    print *, "Model ", model_id, " received " , field_recv(1,1), " at timestep ", curr_timestep

                END IF
            END IF

            !CALL wait_us(int(1.e6))

            ! Increase time counters
            x_curr_date = x_curr_date + x_timestep ! Date
            curr_timestep = curr_timestep + 1 ! Timestep

        END DO

        DEALLOCATE(field_send)
        DEALLOCATE(field_recv)

        CALL xios_context_finalize()

    END SUBROUTINE model


END PROGRAM basic_couple