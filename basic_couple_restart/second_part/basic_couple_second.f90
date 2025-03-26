PROGRAM basic_couple
    USE mod_wait
    USE xios
    USE, INTRINSIC :: ISO_C_BINDING

    IMPLICIT NONE
    INCLUDE "mpif.h"

    INTEGER :: ierr
    INTEGER :: rank, size

    ! Map the process rank to the role
    LOGICAL :: is_server = .FALSE.
    LOGICAL :: is_client_atm = .FALSE.
    LOGICAL :: is_client_ocn = .FALSE.


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
            CALL runModelFromTimestep("ocn")
        ELSE IF (is_client_atm) THEN
            write (*,*) "I am client atm with rank ", rank
            CALL runModelFromTimestep("atm")
        END IF
        print *, "Model ", rank, " is done"
        CALL xios_finalize()
    ENDIF


    CALL MPI_FINALIZE(ierr)

CONTAINS
    SUBROUTINE runModelFromTimestep(model_id)
    IMPLICIT NONE
        CHARACTER(LEN=*) :: model_id
        INTEGER :: model_starting_timestep
        TYPE(xios_context) :: ctx
        INTEGER :: local_comm
        INTEGER :: curr_timestep
        TYPE(xios_date) :: x_start_date, x_end_date, x_curr_date
        TYPE(xios_duration) :: x_timestep, x_duration
        INTEGER :: ierr
        CHARACTER(LEN = 255) :: tmp = ""

        DOUBLE PRECISION, POINTER :: field_send(:,:)
        DOUBLE PRECISION, POINTER :: field_recv(:,:)

        TYPE(xios_duration) :: x_freq_op
        CHARACTER(LEN=255) :: s_freq_op
        INTEGER :: freq_op

        ! Initializing the runModelFromTimestep but local_com is not used in this example (?)
        CALL xios_initialize(model_id, return_comm = local_comm)

        write(*,*) "Model ", model_id, " is init  with rank ", rank

        CALL xios_context_initialize(model_id, local_comm)

        CALL xios_get_handle(model_id, ctx)
        CALL xios_set_current_context(ctx)

        ierr = xios_getvar("model_starting_timestep", model_starting_timestep)

        ierr = xios_getvar("duration", tmp)
        x_duration = xios_duration_convert_from_string(TRIM(tmp))
        ierr = xios_getvar("timestep_duration", tmp)
        x_timestep = xios_duration_convert_from_string(TRIM(tmp))

         
        CALL xios_get_start_date(x_start_date)

        CALL xios_set_timestep(x_timestep)


        CALL xios_close_context_definition()

        ALLOCATE(field_send(10,10))
        ALLOCATE(field_recv(10,10))

        x_curr_date = x_start_date + x_timestep * (model_starting_timestep)
        x_end_date = x_curr_date + x_duration

        curr_timestep = 1

        ! Getting the frequency of the operation
        CALL xios_get_field_attr("field2D_oce_to_atm", freq_op=x_freq_op)
        CALL xios_duration_convert_to_string(x_freq_op, s_freq_op)
        s_freq_op = s_freq_op(1:LEN_TRIM(s_freq_op)-2)
        READ(s_freq_op, *) freq_op
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        print *, "Model ",model_id, " is starting"
        
        CALL xios_update_calendar(1)

        
        ! Iterate for the duration of the simulation
        DO WHILE (x_curr_date <= x_end_date)

            ! Communicate timestep increment to XIOS
            CALL xios_update_calendar(curr_timestep)

            IF(rank == 0) THEN
                field_send = model_starting_timestep+curr_timestep-1 ! Assigning the field values to the current timestep for testing
            END IF

            ! Ocn sends to atm
            IF(rank == 0) THEN

                ! Avoid sending at first timestep, since the send have to be done after a period
                IF (curr_timestep > 1 .and. modulo(curr_timestep, freq_op) == 1) THEN

                    CALL xios_send_field("field2D", field_send)
                    print *, "Model ", model_id, " sended @ts =", curr_timestep, field_send(1,1)

                END IF

            ! Atm receives from ocn
            ELSE IF(rank == 1) THEN

                 IF (curr_timestep == 1) THEN

                    CALL xios_recv_field("field2D_restart", field_recv)
                    print *, "Model ", model_id, " received " , field_recv(1,1), " at timestep ", curr_timestep
                
                ! xios_recv to be called on the right recieving timestep, unlike in OASIS
                ELSE IF (modulo(curr_timestep, freq_op) == 1) THEN

                    CALL xios_recv_field("field2D_oce_to_atm", field_recv)
                    print *, "Model ", model_id, " received " , field_recv(1,1), " at timestep ", curr_timestep

                END IF
            END IF

            x_curr_date = x_curr_date + x_timestep ! Date
            curr_timestep = curr_timestep + 1 ! Timestep

        END DO

        DEALLOCATE(field_send)
        DEALLOCATE(field_recv)

        CALL xios_context_finalize()

    END SUBROUTINE runModelFromTimestep


END PROGRAM basic_couple