program basic_couple
    use xios
    implicit none
    include "mpif.h"

    integer :: ierr, provided
    integer :: rank, size
    character(len=255) :: model_id

    type :: toymodel_config
        type(xios_date) :: start_date, end_date, curr_date
        type(xios_duration) :: timestep, duration
        integer :: freq_op_in_ts
        integer :: ni_glo, nj_glo
        integer :: data_dim, data_ni, data_ibegin
        character(len=255) :: field_type
    end type toymodel_config

    ! Mpi initialization
    call MPI_INIT_THREAD(MPI_THREAD_MULTIPLE, provided, ierr)
    if (provided < MPI_THREAD_MULTIPLE) then
        print *, "The MPI library does not provide the required level of thread support."
        call MPI_FINALIZE(ierr)
        stop
    end if
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

    if (size < 3) then
        print *, "This program must be run with at least 3 processes. Currently, there are ", size, " processes."
        call MPI_FINALIZE(ierr)
        stop
    end if 
    ! -------------------------------

    if(rank==0) then
        call xios_init_server()
    else if (rank==1) then
        model_id = "atm"
        call run_toymodel()
    else if (rank==2) then
        model_id = "ocn"
        call run_toymodel()
    end if

    call MPI_FINALIZE(ierr)

contains
    
    subroutine run_toymodel()
    implicit none
        integer :: local_comm
        type(xios_context) :: ctx
        type(toymodel_config) :: config
        
        ! Standard XIOS initialization
        call xios_initialize(model_id, return_comm = local_comm)
        call xios_context_initialize(model_id, local_comm)
        call xios_get_handle(model_id, ctx)
        call xios_set_current_context(ctx)

        ! --------------------------------------------

        ! Loading the configuration of the toy model
        call load_toymodel_data(config) 

        ! Set the data coming from the model in XIOS
        call configure_xios_from_model(config)

        ! Run the coupling
        call run_coupling(config)

        ! --------------------------------------------
        call xios_context_finalize()
        call xios_finalize()


    end subroutine run_toymodel

    
    subroutine load_toymodel_data(config)
        implicit none
        type(toymodel_config), intent(out) :: config
        character(len=255) :: tmp = ""
        type(xios_duration) :: tmp2

        ierr = xios_getvar("toymodel_duration", tmp)
        print *, "Duration: ", TRIM(tmp)
        config%duration = xios_duration_convert_from_string(TRIM(tmp))
        tmp = ""

        ierr = xios_getvar("toymodel_timestep_duration", tmp)
        print *, "Timestep duration: ", TRIM(tmp)
        config%timestep = xios_duration_convert_from_string(TRIM(tmp))
        tmp = ""

        ierr = xios_getvar("toymodel_ni_glo", tmp)
        read (tmp, *) config%ni_glo
        tmp = ""

        ierr = xios_getvar("toymodel_nj_glo", tmp)
        read (tmp, *) config%nj_glo
        tmp = ""

        ierr = xios_getvar("toymodel_type", tmp)
        config%field_type = TRIM(tmp)
        tmp = ""

        ! Getting the frequency of the operation
        CALL xios_get_field_attr("field2D_oce_to_atm", freq_op=tmp2)
        CALL xios_duration_convert_to_string(tmp2, tmp)
        ! Remove the last two characters from the string to retrieve the pure number "(xx)ts"
        tmp = tmp(1:LEN_TRIM(tmp)-2)
        ! Convert to integer
        READ(tmp, *) config%freq_op_in_ts
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        call xios_get_start_date(config%start_date)

        ! Toymodel process sets the parameters regarding the position of the data of its competence
        if (model_id == "ocn") then
            config%data_dim = 1
            config%data_ni = config%ni_glo / (size-2)
            config%data_ibegin = (rank-2) * config%data_ni
        end if




    end subroutine load_toymodel_data

    subroutine configure_xios_from_model(config)
    implicit none
        type(toymodel_config), intent(in) :: config

        call xios_set_timestep(config%timestep)
        call xios_set_domain_attr("domain", ni_glo=config%ni_glo, nj_glo=config%nj_glo, type=config%field_type)
        call xios_set_domain_attr("domain", data_dim=config%data_dim, data_ni=config%data_ni, data_ibegin=config%data_ibegin)

        call xios_close_context_definition()

    end subroutine configure_xios_from_model
 

    subroutine run_coupling(conf)
    implicit none 
        type(toymodel_config) :: conf 
        double precision, pointer:: field_send(:,:), field_recv(:,:)
        integer :: curr_timestep

        if(model_id=="ocn") allocate(field_send(conf%data_ni, conf%data_nj))
        allocate(field_recv(conf%ni_glo, conf%nj_glo))

        conf%end_date = conf%start_date + conf%duration
        conf%curr_date = conf%start_date
        curr_timestep = 1

        print *, "Start date: ", conf%start_date
        print *, "End date: ", conf%end_date

        do while (conf%curr_date < conf%end_date)

            call xios_update_calendar(curr_timestep)

            if (model_id=="ocn") then

                field_send = curr_timestep
                call xios_send_field("field2D_send", field_send)
                print *, "OCN: sending field @ts=", curr_timestep, " with value ", field_send(1,1)
            else if (model_id=="atm") then
                if (mod(curr_timestep-1, conf%freq_op_in_ts) == 0) then
                    call xios_recv_field("field2D_recv", field_recv)
                    print *, "  ATM: receiving field @ts=", curr_timestep, " with value ", field_recv(1,1)
                end if
            end if

            conf%curr_date = conf%curr_date + conf%timestep
            curr_timestep = curr_timestep + 1
        end do

        if(model_id=="ocn") deallocate(field_send)
        deallocate(field_recv)
        
    end subroutine  run_coupling 
end program