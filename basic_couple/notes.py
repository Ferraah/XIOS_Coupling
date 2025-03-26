

def runOceanBegin():

    xios_ts = 1
    model_ts = 1

    duration = 20
    send_freq = 4

    while model_ts <= 10:

        field = computesomething()
        xios_set_calendar(xios_ts)
        if(ts % send_freq == 1):
            xios_send_field(field)

        model_ts += 1
        xios_ts

def runOceanFromTimestep(start_model_ts: int):

    model_ts = start_model_ts

    xios_ts = 1
    xios_period_started = False

    duration = 20
    send_freq = 4
    while (model_ts <= duration):

        field = computesomething()

        if xios_period_started:
            xios_ts += 1

        if (model_ts % send_freq == 1):
            xios_set_calendar(xios_ts) # !! SET THE CALENDAR ON 1 AT FIRST SEND

            if(xios_period_started == False):
                xios_period_started = True

            xios_send_field(field)

def runAtmosphereFromTimestep(start_model_ts: int):

    model_ts = start_model_ts

    xios_ts = 1
    xios_period_started = False

    duration = 20
    send_freq = 4

    while (model_ts <= duration):
        
        
        if xios_period_started:
            xios_ts += 1

        if (model_ts % send_freq == 1):

            #  False means that we haven't done yet the first recv
            if xios_period_started == False:
                xios_set_calendar(1)
                xios_period_started = True
                xios_recv_field("field_restart")
            else:
                xios_recv_field("field_oce_to_atm")


        
        xios_ts += 1
        model_ts += 1
