# Monodirectional coupling of a single field with no restart file

This example shows the the coupling functionality between two contexts of their relative toy models (`ocn` and `atm`). Ocean is the model in charge of sending the field, to the Atmosphere model. The field is sent at the end of the timesteps which are valid coupling timesteps relative to the coupling frequency.
|  | Ocean | Atmosphere|
|----------|----------|----------|
|Start date|Jan 01, 2025|Jan 01, 2025 
| Duration  |  31d       | 31d         |
|Timestep| 1d | 1d
| Send/recv frequency          | 1ts          | 1ts         |
This translates to:
| freq_op | 1ts| 1ts
| freq_offset | 0ts | 1ts|


## Algorithm explaination
XIOS handles the concept of time through the usage of the routine `xios_update_calendar(timestep)`, by which the user can set the current timestep during the coupling run.
Furthermore, it is designed to start the coupling functionality from @ts=1, which effectively corresponds to the first time step (or @time = 0). Hence, we expect to run the coupling from @ts=1 (Jan 1) to @ts=31 (Jan 31) included.

### freq_op & freq_offset
These are two attributes used in xios fro various tasks. In our coupling envirnonment:\
`freq_offset` specifies the delay from `@ts=0` before performing the operation for the first time.\
`freq_op` determines how often the operation (send or receive) is performed after the initial offset.\
The choice of `freq_offset` requires considering a subtle detail: as `@ts=0` is considered a special case in XIOS, a field cannot be sent but just received during this time state. Hence we want to shift the timeline by one timestep making the algorithms easier to visualize. Indeed, fields are sent by default `@ts=1` and there is no need for defining an offset. Receptions can be made from 0, hence the need to offset the first one with `freq_offset="1ts"`.

### xios_send_field & xios_recv_field
The routine `xios_recv_field` will return whatever is stored in the buffer that was sent using `xios_send_field`, independently of whether the current timestep is a coupling one. **For coupling purposes, `xios_recv_field` should be called only on the "right" timesteps.**
```fortran
! Receive field starting from 1 with a certain frequency
IF (modulo(curr_timestep-1, freq_op) == 0) THEN
    CALL xios_recv_field("field2D_oce_to_atm", field_recv)
    print *, "Model ", model_id, " received " , field_recv(1,1), " @ts = ", curr_timestep
END IF
```


