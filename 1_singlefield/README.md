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
The routine `xios_recv_field` will return whatever is stored in the buffer that was sent using `xios_send_field`, independently of whether the current timestep is a coupling one. **For this reason, `xios_recv_field` should be called only on the "right" timesteps.**
```fortran
! Receive field starting from 1 with a certain frequency
IF (modulo(curr_timestep-1, freq_op) == 0) THEN
    CALL xios_recv_field("field2D_recv", field_recv)
    print *, "Model ", model_id, " received " , field_recv(1,1), " @ts = ", curr_timestep
END IF
```

# Output
```
 Model ocn sended @ts =           1
 Model ocn sended @ts =           2
 Model atm received    1.00000000000000       @ts =            1
 Model ocn sended @ts =           3
 Model atm received    2.00000000000000       @ts =            2
 Model atm received    3.00000000000000       @ts =            3
 Model ocn sended @ts =           4
 Model atm received    4.00000000000000       @ts =            4
 Model ocn sended @ts =           5
 Model atm received    5.00000000000000       @ts =            5
 Model ocn sended @ts =           6
 Model atm received    6.00000000000000       @ts =            6
 Model ocn sended @ts =           7
 Model atm received    7.00000000000000       @ts =            7
 Model ocn sended @ts =           8
 Model atm received    8.00000000000000       @ts =            8
 Model ocn sended @ts =           9
 Model atm received    9.00000000000000       @ts =            9
 Model ocn sended @ts =          10
 Model atm received    10.0000000000000       @ts =           10
 Model ocn sended @ts =          11
 Model atm received    11.0000000000000       @ts =           11
 Model ocn sended @ts =          12
 Model atm received    12.0000000000000       @ts =           12
 Model ocn sended @ts =          13
 Model atm received    13.0000000000000       @ts =           13
 Model ocn sended @ts =          14
 Model atm received    14.0000000000000       @ts =           14
 Model ocn sended @ts =          15
 Model atm received    15.0000000000000       @ts =           15
 Model ocn sended @ts =          16
 Model atm received    16.0000000000000       @ts =           16
 Model ocn sended @ts =          17
 Model atm received    17.0000000000000       @ts =           17
 Model ocn sended @ts =          18
 Model atm received    18.0000000000000       @ts =           18
 Model ocn sended @ts =          19
 Model atm received    19.0000000000000       @ts =           19
 Model ocn sended @ts =          20
 Model atm received    20.0000000000000       @ts =           20
 Model ocn sended @ts =          21
 Model atm received    21.0000000000000       @ts =           21
 Model ocn sended @ts =          22
 Model atm received    22.0000000000000       @ts =           22
 Model ocn sended @ts =          23
 Model atm received    23.0000000000000       @ts =           23
 Model ocn sended @ts =          24
 Model atm received    24.0000000000000       @ts =           24
 Model ocn sended @ts =          25
 Model atm received    25.0000000000000       @ts =           25
 Model ocn sended @ts =          26
 Model atm received    26.0000000000000       @ts =           26
 Model ocn sended @ts =          27
 Model atm received    27.0000000000000       @ts =           27
 Model ocn sended @ts =          28
 Model atm received    28.0000000000000       @ts =           28
 Model ocn sended @ts =          29
 Model atm received    29.0000000000000       @ts =           29
 Model ocn sended @ts =          30
 Model atm received    30.0000000000000       @ts =           30
 Model ocn sended @ts =          31
 Model atm received    31.0000000000000       @ts =           31
```


