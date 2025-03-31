# Monodirectional coupling of a single field with restart file

This example shows the the coupling functionality between two contexts of their relative toy models (`ocn` and `atm`). Ocean is the model in charge of sending the field, to the Atmosphere model. The field is sent at the end of the timesteps which are valid coupling timesteps relative to the coupling frequency. The first receive is relative to the field from the restaring file. Additionally, coupling time parameters are tweaked, but the number of coupling exchanges is still the same as before (one per day).

|  | Ocean | Atmosphere|
|----------|----------|----------|
|Start date|Jan 01, 2025|Jan 01, 2025 
| Duration  |  31d       | 31d         |
|Timestep| 6h | 6h
| Send/recv frequency          | 4ts          | 4ts         |
This translates to:
| freq_op | 4ts| 4ts
| freq_offset | 0ts | 5ts|
| (Restart field) freq_op | 1y *| 1y*
| (Restart field) freq_offset | 0ts | 1ts|

\* arbitrarily large, so to load one time during the run

## Algorithm explaination

We will start indexing the timesteps from 1 as discussed in the first example. To reproduce the Oasis lag logic, we offset the receiving of the first ocean send by 1+freq_op. Then, `@ts=1` atm will receive the restart file, and after a coupling period (`@ts=5`) the coupling field.


### xios_send_field & xios_recv_field
`xios_send_field` logic is unchanged from example 1.\
As opposed to what happens with oasis GET, a restarting field should be set explicitly when calling the first `xios_recv_field`:

```fortran
!!! First reception is done explicitly on the restart field from the related file
IF (curr_timestep == 1) THEN
    CALL xios_recv_field("field2D_restart", field_recv)
    print *, "Model ", model_id, " received " , field_recv(1,1), " @ts = ", curr_timestep

ELSE IF (modulo(curr_timestep-1, freq_op) == 0) THEN
    CALL xios_recv_field("field2D_oce_to_atm", field_recv)
    print *, "Model ", model_id, " received " , field_recv(1,1), " @ts = ", curr_timestep
END IF
```
The way XIOS is implemented, `ocn` will be in charge to send the restarting field to `atm`. For this reason, the restarting file reference is defined in `ocn` context.
```xml
<file_definition>
    ...
<!-- Restart file to READ (No output is done on this file) -->
<!-- output_freq refers to the reading frequency. mode is set up on "read" -->
<file id="output_restart" name="output_out" enabled="true" type="one_file" output_freq="1y" mode="read">
    <field id="field2D_read" name="field2D_oce_to_atm" grid_ref="grid_2D" operation="instant" read_access="true"  />
</file> 

</file_definition>
```
Then, in addition to the coupling field, we refer to this field from file to send it to atm context.
```xml
<!-- OCEAN CONTEXT -->
<coupler_out_definition>
    <coupler_out context="atm::atm" >
        <field id="field2D_oce_to_atm" grid_ref="grid_2D" freq_op="4ts"/>
        <!-- Restart field-->
        <field id="field2D_restart" field_ref="field2D_read" freq_op="1y"/>
    </coupler_out>
</coupler_out_definition>
```
```xml
<!-- ATM CONTEXT -->
<coupler_in_definition>
    <coupler_in context="ocn::ocn" >
        <field id="field2D_oce_to_atm" grid_ref="grid_2D" freq_op="4ts" freq_offset="1ts" operation="instant" read_access="true"/>
        <!-- Restart field for atm is provided by ocean - freq_op big so to execute it only one time, offset to run it @ts=1 instead of @ts=0-->
        <field id="field2D_restart" grid_ref="grid_2D_restart" freq_op="1y" freq_offset="1ts" operation="instant" read_access="true"/>
    </coupler_in>
</coupler_in_definition>
```

# Output
```
 Model atm received    31.0000000000000       @ts =            1
 Model ocn sended @ts =           1
 Model ocn sended @ts =           2
 Model ocn sended @ts =           3
 Model ocn sended @ts =           4
 Model atm received    4.00000000000000       @ts =            5
 Model ocn sended @ts =           5
 Model ocn sended @ts =           6
 Model ocn sended @ts =           7
 Model ocn sended @ts =           8
 Model atm received    8.00000000000000       @ts =            9
 Model ocn sended @ts =           9
 Model ocn sended @ts =          10
 Model ocn sended @ts =          11
 Model ocn sended @ts =          12
 Model atm received    12.0000000000000       @ts =           13
 Model ocn sended @ts =          13
 Model ocn sended @ts =          14
 Model ocn sended @ts =          15
 Model ocn sended @ts =          16
 Model atm received    16.0000000000000       @ts =           17
 Model ocn sended @ts =          17
 Model ocn sended @ts =          18
 Model ocn sended @ts =          19
 Model ocn sended @ts =          20
 Model atm received    20.0000000000000       @ts =           21
 Model ocn sended @ts =          21
 Model ocn sended @ts =          22
 Model ocn sended @ts =          23
 Model ocn sended @ts =          24
 Model atm received    24.0000000000000       @ts =           25
 Model ocn sended @ts =          25
 Model ocn sended @ts =          26
 Model ocn sended @ts =          27
 Model ocn sended @ts =          28
 Model atm received    28.0000000000000       @ts =           29
 Model ocn sended @ts =          29
 Model ocn sended @ts =          30
 Model ocn sended @ts =          31
 Model ocn sended @ts =          32
 Model atm received    32.0000000000000       @ts =           33
 Model ocn sended @ts =          33
 Model ocn sended @ts =          34
 Model ocn sended @ts =          35
 Model ocn sended @ts =          36
 Model atm received    36.0000000000000       @ts =           37
 Model ocn sended @ts =          37
 Model ocn sended @ts =          38
 Model ocn sended @ts =          39
 Model ocn sended @ts =          40
 Model atm received    40.0000000000000       @ts =           41
 Model ocn sended @ts =          41
 Model ocn sended @ts =          42
 Model ocn sended @ts =          43
 Model ocn sended @ts =          44
 Model atm received    44.0000000000000       @ts =           45
 Model ocn sended @ts =          45
 Model ocn sended @ts =          46
 Model ocn sended @ts =          47
 Model ocn sended @ts =          48
 Model atm received    48.0000000000000       @ts =           49
 Model ocn sended @ts =          49
 Model ocn sended @ts =          50
 Model ocn sended @ts =          51
 Model ocn sended @ts =          52
 Model atm received    52.0000000000000       @ts =           53
 Model ocn sended @ts =          53
 Model ocn sended @ts =          54
 Model ocn sended @ts =          55
 Model ocn sended @ts =          56
 Model atm received    56.0000000000000       @ts =           57
 Model ocn sended @ts =          57
 Model ocn sended @ts =          58
 Model ocn sended @ts =          59
 Model ocn sended @ts =          60
 Model atm received    60.0000000000000       @ts =           61
 Model ocn sended @ts =          61
 Model ocn sended @ts =          62
 Model ocn sended @ts =          63
 Model ocn sended @ts =          64
 Model atm received    64.0000000000000       @ts =           65
 Model ocn sended @ts =          65
 Model ocn sended @ts =          66
 Model ocn sended @ts =          67
 Model ocn sended @ts =          68
 Model atm received    68.0000000000000       @ts =           69
 Model ocn sended @ts =          69
 Model ocn sended @ts =          70
 Model ocn sended @ts =          71
 Model ocn sended @ts =          72
 Model atm received    72.0000000000000       @ts =           73
 Model ocn sended @ts =          73
 Model ocn sended @ts =          74
 Model ocn sended @ts =          75
 Model ocn sended @ts =          76
 Model atm received    76.0000000000000       @ts =           77
 Model ocn sended @ts =          77
 Model ocn sended @ts =          78
 Model ocn sended @ts =          79
 Model ocn sended @ts =          80
 Model atm received    80.0000000000000       @ts =           81
 Model ocn sended @ts =          81
 Model ocn sended @ts =          82
 Model ocn sended @ts =          83
 Model ocn sended @ts =          84
 Model atm received    84.0000000000000       @ts =           85
 Model ocn sended @ts =          85
 Model ocn sended @ts =          86
 Model ocn sended @ts =          87
 Model ocn sended @ts =          88
 Model atm received    88.0000000000000       @ts =           89
 Model ocn sended @ts =          89
 Model ocn sended @ts =          90
 Model ocn sended @ts =          91
 Model ocn sended @ts =          92
 Model atm received    92.0000000000000       @ts =           93
 Model ocn sended @ts =          93
 Model ocn sended @ts =          94
 Model ocn sended @ts =          95
 Model ocn sended @ts =          96
 Model atm received    96.0000000000000       @ts =           97
 Model ocn sended @ts =          97
 Model ocn sended @ts =          98
 Model ocn sended @ts =          99
 Model ocn sended @ts =         100
 Model atm received    100.000000000000       @ts =          101
 Model ocn sended @ts =         101
 Model ocn sended @ts =         102
 Model ocn sended @ts =         103
 Model ocn sended @ts =         104
 Model atm received    104.000000000000       @ts =          105
 Model ocn sended @ts =         105
 Model ocn sended @ts =         106
 Model ocn sended @ts =         107
 Model ocn sended @ts =         108
 Model atm received    108.000000000000       @ts =          109
 Model ocn sended @ts =         109
 Model ocn sended @ts =         110
 Model ocn sended @ts =         111
 Model ocn sended @ts =         112
 Model atm received    112.000000000000       @ts =          113
 Model ocn sended @ts =         113
 Model ocn sended @ts =         114
 Model ocn sended @ts =         115
 Model ocn sended @ts =         116
 Model atm received    116.000000000000       @ts =          117
 Model ocn sended @ts =         117
 Model ocn sended @ts =         118
 Model ocn sended @ts =         119
 Model ocn sended @ts =         120
 Model atm received    120.000000000000       @ts =          121
 Model ocn sended @ts =         121
 Model ocn sended @ts =         122
 Model ocn sended @ts =         123
 Model            1  is done
 Model ocn sended @ts =         124
 Model            0  is done
```

