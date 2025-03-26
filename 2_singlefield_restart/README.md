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

## Notes
Using different grids reference for both `field2D_oce_to_atm` and `field2D_restart` seems to trigger a `SEGFAULT`



