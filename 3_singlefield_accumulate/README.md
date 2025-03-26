# Monodirectional coupling of a single field with restart file and accumulation

This examples follows the previous one by enabling the accumulation functionality to the coupler.\
The parameters remain the same as before:

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

When dealing with accumulation, we have to create an auxiliary field in the sender context, and refer to it in the coupler out:
```xml 
<!-- Accumulate and average on this field --> 
<field_definition>
    <field id="field2D_accumulate" grid_ref="grid_2D"  operation="average" read_access="true" expr="@this"/>
</field_definition>

<coupler_out_definition>
    <coupler_out context="atm::atm" >
        <field id="field2D_oce_to_atm" field_ref="field2D_accumulate" freq_op="4ts"/>
        ...
        ...
```

## Notes
Using the same grid reference for both `field2D_oce_to_atm` and `field2D_restart` seems to trigger
`In file "element.cpp", function "std::shared_ptr<xios::CLocalView> xios::CLocalElement::getView(xios::CElementView::type)",  line 141 -> View is not initialized`


