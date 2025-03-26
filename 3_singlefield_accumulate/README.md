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


# Output
```
 Model atm received    26.0000000000000       @ts =            1
 Model ocn sended @ts =           1
 Model ocn sended @ts =           2
 Model ocn sended @ts =           3
 Model ocn sended @ts =           4
 Model atm received    2.50000000000000       @ts =            5
 Model ocn sended @ts =           5
 Model ocn sended @ts =           6
 Model ocn sended @ts =           7
 Model ocn sended @ts =           8
 Model atm received    6.50000000000000       @ts =            9
 Model ocn sended @ts =           9
 Model ocn sended @ts =          10
 Model ocn sended @ts =          11
 Model ocn sended @ts =          12
 Model atm received    10.5000000000000       @ts =           13
 Model ocn sended @ts =          13
 Model ocn sended @ts =          14
 Model ocn sended @ts =          15
 Model ocn sended @ts =          16
 Model atm received    14.5000000000000       @ts =           17
 Model ocn sended @ts =          17
 Model ocn sended @ts =          18
 Model ocn sended @ts =          19
 Model ocn sended @ts =          20
 Model atm received    18.5000000000000       @ts =           21
 Model ocn sended @ts =          21
 Model ocn sended @ts =          22
 Model ocn sended @ts =          23
 Model ocn sended @ts =          24
 Model atm received    22.5000000000000       @ts =           25
 Model ocn sended @ts =          25
 Model ocn sended @ts =          26
 Model ocn sended @ts =          27
 Model ocn sended @ts =          28
 Model atm received    26.5000000000000       @ts =           29
 Model ocn sended @ts =          29
 Model ocn sended @ts =          30
 Model ocn sended @ts =          31
 Model atm received    30.5000000000000       @ts =           33
 Model ocn sended @ts =          32
 Model ocn sended @ts =          33
 Model ocn sended @ts =          34
 Model ocn sended @ts =          35
 Model ocn sended @ts =          36
 Model atm received    34.5000000000000       @ts =           37
 Model ocn sended @ts =          37
 Model ocn sended @ts =          38
 Model ocn sended @ts =          39
 Model ocn sended @ts =          40
 Model atm received    38.5000000000000       @ts =           41
 Model ocn sended @ts =          41
 Model ocn sended @ts =          42
 Model ocn sended @ts =          43
 Model ocn sended @ts =          44
 Model atm received    42.5000000000000       @ts =           45
 Model ocn sended @ts =          45
 Model ocn sended @ts =          46
 Model ocn sended @ts =          47
 Model ocn sended @ts =          48
 Model atm received    46.5000000000000       @ts =           49
 Model ocn sended @ts =          49
 Model ocn sended @ts =          50
 Model ocn sended @ts =          51
 Model ocn sended @ts =          52
 Model atm received    50.5000000000000       @ts =           53
 Model ocn sended @ts =          53
 Model ocn sended @ts =          54
 Model ocn sended @ts =          55
 Model ocn sended @ts =          56
 Model atm received    54.5000000000000       @ts =           57
 Model ocn sended @ts =          57
 Model ocn sended @ts =          58
 Model ocn sended @ts =          59
 Model ocn sended @ts =          60
 Model atm received    58.5000000000000       @ts =           61
 Model ocn sended @ts =          61
 Model ocn sended @ts =          62
 Model ocn sended @ts =          63
 Model ocn sended @ts =          64
 Model atm received    62.5000000000000       @ts =           65
 Model ocn sended @ts =          65
 Model ocn sended @ts =          66
 Model ocn sended @ts =          67
 Model ocn sended @ts =          68
 Model atm received    66.5000000000000       @ts =           69
 Model ocn sended @ts =          69
 Model ocn sended @ts =          70
 Model ocn sended @ts =          71
 Model ocn sended @ts =          72
 Model atm received    70.5000000000000       @ts =           73
 Model ocn sended @ts =          73
 Model ocn sended @ts =          74
 Model ocn sended @ts =          75
 Model ocn sended @ts =          76
 Model atm received    74.5000000000000       @ts =           77
 Model ocn sended @ts =          77
 Model ocn sended @ts =          78
 Model ocn sended @ts =          79
 Model ocn sended @ts =          80
 Model atm received    78.5000000000000       @ts =           81
 Model ocn sended @ts =          81
 Model ocn sended @ts =          82
 Model ocn sended @ts =          83
 Model ocn sended @ts =          84
 Model atm received    82.5000000000000       @ts =           85
 Model ocn sended @ts =          85
 Model ocn sended @ts =          86
 Model ocn sended @ts =          87
 Model ocn sended @ts =          88
 Model atm received    86.5000000000000       @ts =           89
 Model ocn sended @ts =          89
 Model ocn sended @ts =          90
 Model ocn sended @ts =          91
 Model ocn sended @ts =          92
 Model atm received    90.5000000000000       @ts =           93
 Model ocn sended @ts =          93
 Model ocn sended @ts =          94
 Model ocn sended @ts =          95
 Model ocn sended @ts =          96
 Model atm received    94.5000000000000       @ts =           97
 Model ocn sended @ts =          97
 Model ocn sended @ts =          98
 Model ocn sended @ts =          99
 Model ocn sended @ts =         100
 Model atm received    98.5000000000000       @ts =          101
 Model ocn sended @ts =         101
 Model ocn sended @ts =         102
 Model ocn sended @ts =         103
 Model ocn sended @ts =         104
 Model atm received    102.500000000000       @ts =          105
 Model ocn sended @ts =         105
 Model ocn sended @ts =         106
 Model ocn sended @ts =         107
 Model ocn sended @ts =         108
 Model atm received    106.500000000000       @ts =          109
 Model ocn sended @ts =         109
 Model ocn sended @ts =         110
 Model ocn sended @ts =         111
 Model ocn sended @ts =         112
 Model atm received    110.500000000000       @ts =          113
 Model ocn sended @ts =         113
 Model ocn sended @ts =         114
 Model ocn sended @ts =         115
 Model ocn sended @ts =         116
 Model atm received    114.500000000000       @ts =          117
 Model ocn sended @ts =         117
 Model ocn sended @ts =         118
 Model ocn sended @ts =         119
 Model ocn sended @ts =         120
 Model atm received    118.500000000000       @ts =          121
 Model ocn sended @ts =         121
 Model ocn sended @ts =         122
 Model ocn sended @ts =         123
 Model            1  is done
 Model ocn sended @ts =         124
 Model            0  is done
```

