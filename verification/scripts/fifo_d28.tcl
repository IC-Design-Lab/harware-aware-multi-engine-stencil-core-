create_ip -name fifo_generator \
          -vendor xilinx.com \
          -library ip \
          -version 13.2 \
          -module_name fifo_d_beh

set_property -dict [list \
CONFIG.Fifo_Implementation {Common_Clock_Built-in_FIFO} \
CONFIG.Input_Data_Width {32} \
CONFIG.Input_Depth {32} \
CONFIG.Output_Data_Width {32} \
CONFIG.Performance_Options {First_Word_Fall_Through} \
] [get_ips fifo_d28]

generate_target all [get_ips fifo_d28]