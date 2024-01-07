cmake_minimum_required(VERSION 3.13)

set(PICO_BOARD "pico_w")
#set(PICO_BOARD "pico")

include(pico_sdk_import.cmake)

project(hopper_project C CXX ASM)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")

pico_sdk_init()

add_compile_options(-Ofast -Wall)

add_executable(hopper
    Hopper.cpp
    HopperMemory.cpp
    HopperString.cpp
    HopperArray.cpp
    HopperDictionary.cpp
    HopperList.cpp
    HopperVariant.cpp
    HopperPair.cpp
    OpCodes.cpp
    SysCalls.cpp
    LibCalls.cpp
)
pico_enable_stdio_usb(hopper 1)
pico_enable_stdio_uart(hopper 0)

pico_add_extra_outputs(hopper)

if (PICO_CYW43_SUPPORTED)
target_link_libraries(hopper
                      pico_stdlib
                      hardware_adc
                      hardware_pwm
                      hardware_clocks
                      pico_cyw43_arch_none     # we need Wifi to access the GPIO, but we don't need anything else
                      )
else()
target_link_libraries(hopper
                      pico_stdlib
                      hardware_pwm
                      hardware_clocks
                      hardware_adc
                      )
endif()

