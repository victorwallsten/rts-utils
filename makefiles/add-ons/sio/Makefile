###
### Common definitions
###

# Compilers
C=      arm-none-eabi
CC=     $C-gcc
AS=     $C-as
LINKER= $C-g++
POST=   $C-objcopy

# Flags
CCFLAGS=     -g -O0 -Wall -mthumb -mcpu=cortex-m4 -mfloat-abi=hard -mfpu=fpv4-sp-d16 -fverbose-asm -DSTM32F40_41xxx -I ./device/inc -I ./driver/inc
ASFLAGS=     -IIFLAGS
LINKERFLAGS= --specs=nano.specs -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -mcpu=cortex-m4 -nostartfiles -T ./md407-ram.x -Wl,-Map=./Debug/RTS-Lab.map,--cref
POSTFLAGS=   -S -O srec

# Directories
DEBUGDIR=  ./Debug/
DRIVERDIR= ./driver/src/
MKDIR=     test -d $(DEBUGDIR) || mkdir -p $(DEBUGDIR)

# Objects
OBJECTS= $(DEBUGDIR)dispatch.o $(DEBUGDIR)TinyTimber.o $(DEBUGDIR)canTinyTimber.o $(DEBUGDIR)sciTinyTimber.o $(DEBUGDIR)stm32f4xx_can.o $(DEBUGDIR)stm32f4xx_dac.o $(DEBUGDIR)stm32f4xx_exti.o $(DEBUGDIR)stm32f4xx_gpio.o $(DEBUGDIR)stm32f4xx_rcc.o $(DEBUGDIR)stm32f4xx_syscfg.o $(DEBUGDIR)stm32f4xx_tim.o $(DEBUGDIR)stm32f4xx_usart.o $(DEBUGDIR)startup.o $(DEBUGDIR)sioTinyTimber.o $(DEBUGDIR)application.o

###
### Main target
###

.PHONY: all
all: $(DEBUGDIR) $(DEBUGDIR)RTS-Lab.elf $(DEBUGDIR)RTS-Lab.s19

###
### Intermediate targets
###

# System-defined targets
$(DEBUGDIR):
	$(MKDIR)
$(DEBUGDIR)RTS-Lab.elf: $(OBJECTS)
	$(LINKER) -o $@ $(LINKERFLAGS) $^
$(DEBUGDIR)RTS-Lab.s19: $(DEBUGDIR)RTS-Lab.elf
	$(POST) $(POSTFLAGS) $< $@
$(DEBUGDIR)dispatch.o: dispatch.s
	$(AS) $< -o $@ $(ASFLAGS)
$(DEBUGDIR)stm32f4xx_can.o: $(DRIVERDIR)stm32f4xx_can.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)stm32f4xx_dac.o: $(DRIVERDIR)stm32f4xx_dac.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)stm32f4xx_exti.o: $(DRIVERDIR)stm32f4xx_exti.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)stm32f4xx_gpio.o: $(DRIVERDIR)stm32f4xx_gpio.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)stm32f4xx_rcc.o: $(DRIVERDIR)stm32f4xx_rcc.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)stm32f4xx_syscfg.o: $(DRIVERDIR)stm32f4xx_syscfg.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)stm32f4xx_tim.o: $(DRIVERDIR)stm32f4xx_tim.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)stm32f4xx_usart.o: $(DRIVERDIR)stm32f4xx_usart.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)startup.o: startup.c
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)TinyTimber.o: TinyTimber.c TinyTimber.h
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)canTinyTimber.o: canTinyTimber.c canTinyTimber.h
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)sciTinyTimber.o: sciTinyTimber.c sciTinyTimber.h
	$(CC) -c $< -o $@ $(CCFLAGS)

# User-defined targets
$(DEBUGDIR)sioTinyTimber.o: sioTinyTimber.c TinyTimber.h sioTinyTimber.h
	$(CC) -c $< -o $@ $(CCFLAGS)
$(DEBUGDIR)application.o: application.c TinyTimber.h sciTinyTimber.h canTinyTimber.h
	$(CC) -c $< -o $@ $(CCFLAGS)

###
### Clean
###

.PHONY: clean
clean:
	rm -vrf $(DEBUGDIR)

