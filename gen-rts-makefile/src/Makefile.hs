module Makefile (makeFile, targetCS, ccRecipe) where

comment :: String -> String
comment = (<>) "# "

comment' :: String -> String
comment' s =
  init $
    unlines
      [ "###",
        "### " <> s,
        "###"
      ]

compilers :: Maybe String -> String
compilers path =
  case path of
    Nothing -> define "arm-none-eabi"
    (Just s) -> define s
  where
    define p =
      unlines
        [ "C=      " <> p,
          "CC=     $C-gcc",
          "AS=     $C-as",
          "LINKER= $C-g++",
          "POST=   $C-objcopy"
        ]

flags :: String
flags =
  unlines
    [ "CCFLAGS=     -g \\",
      "             -O0 \\",
      "             -Wall \\",
      "             -mthumb \\",
      "             -mcpu=cortex-m4 \\",
      "             -mfloat-abi=hard \\",
      "             -mfpu=fpv4-sp-d16 \\",
      "             -fverbose-asm \\",
      "             -DSTM32F40_41xxx \\",
      "             -I ./device/inc \\",
      "             -I ./driver/inc",
      "",
      "ASFLAGS=     -IIFLAGS",
      "",
      "LINKERFLAGS= --specs=nano.specs \\",
      "             -mthumb \\",
      "             -mfloat-abi=hard \\",
      "             -mfpu=fpv4-sp-d16 \\",
      "             -mcpu=cortex-m4 \\",
      "             -nostartfiles \\",
      "             -T ./md407-ram.x \\",
      "             -Wl,-Map=./Debug/RTS-Lab.map,--cref",
      "",
      "POSTFLAGS=   -S -O srec"
    ]

directories :: String
directories =
  unlines
    [ "DEBUGDIR=  ./Debug/",
      "DRIVERDIR= ./driver/src/",
      "MKDIR=     test -d $(DEBUGDIR) || mkdir -p $(DEBUGDIR)"
    ]

objects :: String -> String
objects s =
  unlines
    [ "OBJECTS= $(DEBUGDIR)dispatch.o \\",
      "         $(DEBUGDIR)TinyTimber.o \\",
      "         $(DEBUGDIR)canTinyTimber.o \\",
      "         $(DEBUGDIR)sciTinyTimber.o \\",
      "         $(DEBUGDIR)stm32f4xx_can.o \\",
      "         $(DEBUGDIR)stm32f4xx_dac.o \\",
      "         $(DEBUGDIR)stm32f4xx_exti.o \\",
      "         $(DEBUGDIR)stm32f4xx_gpio.o \\",
      "         $(DEBUGDIR)stm32f4xx_rcc.o \\",
      "         $(DEBUGDIR)stm32f4xx_syscfg.o \\",
      "         $(DEBUGDIR)stm32f4xx_tim.o \\",
      "         $(DEBUGDIR)stm32f4xx_usart.o \\",
      "         $(DEBUGDIR)startup.o \\",
      s
    ]

target :: String -> String
target = ("$(DEBUGDIR)" <>) . (<> ".o: ")

targetC :: String -> String
targetC s = target s <> s <> ".c"

targetCH :: String -> String
targetCH s = targetC s <> " " <> s <> ".h"

targetCS :: String -> String -> String
targetCS c s = targetC c <> " " <> s

ccRecipe :: String
ccRecipe = "\t$(CC) -c $< -o $@ $(CCFLAGS)"

driverTarget :: String -> String
driverTarget s =
  target ("stm32f4xx_" <> s) <> "$(DRIVERDIR)" <> "stm32f4xx_" <> s <> ".c"

driverRule :: String -> String
driverRule s = unlines [driverTarget s, ccRecipe]

intermediateRules :: String
intermediateRules =
  debugDirRule
    <> elfRule
    <> s19Rule
    <> dispatchRule
    <> driverRule "can"
    <> driverRule "dac"
    <> driverRule "exti"
    <> driverRule "gpio"
    <> driverRule "rcc"
    <> driverRule "syscfg"
    <> driverRule "tim"
    <> driverRule "usart"
    <> unlines [targetC "startup", ccRecipe]
    <> unlines [targetCH "TinyTimber", ccRecipe]
    <> unlines [targetCH "canTinyTimber", ccRecipe]
    <> unlines [targetCH "sciTinyTimber", ccRecipe]
  where
    debugDirRule =
      unlines
        [ "$(DEBUGDIR):",
          "\t$(MKDIR)"
        ]
    elfRule =
      unlines
        [ "$(DEBUGDIR)RTS-Lab.elf: $(OBJECTS)",
          "\t$(LINKER) -o $@ $(LINKERFLAGS) $^"
        ]
    s19Rule =
      unlines
        [ "$(DEBUGDIR)RTS-Lab.s19: $(DEBUGDIR)RTS-Lab.elf",
          "\t$(POST) $(POSTFLAGS) $< $@"
        ]
    dispatchRule =
      unlines
        [ "$(DEBUGDIR)dispatch.o: dispatch.s",
          "\t$(AS) $< -o $@ $(ASFLAGS)"
        ]

cleanRule :: String
cleanRule =
  unlines
    [ "clean:",
      "\trm -vrf $(DEBUGDIR)"
    ]

makeFile :: Maybe String -> String -> String -> String
makeFile p r o =
  unlines
    [ comment' "Common definitions",
      "",
      comment "Compilers",
      compilers p,
      comment "Flags",
      flags,
      comment "Directories",
      directories,
      comment "Objects",
      objects o,
      "",
      comment' "Main target",
      "",
      ".PHONY: all",
      "all: $(DEBUGDIR) $(DEBUGDIR)RTS-Lab.elf $(DEBUGDIR)RTS-Lab.s19",
      "",
      comment' "Intermediate targets",
      "",
      comment "System-defined targets",
      intermediateRules,
      comment "User-defined targets",
      r,
      comment' "Clean",
      "",
      ".PHONY: clean",
      cleanRule
    ]
