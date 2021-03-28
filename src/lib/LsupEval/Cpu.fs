namespace LsupEval

module Cpu =
    open LsupEval.Logging
    open LsupEval.WmiHelper
    let logger = Logging.getLoggerByName "Cpu"

    type CpuAddressWidth = Bit32=32us | Bit64=64us
        
    let getCurrentCpuAddressWidth () =
        result{
            let! addressWidthObject = getWmiPropertyValue "Win32_Processor" "AddressWidth"
            let addresswith =  addressWidthObject :?> uint16
            let aw: CpuAddressWidth = LanguagePrimitives.EnumOfValue addresswith
            return aw
        }

    let isCpuAddressWidthMatch (logger:Common.Logging.ILog) (cpuAddressWidth:CpuAddressWidth) (currentCpuAddressWidth) =
        let isMatch = cpuAddressWidth = currentCpuAddressWidth        
        if(logger.IsDebugEnabled) then logger.Debug(sprintf "Comparing cpu address width '%A' with required cpu address width '%A'. Return: %b" currentCpuAddressWidth cpuAddressWidth isMatch)
        isMatch

        
