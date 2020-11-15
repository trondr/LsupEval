# LsupEval
Evaluate Lenovo System Update packages with respect to Dependencies and DetectInstall elements in the update package xml

## Status 
Implemented detection rules

5 of 5 logical detection rules are supported

| Logical Rule               | Status      |
|----------------------------|-------------|
| True                       | &#x2714;    |
| False                      | &#x2714;    |
| And                        | &#x2714;    |
| Or                         | &#x2714;    |
| Not                        | &#x2714;    |

8 of 23 detection rules are supported

| Base Applicability Rule | Status      |  Comment    |
|-------------------------|-------------|-------------|
| _AddRemovePrograms      | &#x274C;    | Not in use by any known Lenovo update packages (2020-10-24)|
| _Bios			              | &#x2714;    ||
| _BiosDate               | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _CpuAddressWidth        | &#x2714;    ||
| _Driver                 | &#x2714;    ||
| _EmbeddedControllerVersion | &#x2714; ||
| _ExternalDetection      | &#x2714;    ||
| _FileVersion            | &#x2714;    ||
| _FileDate               | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _FileExists             | &#x2714;    ||
| _FreeSpace              | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _HDD                    | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _MemDetect              | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _OS                     | &#x2714;    ||
| _OSLang                 | &#x2714;    ||
| _OSNLang                | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _PnPID                  | &#x2714;    ||
| _RegistryKey            | &#x2714;    ||
| _RegistryKeyValue       | &#x2714;    ||
| _SupportedSystems       | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _SystemVendor           | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _Ioctl                  | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _OpticalDriveDetection  | &#x274C;    |Not in use by any known Lenovo update packages (2020-10-24)|
| _Coreq                  | &#x2714;    ||

# Usage example
See unit tests:
* "load Lsu Package evaluate depencies"
* "load Lsu Package evaluate detect install"
