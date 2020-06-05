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

7 of 23 detection rules are supported

| Base Applicability Rule | Status      |
|-------------------------|-------------|
| _AddRemovePrograms      | &#x274C;    |
| _Bios			          | &#x2714;    |
| _BiosDate               | &#x274C;    |
| _CpuAddressWidth        | &#x2714;    |
| _Driver                 | &#x26A0;    |
| _EmbeddedControllerVersion | &#x2714; |
| _ExternalDetection      | &#x274C;    |
| _FileVersion            | &#x274C;    |
| _FileDate               | &#x274C;    |
| _FileExists             | &#x2714;    |
| _FreeSpace              | &#x274C;    |
| _HDD                    | &#x274C;    |
| _MemDetect              | &#x274C;    |
| _OS                     | &#x2714;    |
| _OSLang                 | &#x2714;    |
| _OSNLang                | &#x274C;    |
| _PnPID                  | &#x274C;    |
| _RegistryKey            | &#x274C;    |
| _RegistryKeyValue       | &#x274C;    |
| _SupportedSystems       | &#x274C;    |
| _SystemVendor           | &#x274C;    |
| _Ioctl                  | &#x274C;    |
| _OpticalDriveDetection  | &#x274C;    |
