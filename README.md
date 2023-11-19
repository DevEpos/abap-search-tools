[![abap package version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/DevEpos/abap-search-tools/src/zif_sat_version.intf.abap/version&label=version)](https://github.com/DevEpos/abap-search-tools/blob/main/src/zif_sat_version.intf.abap)

# abap-search-tools

This Repository contains the ADT Backend that is needed to use the eclipse plugin
[ABAP Search and Analysis Tools](https://www.github.com/DevEpos/abap-search-tools-ui)

## Installation

Install this repository using [abapGit](https://github.com/abapGit/abapGit#abapgit).

### Choosing the correct branch for your System

NW version|ABAP Platform|Branch name
----------|-------------|-----------
&#8805; 7.55|&#8805; 2020  |**main**
7.54|1909|nw-753
7.53|1809|nw-753
7.52||nw-752
7.51||nw-752 (_not tested_)
7.50||nw-750
7.40||nw-740
< 7.40||*Not officially supported*

## Additional Configuration

### Configure Job for updating the Metadata Index of CDS Views v2 (aka _View Entities_)

> **Note**: Only relevant for NW 7.55 and higher

With NW 7.55 a new version of CDS Views were introduced which do not have an accompanying DDIC View.  
Therefore some Metadata information that is necessary for the following features:

- ABAP Object Search; CDS View:
  - Parameter `from`
  - Parameter `field`
- CDS Analyzer
  - Where-Used Analysis
  - Field Analysis

is missing for these new CDS Views.

To enable the aforementioned features for the new CDS Views as well, a daily job should be configured for the ABAP program `ZSAT_CDS_V2_META_INDEX_UPDATE`.
This program will create the missing Metadata information in the tables:

- `ZSATCDS2MHEAD`
- `ZSATCDS2MTAB`
- `ZSATCDS2MFIELD`

The program will also remove any invalid indexes of deleted CDS Views.  
In addition to this program the ADT plugin [ABAP Search and Analysis Tools](https://www.github.com/DevEpos/abap-search-tools-ui) will also trigger an update of the index during the activation of one or more CDS Views.

## Issues during installation

See [Wiki](https://github.com/DevEpos/abap-search-tools/wiki/Installation-Issues)

## Necessary Authorizations

To access the backend from ADT a user must have the following authorizations
Authorization Object | Authorization Field | Value
---------------------| ------------------- | -----
S_ADT_RES            | URI                 | /devepos/adt/saat/*
