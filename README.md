# abap-search-tools

This Repository contains the ADT Backend that is needed to use the eclipse plugin
[ABAP Search and Analysis Tools](https://www.github.com/stockbal/abap-search-tools-ui)

## Installation

Install this repository using [abapGit](https://github.com/larshp/abapGit#abapgit).

### Choosing the correct branch for your System

NW version|Branch name
----------|-----------
&#8805; 7.53  |main
7.52|nw-752
7.51|nw-752 (_not tested_)
7.50|nw-750
7.40|nw-740
< 7.40|*Not officially supported*

## Necessary Authorizations

To access the backend from ADT a user must have the following authorizations
Authorization Object | Authorization Field | Value
---------------------| ------------------- | -----
S_ADT_RES            | URI                 | /devepos/adt/saat/*
