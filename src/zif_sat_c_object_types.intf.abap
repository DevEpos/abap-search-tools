"! <p class="shorttext synchronized">Object Type Constants</p>
INTERFACE zif_sat_c_object_types
  PUBLIC.

  CONSTANTS data_definition TYPE string VALUE 'DDLS/DF'.
  CONSTANTS metadata_extension TYPE string VALUE 'DDLX/EX'.
  CONSTANTS access_control TYPE string VALUE 'DCLS/DL'.
  CONSTANTS table_field TYPE string VALUE 'TABL/DTF'.
  CONSTANTS package TYPE string VALUE 'DEVC/K'.
  CONSTANTS table_definition TYPE string VALUE 'TABL/DT'.
  CONSTANTS view_definition TYPE string VALUE 'VIEW/DV'.
  CONSTANTS view_field TYPE string VALUE 'VIEW/DVF'.
  CONSTANTS structure TYPE string VALUE 'TABL/DS'.
  CONSTANTS structured_object TYPE string VALUE 'STOB/DO'.
  CONSTANTS structured_object_field TYPE string VALUE 'STOB/DOF'.
  CONSTANTS interface TYPE string VALUE 'INTF/OI'.
  CONSTANTS class TYPE string VALUE 'CLAS/OC'.
  CONSTANTS class_method_impl TYPE string VALUE 'CLAS/OM'.
  CONSTANTS interface_method TYPE string VALUE 'INTF/IO'.
  CONSTANTS message_class TYPE string VALUE 'MSAG/N'.
  CONSTANTS message TYPE string VALUE 'MSAG/NN'.
ENDINTERFACE.
