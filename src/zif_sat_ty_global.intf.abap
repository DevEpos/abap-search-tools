"! <p class="shorttext synchronized" lang="en">Global types</p>
INTERFACE zif_sat_ty_global
  PUBLIC .

  TYPES:
    ty_t_tabname_range       TYPE RANGE OF tabname,
    ty_t_string_range        TYPE RANGE OF string,
    "! <p class="shorttext synchronized" lang="en">Range for Annotation name</p>
    ty_t_cds_anno_name_range TYPE RANGE OF ddannotation_key,
    ty_sql_fieldname         TYPE c LENGTH 62,
    ty_selopt_tab_position   TYPE n LENGTH 3.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Information about TADIR entry</p>
    BEGIN OF ty_s_cds_tadir,
      created_by   TYPE as4user,
      created_date TYPE as4date,
      devclass     TYPE devclass,
    END OF ty_s_cds_tadir.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Key/Value pair of CDS parameter</p>
    BEGIN OF ty_s_cds_param_value,
      name  TYPE ddparname,
      value TYPE zsat_value,
    END OF ty_s_cds_param_value.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">CDS View Annoation data</p>
    BEGIN OF ty_s_cds_annotation,
      fieldname TYPE fieldname,
      name      TYPE string,
      value     TYPE string,
    END OF ty_s_cds_annotation.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">List of annotations</p>
    ty_t_cds_annotation  TYPE STANDARD TABLE OF ty_s_cds_annotation WITH KEY name,
    ty_t_cds_param_value TYPE STANDARD TABLE OF ty_s_cds_param_value WITH KEY name,
    ty_t_cds_parameter   TYPE STANDARD TABLE OF zsat_cds_parameter WITH KEY parametername,
    ty_t_cds_association TYPE STANDARD TABLE OF zsat_cds_association WITH KEY name,
    ty_t_cds_header      TYPE STANDARD TABLE OF zsat_cds_view_header WITH KEY entityname,
    ty_t_cds_view_name   TYPE STANDARD TABLE OF zsat_cds_view_name WITH KEY table_line.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Generic range structure</p>
    BEGIN OF ty_s_selopt,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE zsat_value,
      high   TYPE zsat_value,
    END OF ty_s_selopt,

    "! <p class="shorttext synchronized" lang="en">Generic range table</p>
    ty_t_selopt TYPE STANDARD TABLE OF ty_s_selopt WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Extended SELOPT for SQL Where condition</p>
    BEGIN OF ty_s_seltab_sql,
      sqlfieldname TYPE ty_sql_fieldname,
      field        TYPE zsat_fieldname_with_alias,
      sign         TYPE ddsign,
      option       TYPE ddoption,
      low          TYPE zsat_value,
      high         TYPE zsat_value,
      subquery     TYPE string,
      sql_function TYPE zsat_sql_function,
    END OF ty_s_seltab_sql,

    ty_t_seltab_sql TYPE STANDARD TABLE OF ty_s_seltab_sql WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_s_or_seltab_sql,
      pos    TYPE ty_selopt_tab_position,
      values TYPE ty_t_seltab_sql,
    END  OF ty_s_or_seltab_sql,

    ty_t_or_seltab_sql  TYPE STANDARD TABLE OF ty_s_or_seltab_sql WITH EMPTY KEY,
    ty_t_and_seltab_sql TYPE STANDARD TABLE OF ty_t_or_seltab_sql WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_s_join_field_condition,
      field           TYPE fieldname,
      ref_field       TYPE fieldname,
      ref_table       TYPE tabname,
      ref_table_alias TYPE zsat_entity_alias,
      operator        TYPE voperator,
      off_offset      TYPE doffset,
      off_length      TYPE ddleng,
    END OF ty_s_join_field_condition,

    ty_t_join_field_condition TYPE STANDARD TABLE OF ty_s_join_field_condition WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_s_join_filter_condition,
      tabname       TYPE tabname,
      tabname_alias TYPE zsat_entity_alias,
      fieldname     TYPE fieldname,
      value_type    TYPE zsat_join_cond_value_type,
      operator      TYPE voperator,
      value         TYPE zsat_value,
      value2        TYPE zsat_value,
      and_or        TYPE vsconj,
    END OF ty_s_join_filter_condition,

    ty_t_join_filter_condition TYPE STANDARD TABLE OF ty_s_join_filter_condition WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_s_join_table,
      add_table           TYPE tabname,
      add_table_raw       TYPE zsat_entity_id_raw,
      table_name          TYPE ddtext,
      add_table_alias     TYPE zsat_entity_alias,
      add_table_alias_alv TYPE zsat_entity_alias_alv,
      entity_type         TYPE zsat_entity_type,
      join_type           TYPE zsat_jointype,
      is_virtual          TYPE abap_bool,
      parameters          TYPE zsat_table_parameter_t,
      field_conditions    TYPE ty_t_join_field_condition,
      filter_conditions   TYPE ty_t_join_filter_condition,
      conditions          TYPE zsat_join_condition_data_t,
    END OF ty_s_join_table,

    ty_t_join_table TYPE STANDARD TABLE OF ty_s_join_table WITH EMPTY KEY.


  TYPES:
    BEGIN OF ty_s_join_def,
      primary_table             TYPE tabname,
      primary_table_alias       TYPE zsat_entity_alias,
      primary_table_alias_alv   TYPE zsat_entity_alias_alv,
      primary_table_entity_type TYPE zsat_entity_type,
      parameters                TYPE zsat_table_parameter_t,
      tables                    TYPE ty_t_join_table,
    END OF ty_s_join_def.
ENDINTERFACE.
