"! <p class="shorttext synchronized">Result Converter for Object Search</p>
INTERFACE zif_sat_adt_objs_res_converter
  PUBLIC.

  METHODS convert
    RETURNING
      VALUE(result) TYPE zif_sat_ty_adt_types=>ty_s_search_result.
ENDINTERFACE.
