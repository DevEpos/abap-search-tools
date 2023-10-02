INTERFACE zif_sat_c_cds_anno_definition
  PUBLIC.

  CONSTANTS environment_systemfield TYPE ddannotation_key VALUE 'Environment.systemField' ##NO_TEXT.
  CONSTANTS consumption_defaultvalue TYPE ddannotation_key VALUE 'Consumption.defaultValue' ##NO_TEXT.
  CONSTANTS semantics TYPE ddannotation_key VALUE 'Semantics' ##NO_TEXT.
  CONSTANTS semantics_amount_currencycode TYPE ddannotation_key VALUE 'Semantics.amount.currencyCode' ##NO_TEXT.
  CONSTANTS semantics_quan_unitofmeasure TYPE ddannotation_key VALUE 'Semantics.quantity.unitOfMeasure' ##NO_TEXT.
  CONSTANTS semantics_currencycode TYPE ddannotation_key VALUE 'Semantics.currencyCode' ##NO_TEXT.
ENDINTERFACE.
