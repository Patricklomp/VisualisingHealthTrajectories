UPDATE @resultsSchema.@prefixE1E2_model
SET
  EVENT_PAIR_PVALUE_SIGNIFICANT='*'
WHERE
  EVENT_PAIR_PVALUE < @cutoff_pval
;

UPDATE @resultsSchema.@prefixE1E2_model
SET
  DIRECTIONAL_EVENT_PAIR_PVALUE_SIGNIFICANT='*'
WHERE
  DIRECTIONAL_EVENT_PAIR_PVALUE < @cutoff_pval_direction
;
