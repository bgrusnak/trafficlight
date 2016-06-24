-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
-record(trafficlight_sequence, {id, createdate}).
-record(trafficlight_broken, {id}).
-record(trafficlight_event, {id, sequence, createdate, numbers}).
-record(trafficlight_state, {id, sequence, color}).
-record(dolist, {sequence, variants=[], found=false, error=false, passed=0, started}).

