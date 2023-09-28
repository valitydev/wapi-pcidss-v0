-ifndef(wapi_bouncer_data_included__).
-define(wapi_bouncer_data_included__, ok).

-include_lib("bouncer_proto/include/bouncer_decision_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").

-define(TEST_USER_REALM, <<"external">>).
-define(TEST_RULESET_ID, <<"test/api">>).

-define(JUDGEMENT(Resolution), #decision_Judgement{resolution = Resolution}).
-define(ALLOWED, {allowed, #decision_ResolutionAllowed{}}).
-define(FORBIDDEN, {forbidden, #decision_ResolutionForbidden{}}).
-define(RESTRICTED(R), {restricted, #decision_ResolutionRestricted{restrictions = R}}).

-endif.
