-ifndef(KZ_ATT_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(CONFIG_CAT, <<"attachments">>).

-type attachment_info() :: {ne_binary(), ne_binary(), ne_binary()}.

-define(KZ_ATT_HRL, 'true').
-endif.
