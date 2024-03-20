-module(client).

-compile(export_all).

-record(message, {unixtime = {}, nickname = {}, text = {}}).
