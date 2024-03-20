-module(server).

-compile(export_all).

-record(message, {unixtime = {}, nickname = {}, text = {}}).
