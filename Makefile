PROJECT = dummy_ws
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS += cowboy
DEPS += lager

dep_cowboy = git https://github.com/ninenines/cowboy.git 2.9.0
dep_lager = git https://github.com/erlang-lager/lager.git 3.7.0

ERLC_OPTS += "+{parse_transform, lager_transform}"

include erlang.mk
