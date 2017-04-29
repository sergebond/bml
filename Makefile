PROJECT = bml
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

# deps
DEPS=exomler

# deps urls

dep_exomler=git git@github.com:erlangbureau/exomler.git

# Compiler options.
ERLC_OPTS ?= -W1


include erlang.mk