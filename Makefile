REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = schemes/swag build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

COMPOSE_HTTP_TIMEOUT := 300
export COMPOSE_HTTP_TIMEOUT

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := wapi
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := 51bd5f25d00cbf75616e2d672601dfe7351dcaa4

BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := 61a001bbb48128895735a3ac35b0858484fdb2eb

CALL_ANYWHERE := \
	submodules \
	all compile xref lint dialyze test cover \
	start devrel release clean distclean \
	swag_server.generate swag_client.generate \
	generate regenerate swag_server.regenerate swag_client.regenerate \
	check_format format

CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER) all

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

generate: swag_server.generate swag_client.generate

regenerate: swag_server.regenerate swag_client.regenerate

compile: submodules generate
	$(REBAR) compile

xref:
	$(REBAR) xref

lint: generate
	elvis rock -V

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze:
	$(REBAR) as test dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: submodules generate
	$(REBAR) as prod release

clean:
	$(REBAR) cover -r
	$(REBAR) clean

distclean: swag_server.distclean swag_client.distclean
	rm -rf _build

cover:
	$(REBAR) cover

# CALL_W_CONTAINER
test: submodules generate
	$(REBAR) do eunit, ct

SWAGGER_CODEGEN = $(call which, swagger-codegen)
SWAGGER_SCHEME_BASE_PATH := schemes/swag
APP_PATH := apps
SWAGGER_SCHEME_API_PATH := $(SWAGGER_SCHEME_BASE_PATH)/api
SWAG_SPEC_FILE := swagger.yaml

# Swagger server

SWAG_SERVER_PREFIX := swag_server
SWAG_SERVER_APP_TARGET := $(APP_PATH)/$(SWAG_SERVER_PREFIX)
SWAG_SERVER_APP_PATH := $(APP_PATH)/$(SWAG_SERVER_PREFIX)

SWAG_SERVER_APP_TARGET_PAYRES  := $(SWAG_SERVER_APP_PATH)_payres/rebar.config
SWAG_SERVER_APP_TARGET_PRIVDOC := $(SWAG_SERVER_APP_PATH)_privdoc/rebar.config

$(SWAG_SERVER_APP_PATH)_%/rebar.config: $(SWAGGER_SCHEME_API_PATH)/$*
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME_API_PATH)/$*/$(SWAG_SPEC_FILE) \
		-l erlang-server \
		-o $(SWAG_SERVER_APP_PATH)_$* \
		--additional-properties \
			packageName=$(SWAG_SERVER_PREFIX)_$*

swag_server.generate: $(SWAG_SERVER_APP_TARGET_PAYRES) $(SWAG_SERVER_APP_TARGET_PRIVDOC)

swag_server.distclean: swag_server.distclean_payres swag_server.distclean_privdoc

swag_server.distclean_%:
	rm -rf $(SWAG_SERVER_APP_PATH)_$*

swag_server.regenerate: swag_server.distclean swag_server.generate

# Swagger client

SWAG_CLIENT_PREFIX := swag_client
SWAG_CLIENT_APP_TARGET := $(APP_PATH)/$(SWAG_CLIENT_PREFIX)
SWAG_CLIENT_APP_PATH := $(APP_PATH)/$(SWAG_CLIENT_PREFIX)

SWAG_CLIENT_APP_TARGET_PAYRES  := $(SWAG_CLIENT_APP_PATH)_payres/rebar.config
SWAG_CLIENT_APP_TARGET_PRIVDOC := $(SWAG_CLIENT_APP_PATH)_privdoc/rebar.config

$(SWAG_CLIENT_APP_PATH)_%/rebar.config: $(SWAGGER_SCHEME_API_PATH)/$*
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME_API_PATH)/$*/$(SWAG_SPEC_FILE) \
		-l erlang-client \
		-o $(SWAG_CLIENT_APP_PATH)_$* \
		--additional-properties \
			packageName=$(SWAG_CLIENT_PREFIX)_$*

swag_client.generate: $(SWAG_CLIENT_APP_TARGET_PAYRES) $(SWAG_CLIENT_APP_TARGET_PRIVDOC)

swag_client.distclean: swag_client.distclean_payres swag_client.distclean_privdoc

swag_client.distclean_%:
	rm -rf $(SWAG_CLIENT_APP_PATH)_$*

swag_client.regenerate: swag_client.distclean swag_client.generate
