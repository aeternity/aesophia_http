SWAGGER_ENDPOINTS_SPEC = apps/aesophia_http/src/endpoints.erl

HTTP_APP := apps/aesophia_http
MAKE := make

REBAR ?= ./rebar3

.PHONY: test docker clean distclean swagger-docs

all: swagger swagger-docs
	@($(REBAR) compile)

test: swagger
	@($(REBAR) eunit)
	@($(REBAR) ct)

swagger: $(HTTP_APP)/priv/oas3.json $(HTTP_APP)/priv/swagger.json $(SWAGGER_ENDPOINTS_SPEC)

$(HTTP_APP)/priv/oas3.json: config/oas3.yaml
	@mkdir -p $(HTTP_APP)/priv
	@yq config/oas3.yaml -o json > $(HTTP_APP)/priv/oas3.json

$(HTTP_APP)/priv/swagger.json: config/swagger.yaml
	@mkdir -p $(HTTP_APP)/priv
	@yq config/swagger.yaml -o json > $(HTTP_APP)/priv/swagger.json

$(SWAGGER_ENDPOINTS_SPEC): config/oas3.yaml
	$(REBAR) swagger_endpoints

SWAGGER_UI_GIT = https://github.com/swagger-api/swagger-ui.git
SWAGGER_UI_GIT_DIR = $(HTTP_APP)/priv/swagger-ui
SWAGGER_DOCS_DIR = $(HTTP_APP)/priv/swagger-docs

swagger-docs: $(SWAGGER_DOCS_DIR)

$(SWAGGER_DOCS_DIR): |$(SWAGGER_UI_GIT_DIR)/.git $(HTTP_APP)/priv/oas3.json
	@mkdir -p $(SWAGGER_DOCS_DIR)
	@cp -p $(SWAGGER_UI_GIT_DIR)/dist/* $(SWAGGER_DOCS_DIR)
	@cp -p $(HTTP_APP)/priv/oas3.json $(SWAGGER_DOCS_DIR)
	@sed -ibkp 's/https:\/\/petstore.swagger.io\/v2\/swagger.json/oas3.json/g' $(SWAGGER_DOCS_DIR)/swagger-initializer.js

$(SWAGGER_UI_GIT_DIR)/.git:
	@git clone -n --depth 1 $(SWAGGER_UI_GIT) $(SWAGGER_UI_GIT_DIR)
	( cd $(SWAGGER_UI_GIT_DIR) && git checkout master dist ; )

docker:
	@docker pull aeternity/builder
	@docker build -t aeternity/aesophia_http:local .

prod-build: swagger
	@($(REBAR) as prod release)

clean:
	rm -rf $(SWAGGER_DOCS_DIR)
	@$(REBAR) clean
	@-rm $(SWAGGER_ENDPOINTS_SPEC)

distclean:
	rm -rf $(SWAGGER_DOCS_DIR) $(SWAGGER_UI_GIT_DIR) $(SWAGGER_ENDPOINTS_SPEC)
	@$(REBAR) clean --all

