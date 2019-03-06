SWAGGER_CODEGEN_CLI_V = 2.3.1
SWAGGER_CODEGEN_CLI = swagger/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar
SWAGGER_CODEGEN = java -jar $(SWAGGER_CODEGEN_CLI)
SWAGGER_ENDPOINTS_SPEC = apps/aesophia_http/src/endpoints.erl

SWTEMP := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir')
HTTP_APP := apps/aesophia_http
MAKE := make

REBAR ?= rebar3

.PHONY: test docker clean distclean swagger-docs

all: swagger swagger-docs
	@($(REBAR) compile)

test: swagger
	@($(REBAR) eunit)
	@($(REBAR) ct)

swagger: $(SWAGGER_CODEGEN_CLI) $(HTTP_APP)/priv/swagger.json $(SWAGGER_ENDPOINTS_SPEC)

$(HTTP_APP)/priv/swagger.json: config/swagger.yaml
	@$(SWAGGER_CODEGEN) generate -i $< -l erlang-server -o $(SWTEMP)
	@echo "Swagger tempdir: $(SWTEMP)"
	@( mkdir -p $(HTTP_APP)/priv && cp $(SWTEMP)/priv/swagger.json $(HTTP_APP)/priv/; )
	@$(MAKE) updateswagger
	@rm -fr $(SWTEMP)

$(SWAGGER_CODEGEN_CLI):
	curl -fsS --create-dirs -o $@ http://central.maven.org/maven2/io/swagger/swagger-codegen-cli/$(SWAGGER_CODEGEN_CLI_V)/swagger-codegen-cli-$(SWAGGER_CODEGEN_CLI_V).jar

$(SWAGGER_ENDPOINTS_SPEC): config/swagger.yaml
	$(REBAR) swagger_endpoints

SWAGGER_UI_GIT = https://github.com/swagger-api/swagger-ui.git
SWAGGER_UI_GIT_DIR = $(HTTP_APP)/priv/swagger-ui
SWAGGER_DOCS_DIR = $(HTTP_APP)/priv/swagger-docs

swagger-docs: $(SWAGGER_DOCS_DIR)

$(SWAGGER_DOCS_DIR): |$(SWAGGER_UI_GIT_DIR)/.git
	@mkdir -p $(SWAGGER_DOCS_DIR)
	@cp -p $(SWAGGER_UI_GIT_DIR)/dist/* $(SWAGGER_DOCS_DIR)
	@cp -p $(HTTP_APP)/priv/swagger.json $(SWAGGER_DOCS_DIR)
	@sed -ibkp 's/https:\/\/petstore.swagger.io\/v2\/swagger.json/swagger.json/g' $(SWAGGER_DOCS_DIR)/index.html

updateswagger:
	@if [ -f $(SWAGGER_DOCS_DIR)/swagger.json ]; then \
	        cp -p $(HTTP_APP)/priv/swagger.json $(SWAGGER_DOCS_DIR); \
	fi

$(SWAGGER_UI_GIT_DIR)/.git:
	@git clone -n --depth 1 $(SWAGGER_UI_GIT) $(SWAGGER_UI_GIT_DIR)
	( cd $(SWAGGER_UI_GIT_DIR) && git checkout master dist ; )

docker:
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

