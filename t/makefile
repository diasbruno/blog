TESTS=create-delete-content

define render-test
.PHONY: $(1)
test-$(1):
	@bash test-$(1).bash
endef

$(foreach D,$(TESTS),$(eval $(call render-test,$(D))))

all: $(TESTS:%=test-%)
