package org.keycloak.config;

import java.time.LocalDate;
import java.util.Optional;

public class OptionStatusBuilder {
    private String reason;
    private boolean isDeprecated;
    private boolean isPreview;

    private ConfigSupportLevel level;

    public OptionStatusBuilder reason(String reason) {
        this.reason = reason;
        return this;
    }

    public OptionStatusBuilder isDeprecated(boolean isDeprecated) {
        this.isDeprecated = isDeprecated;
        return this;
    }

    public OptionStatusBuilder isPreview(boolean isPreview) {
        this.isPreview = isPreview;
        return this;
    }

    public OptionStatusBuilder status(ConfigSupportLevel level) {
        this.level = level;
        return this;
    }

    public OptionStatus build() {
        return new OptionStatus(reason, isDeprecated, isPreview, level);
    }
}