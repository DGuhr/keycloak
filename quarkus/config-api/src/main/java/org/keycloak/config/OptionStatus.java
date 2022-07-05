package org.keycloak.config;

public class OptionStatus {

    public OptionStatus(String reason, boolean isDeprecated, boolean isPreview, ConfigSupportLevel supportLevel) {
        this.reason = reason;
        this.isDeprecated = isDeprecated;
        this.isPreview = isPreview;
        this.supportLevel = supportLevel;
    }

    private String reason;

    private boolean isDeprecated;

    private boolean isPreview;

    private ConfigSupportLevel supportLevel;

    public boolean isDeprecated() {
        return isDeprecated;
    }

    public void setDeprecated(boolean deprecated) {
        isDeprecated = deprecated;
    }

    public boolean isPreview() {
        return isPreview;
    }

    public void setPreview(boolean preview) {
        isPreview = preview;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public ConfigSupportLevel getSupportLevel() {
        return supportLevel;
    }

    public void setStatus(ConfigSupportLevel status) {
        this.supportLevel = status;
    }
}
