/*
 * Copyright 2021 Red Hat, Inc. and/or its affiliates
 * and other contributors as indicated by the @author tags.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.keycloak.quarkus.runtime.configuration;

import io.smallrye.config.SysPropConfigSource;
import io.smallrye.config.common.AbstractConfigSource;
import io.smallrye.config.common.utils.ConfigSourceUtil;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * The only reason for this config source is to keep the Keycloak specific properties when configuring the server so that
 * they are read again when running the server after the configuration.
 */
public class KcSysPropConfigSource extends AbstractConfigSource {

    private final Map<String, String> properties = new TreeMap<>();
    private static final int ORDINAL = 550;

    public KcSysPropConfigSource() {
        for (Map.Entry<Object, Object> entry : System.getProperties().entrySet()) {
            String key = (String) entry.getKey();
            if (key.startsWith(MicroProfileConfigProvider.NS_KEYCLOAK_PREFIX)) {
                properties.put(key, entry.getValue().toString());
            }
        }
    }

    @Override
    public Map<String, String> getProperties() {
        return properties;
    }

    @Override
    public Set<String> getPropertyNames() {
        return properties.keySet();
    }

    @Override
    public String getValue(final String propertyName) {
        return System.getProperty(propertyName);
    }

    @Override
    public String getName() {
        return "KcSysPropConfigSource";
    }

    @Override
    public int getOrdinal() {
        return ORDINAL;
    }
}
