/*
 * Copyright 2016 Black Duck Software, Inc.
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

package com.blackducksoftware.ohcount4j;

import com.blackducksoftware.ohcount4j.detect.Magic;

public class OhcountConfig {

    private static final String disableLibmagicProperty = "blackduck.ohcount4j.disable.libmagic";

    private static OhcountConfig INSTANCE = new OhcountConfig();

    private boolean disableLibmagic;

    private boolean libmagicExists;

    private OhcountConfig() {
        disableLibmagic = Boolean.valueOf(System.getProperty(disableLibmagicProperty, "true"));
        Magic magic = new Magic();
        libmagicExists = magic.open();

        if (libmagicExists) {
            // We just want to know if it's available
            magic.close();
        }
    }

    public static OhcountConfig getInstance() {
        return INSTANCE;
    }

    public boolean useLibmagic() {
        return !disableLibmagic && libmagicExists;
    }

}
