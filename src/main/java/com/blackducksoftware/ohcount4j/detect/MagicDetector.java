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

package com.blackducksoftware.ohcount4j.detect;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.OhcountException;

public class MagicDetector {

    // Use libmagic to identify the buffer contents.
    // Returns a Scanner class if the file type is recognized, otherwise null.
    public static Language detect(String buffer) {
        String description = getMagicDescription(buffer, MagicForType.BUFFER);
        String languageName = getLanguageName(description);

        return Detector.getInstance().detectByLanguageName(languageName);
    }

    private enum MagicForType {
        BUFFER, FILE_PATH;
    }

    public static Language detectFile(String path) {
        String description = getMagicDescription(path, MagicForType.FILE_PATH);
        String languageName = getLanguageName(description);

        return Detector.getInstance().detectByLanguageName(languageName);
    }

    protected static Pattern patterns[] = {
            Pattern.compile("^script text(?: executable)? for (\\w+)", 0),
            Pattern.compile("(\\w+)(?: -\\w+)* script(?:, \\w+)? text", 0),
            Pattern.compile("(\\w+) program text", 0),
    };

    public static String getLanguageName(String description) {
        if (description == null) {
            return null;
        }

        for (Pattern pattern : patterns) {
            Matcher matcher = pattern.matcher(description);
            if (matcher.find()) {
                return matcher.group(1);
            }
        }
        return null;
    }

    private static String getMagicDescription(String descriptionFor, MagicForType type) {
        if (descriptionFor == null) {
            return null;
        }
        Magic magic = getMagicLib();
        if (magic == null) {
            System.err.println("Can not get magic lib");
            return null;
        }
        try {
            String description;
            if (type == MagicForType.BUFFER) {
                description = magic.buffer(descriptionFor);
            } else {
                description = magic.file(descriptionFor);
            }
            if (magic.error() != null) {
                throw new OhcountException(magic.error());
            }
            return description;
        } finally {
            magic.close();
        }
    }

    /**
     * Opens, loads and returns Magic Lib
     *
     * @return
     */
    private static Magic getMagicLib() {
        Magic magic = new Magic();

        if (!magic.open()) {
            return null;
        }

        if (magic.error() != null) {
            throw new OhcountException(magic.error());
        }

        magic.load();

        if (magic.error() != null) {
            throw new OhcountException(magic.error());
        }

        return magic;
    }

}
