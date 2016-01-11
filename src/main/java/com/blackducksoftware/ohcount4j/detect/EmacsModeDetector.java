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

public class EmacsModeDetector {

    protected static Pattern pattern =
            Pattern.compile("-\\*-\\s*(mode:)?\\s*([^\\s;]+);?\\s*-\\*-",
                    Pattern.CASE_INSENSITIVE);

    // Given a string representing the initial portion of a file,
    // check for an Emacs mode header declaration and return its mode.
    public static String getMode(String s) {
        if (s == null) {
            return null;
        }

        Matcher matcher = pattern.matcher(s);
        if (matcher.find()) {
            return matcher.group(2);
        } else {
            return null;
        }
    }

    // Inspect the buffer for a possible Emacs mode header.
    // Returns a Scanner if a mode header is recognized, otherwise null.
    public static Language detect(String buffer) {
        String mode = getMode(buffer);
        return Detector.getInstance().detectByLanguageName(mode);
    }
}
