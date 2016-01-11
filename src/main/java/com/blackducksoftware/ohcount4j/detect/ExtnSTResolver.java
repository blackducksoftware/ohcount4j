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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnSTResolver extends AbstractExtnResolver {

    private static final Pattern ASSIGNMENT_PATTERN = Pattern.compile(":\\s*=");

    private static final Pattern BLOCKSTART_PATTERN = Pattern.compile(":\\s*\\[");

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (looksLikeSmalltalk(sourceFile.getCharSequence())) {
            return Language.SMALLTALK;
        } else {
            return null;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        // Note that this Resolver never actually returns BINARY.
        // We advertise more than one language to force the Detector to run this Resolver.
        // Otherwise, all *.st files would automatically be detected as SMALLTALK, which
        // is not true.
        if (language == Language.BINARY || language == Language.SMALLTALK) {
            return true;
        } else {
            return false;
        }
    }

    private boolean looksLikeSmalltalk(CharSequence contents) {
        if (ASSIGNMENT_PATTERN.matcher(contents).find() && BLOCKSTART_PATTERN.matcher(contents).find()) {
            return true;
        } else {
            return false;
        }
    }

}
